
// @TODO: currently the Regex compiler is too lax. It allows for patterns like "foo^" or similar
// things that is not actually allowed
compile :: (pattern: string, flags := Flags.NONE) -> Regex, bool {
    regex: Regex;
    regex.flags = flags;

    group_stack: [..] *Group;
    group_stack.allocator = temp;
    array_add(*group_stack, *regex.root);

    group_index := 0;
    regex.root.index = 0;

    if flags & .CASE_INSENSITIVE then regex.root.flags |= .CASE_INSENSITIVE;
    if flags & .MULTILINE then regex.root.flags |= .MULTILINE;
    if flags & .DOT_ALL then regex.root.flags |= .DOT_ALL;

    add_step :: (step: Step) -> *Step #expand {
        array_add(*peek(group_stack).steps, step);
        return peek_pointer(peek(group_stack).steps);
    }

    add_literal :: (char: int, try_collapse := true) #expand {
        added := false;
        if try_collapse && group_stack.count > 0 && peek(group_stack).steps.count > 0 {
            last_step := peek_pointer(peek(group_stack).steps);

            if last_step.symbol == .LITERAL &&
               last_step.quantifier.min_times == 1 && last_step.quantifier.max_times == 1 &&
               last_step.literal.count + 4 < last_step.literal.chars.count {
                added = true;

                str: string;
                str.data = last_step.literal.chars.data + last_step.literal.count;
                str.count = 0;

                character_utf32_to_utf8(xx char, *str);
                last_step.literal.count += str.count;
            }
        }

        if !added {
            step: Step;
            step.symbol = .LITERAL;

            str: string;
            str.data = step.literal.chars.data;
            str.count = 0;

            character_utf32_to_utf8(xx char, *str);
            step.literal.count += str.count;

            array_add(*peek(group_stack).steps, step);
        }
    }

    add_char_class :: (negated: bool, chars: ..Char_Class) #expand {
        step: Step;
        step.symbol = .CHAR_CLASS;

        step.char_class.negated = negated;
        for chars {
            if it.range_start <= 0xFF && it.range_end <= 0xFF {
                for c: it.range_start..it.range_end {
                    step.char_class.bitmask[c / 8] |= xx (1 << (c.(u8) % 8));
                }
            } else {
                array_add(*step.char_class.chars, it);
            }
        }

        insertion_sort(step.char_class.chars, (a: Char_Class, b: Char_Class) -> int {
            return a.range_start - b.range_start;
        });

        array_add(*peek(group_stack).steps, step);
    }

    add_backreference :: (index: int) #expand {
        step: Step;
        step.symbol = .BACKREFERENCE;
        step.backreference.by_index = true;
        step.backreference.index = index;

        array_add(*peek(group_stack).steps, step);
    }

    add_backreference :: (name: string) #expand {
        step: Step;
        step.symbol = .BACKREFERENCE;
        step.backreference.by_index = false;
        step.backreference.name = name;

        array_add(*peek(group_stack).steps, step);
    }

    add_assertion :: (type: Assertion_Type, negated: bool) -> *Group #expand {
        step: Step;
        step.symbol = .ASSERTION;
        step.assertion.type = type;
        step.assertion.negated = negated;

        array_add(*peek(group_stack).steps, step);
        last_step := peek_pointer(peek(group_stack).steps);
        return *last_step.assertion.group;
    }

    break_out_last_literal :: () #expand {
        last_step := peek_pointer(peek(group_stack).steps);
        if last_step.symbol == .LITERAL && last_step.literal.count > 1 {

            str: string;
            str.data = last_step.literal.chars.data;
            str.count = last_step.literal.count;

            end := str.data + str.count;
            cursor := end - 1;

            // Seek backwards to the first character that is <127
            while ((cursor.* & 0xC0) == 0x80) && (cursor > str.data) {
                cursor -= 1;
            }

            char, char_size := character_utf8_to_utf32(cursor, end - cursor);
            last_step.literal.count -= char_size;

            add_literal(char, false);
        }
    }

    p := *pattern;

    last_char := 0;
    while p.count > 0 {
        char, char_size := get_codepoint(p);
        defer last_char = char;

        if char == {
            case #char ".";
                add_step(.{ symbol=.WILDCARD });

            case #char "*";
                break_out_last_literal();
                last_step := peek_pointer(peek(group_stack).steps);
                last_step.quantifier.min_times = 0;
                last_step.quantifier.max_times = S64_MAX;

            case #char "+";
                break_out_last_literal();
                last_step := peek_pointer(peek(group_stack).steps);
                last_step.quantifier.min_times = 1;
                last_step.quantifier.max_times = S64_MAX;

            case #char "|";
                // For disjunction, we swap the latest group with the disjunction
                // itself, and put the latest group as the left side compare, and then pop
                // the right side compare
                group := peek(group_stack);
                pop(*group_stack);

                alt_group: Group;
                alt_group.is_non_capturing = group.is_non_capturing;
                alt_group.index = group.index;

                alt := array_add(*alt_group.steps);
                alt.symbol = .DISJUNCTION;
                alt.disjunction.left = group.*;
                alt.disjunction.left.is_non_capturing = true;

                alt.disjunction.right.index = alt.disjunction.left.index;
                alt.disjunction.right.is_non_capturing = true;
                if regex.flags & .FOLD_GROUPS then group_index = alt.disjunction.left.index;

                group.* = alt_group;

                array_add(*group_stack, *alt.disjunction.right);

            case #char "^";
                add_assertion(.ANCHOR_BEGIN, false);

            case #char "$";
                add_assertion(.ANCHOR_END, false);

            case #char "(";
                old_p := p.*;

                step := add_step(.{ symbol=.GROUP });
                group := *step.group;
                group.flags = peek(group_stack).flags;

                advance(p);
                if !p.count {
                    return regex, false;
                }
                if p.data[0] == #char "?" {
                    advance(p);
                    if !p.count {
                        return regex, false;
                    }

                    if p.data[0] == {
                        case #char ":";
                            // Non-capturing (?:)
                            group.is_non_capturing = true;

                        case #char "!";
                            // Negative lookahead (?!Foo)
                            step.assertion.negated = true;
                            #through;

                        case #char "=";
                            // Lookahead (?=Foo)
                            step.symbol = .ASSERTION;
                            step.assertion.type = .LOOKAHEAD;
                            group = *step.assertion.group;
                            group.is_non_capturing = true;

                        case #char "<";
                            advance(p);
                            if !p.count {
                                return regex, false;
                            }
                            if p.data[0] == {
                                case #char "!";
                                    // Negative lookbehind (?<!Foo)
                                    step.assertion.negated = true;
                                    #through;

                                case #char "=";
                                    // Lookbehind (?<=Foo)
                                    step.symbol = .ASSERTION;
                                    step.assertion.type = .LOOKBEHIND;
                                    group = *step.assertion.group;
                                    group.is_non_capturing = true;

                                case;
                                    // Named capture (?<Foo>)
                                    name := p.*;
                                    name.count = 0;
                                    while p.data[0] != #char ">" {
                                        advance(p);
                                        name.count += 1;
                                    }
                                    group.name = name;
                            }

                        case;
                            // Flags (?ims)
                            group.is_non_capturing = true;

                            flags_to_enable := p.*;
                            flags_to_enable.count = 0;

                            while p.count {
                                if p.data[0] == #char "-" || p.data[0] == #char ":" then break;
                                flags_to_enable.count += 1;
                                advance(p);
                            }

                            flags_to_disable: string;
                            if !p.count {
                                return regex, false;
                            }
                            if p.data[0] == #char "-" {
                                advance(p);

                                flags_to_disable = p.*;
                                flags_to_disable.count = 0;

                                while p.count && p.data[0] != #char ":" {
                                    flags_to_disable.count += 1;
                                    advance(p);
                                }
                            }

                            for flags_to_enable {
                                if it == {
                                    case #char "i";
                                        group.flags |= .CASE_INSENSITIVE;
                                    case #char "m";
                                        group.flags |= .MULTILINE;
                                    case #char "s";
                                        group.flags |= .DOT_ALL;
                                }
                            }

                            for flags_to_disable {
                                if it == {
                                    case #char "i";
                                        group.flags &= ~.CASE_INSENSITIVE;
                                    case #char "m";
                                        group.flags &= ~.MULTILINE;
                                    case #char "s";
                                        group.flags &= ~.DOT_ALL;
                                }
                            }
                    }
                } else {
                    p.* = old_p;
                }

                if !group.is_non_capturing {
                    group_index += 1;
                    group.index = group_index;
                }

                array_add(*group_stack, group);

            case #char ")";
                if group_stack.count <= 1 {
                    log_error("Regex: encountered ')' while no group is open");
                    return regex, false;
                }
                group := pop(*group_stack);

            case #char "{";
                advance(p);

                min_str := p.*;
                min_str.count = 0;
                if !p.count {
                    return regex, false;
                }
                if p.data[0] != #char "," {
                    while p.count > 0 {
                        advance(p);
                        if p.data[0] == #char "," || p.data[0] == #char "}" then break;
                    }
                    min_str.count = p.data - min_str.data;
                }

                max_str := p.*;
                max_str.data += 1;
                max_str.count = 0;
                if p.data[0] == #char "," {
                    advance(p);
                    while p.data[0] != #char "}" advance(p);
                    max_str.count = p.data - max_str.data;
                } else if p.data[0] == #char "}" {
                    max_str = min_str;
                }
                if !p.count {
                    return regex, false;
                }
                assert(p.data[0] == #char "}");

                min := to_integer(min_str);
                max := S64_MAX;
                if max_str.count > 0 then max = to_integer(max_str);

                break_out_last_literal();
                last_step := peek_pointer(peek(group_stack).steps);
                last_step.quantifier.min_times = min;
                last_step.quantifier.max_times = max;

                char = #char "}";

            case #char "[";
                advance(p);

                negated := false;
                if !p.count {
                    return regex, false;
                }
                if p.data[0] == #char "^" {
                    negated = true;
                    advance(p);
                }

                chars: [..] Char_Class;
                chars.allocator = temp;

                while p.count && p.data[0] != #char "]" {
                    start, start_size := get_codepoint(p);

                    if start == #char "\\" {
                        start=, success := parse_escape(p);
                        if !success {
                            advance(p);
                            if !p.count {
                                return regex, false;
                            }
                            if p.data[0] == {
                                case #char "W";
                                    array_add(*chars, ..NEGATED_WORD_CHAR_CLASS);

                                case #char "w";
                                    array_add(*chars, ..WORD_CHAR_CLASS);

                                case #char "S";
                                    array_add(*chars, ..NEGATED_WHITESPACE_CHAR_CLASS);

                                case #char "s";
                                    array_add(*chars, ..WHITESPACE_CHAR_CLASS);

                                case #char "D";
                                    array_add(*chars, ..NEGATED_DIGIT_CHAR_CLASS);

                                case #char "d";
                                    array_add(*chars, ..DIGIT_CHAR_CLASS);

                                case;
                                    return regex, false;
                            }
                            advance(p);
                        }
                    } else {
                        next_codepoint(p, start_size);
                    }


                    end := start;
                    if p.count > 1 && p.data[0] == #char "-" && p.data[1] != #char "]" {
                        advance(p);

                        end=, end_size := get_codepoint(p);
                        if end == #char "\\" {
                            end=, success := parse_escape(p);
                        } else {
                            next_codepoint(p, end_size);
                        }
                    }

                    array_add(*chars, .{ start, end });
                }

                add_char_class(negated, ..chars);
                if !p.count {
                    return regex, false;
                }
                assert(p.data[0] == #char "]");
                char = #char "]";

            case #char "?";
                if last_char == {
                    case #char "*"; #through;
                    case #char "+"; #through;
                    case #char "?"; #through;
                    case #char "}";
                        break_out_last_literal();
                        last_step := peek_pointer(peek(group_stack).steps);
                        last_step.quantifier.greedy = false;

                    case;
                        break_out_last_literal();
                        last_step := peek_pointer(peek(group_stack).steps);
                        last_step.quantifier.min_times = 0;
                        last_step.quantifier.max_times = 1;
                }

            case #char "\\";
                if p.count <= 1 then break;
                next := p.data[1];

                negated := false;
                if next == {
                    case #char "W";
                        negated = true;
                        #through;

                    case #char "w";
                        // [a-zA-Z0-9_]
                        add_char_class(negated, ..WORD_CHAR_CLASS);

                    case #char "D";
                        negated = true;
                        #through;

                    case #char "d";
                        // [0-9]
                        add_char_class(negated, ..DIGIT_CHAR_CLASS);

                    case #char "S";
                        negated = true;
                        #through;
                    case #char "s";
                        // [\f\n\r\t\v\u0020\u00a0\u1680\u2000-\u200a\u2028\u2029\u202f\u205f\u3000\ufeff]
                        add_char_class(negated, ..WHITESPACE_CHAR_CLASS);

                    case #char "B";
                        negated = true;
                        #through;
                    case #char "b";
                        add_assertion(.WORD_BOUNDARY, negated);
                    case #char "p"; #through;
                    case #char "P";
                        // Add unicode character class escape
                        // Do we want to even support this?
                    case #char "k";
                        // Named backreference
                        advance(p);
                        advance(p);
                        if !p.count {
                            return regex, false;
                        }
                        if p.data[0] != #char "<" then return regex, false;
                        advance(p);
                        name := p.*;
                        name.count = 0;

                        while p.count && p.data[0] != #char ">" {
                            advance(p);
                            name.count += 1;
                        }
                        if !p.count {
                            return regex, false;
                        }
                        advance(p);
                        add_backreference(name);

                        // Roll back so we're aligned (there are 2 consumes below)
                        p.data -= 2;
                        p.count += 2;

                    case;
                        escape, parsed_escape := parse_escape(p);

                        if parsed_escape {
                            add_literal(escape);


                            // Roll back so we're aligned (there are 2 consumes below)
                            p.data -= 2;
                            p.count += 2;
                        } else {
                            if is_digit(next) {
                                // Backreference
                                advance(p);
                                digit_str := p.*;

                                while p.count > 0 {
                                    if !is_digit(p.data[0]) then break;
                                    advance(p);
                                }

                                digit_str.count = p.data - digit_str.data;
                                digit := to_integer(digit_str);

                                // Roll back so we're aligned (there are 2 consumes below)
                                p.data -= 2;
                                p.count += 2;

                                add_backreference(digit);
                            } else {
                                add_literal(next);
                            }
                        }
                }
                next_codepoint(p, char_size);

            case;
                add_literal(char);
        }

        next_codepoint(p, char_size);
    }

    regex.max_capture_index = group_index;
    return regex, true;
}

#scope_file

parse_escape :: (p: *string) -> int, bool {
    assert(p.data[0] == #char "\\", "Expected '\\', received '%'", string.{ 1, p.data });

    old_p := p.*;
    advance(p);

    if p.data[0] == {
        case #char "n";
            advance(p);
            return #char "\n", true;

        case #char "t";
            advance(p);
            return #char "\t", true;

        case #char "r";
            advance(p);
            return #char "\r", true;

        case #char "v";
            advance(p);
            return 11, true; // \v (vertical tab)

        case #char "f";
            advance(p);
            return 12, true; // \f (formfeed)

        case #char "0";
            advance(p);
            return 0, true; // \0 (null byte)

        case #char "b";
            advance(p);
            return 8, true; // \b (backspace)

        case #char "c";
            // \cJ
            advance(p);
            char := p.data[0] % 32;
            advance(p);
            return char, true;

        case #char "x";
            // \xFF
            if p.count < 3 {
                return 0, false;
            }
            advance(p);
            hex_str := p.*;
            hex_str.count = 2;
            advance(p, 2);
            result, success := string_to_int(hex_str, base=16);
            return result, success;
        case #char "u";
            if p.count < 3 {
                return 0, false;
            }
            advance(p);
            hex_str := p.*;
            hex_str.count = 0;
            if p.data[0] == #char "{" {
                // \u{H} - \u{HHHHHH}
                hex_str.data += 1;
                advance(p);

                while p.data[0] != #char "}" {
                    if p.count == 0 then return 0, false;
                    if hex_str.count == 6 then break;

                    hex_str.count += 1;
                    advance(p);
                }

                if p.data[0] != #char "}" then return 0, false;

                advance(p);
            } else {
                // \uHHHH
                if p.count < 4 {
                    return 0, false;
                }
                advance(p, 4);
                hex_str.count = 4;
            }

            result, success := string_to_int(hex_str, base=16);
            return result, success;
    }

    p.* = old_p;
    return 0, false;
}

MAX_CHAR :: S32_MAX;

WORD_CHAR_CLASS :: Char_Class.[
    .{ #char "a", #char "z" },
    .{ #char "A", #char "Z" },
    .{ #char "0", #char "9" },
    .{ #char "_", #char "_" },
];

NEGATED_WORD_CHAR_CLASS :: Char_Class.[
    .{ 0, 47 },
    .{ 58, 64 },
    .{ 91, 94 },
    .{ 96, 96 },
    .{ 123, MAX_CHAR },
];

DIGIT_CHAR_CLASS :: Char_Class.[
    .{ #char "0", #char "9" }
];

NEGATED_DIGIT_CHAR_CLASS :: Char_Class.[
    .{ 0, 47 },
    .{ 58, MAX_CHAR },
];

WHITESPACE_CHAR_CLASS :: Char_Class.[
    .{ #char " ", #char " " },
    .{ #char "\n", #char "\n" },
    .{ #char "\r", #char "\r" },
    .{ #char "\t", #char "\t" },
    .{ 13, 13 }, // \v
    .{ 14, 14 }, // \f
    .{ #char "\u00a0", #char "\u00a0" },
    .{ #char "\u1680", #char "\u1680" },
    .{ #char "\u2000", #char "\u2000" },
    .{ #char "\u2001", #char "\u2001" },
    .{ #char "\u2002", #char "\u2002" },
    .{ #char "\u2003", #char "\u2003" },
    .{ #char "\u2004", #char "\u2004" },
    .{ #char "\u2005", #char "\u2005" },
    .{ #char "\u2006", #char "\u2006" },
    .{ #char "\u2007", #char "\u2007" },
    .{ #char "\u2008", #char "\u2008" },
    .{ #char "\u2009", #char "\u2009" },
    .{ #char "\u200a", #char "\u200a" },
    .{ #char "\u2028", #char "\u2028" },
    .{ #char "\u2029", #char "\u2029" },
    .{ #char "\u202f", #char "\u202f" },
    .{ #char "\u205f", #char "\u205f" },
    .{ #char "\u3000", #char "\u3000" },
    .{ #char "\ufeff", #char "\ufeff" },
];

NEGATED_WHITESPACE_CHAR_CLASS :: Char_Class.[
    .{ 0, 8 },
    .{ 11, 12 },
    .{ 15, 31 },
    .{ 33, 159 },
    .{ 161, 5759 },
    .{ 8203, 8231 },
    .{ 8234, 8238 },
    .{ 8240, 8286 },
    .{ 8288, 12287 },
    .{ 12289, 65278 },
    .{ 65280, MAX_CHAR },
];

