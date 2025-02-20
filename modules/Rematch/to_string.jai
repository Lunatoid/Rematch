
to_string :: (regex: *Regex) -> string {
    builder: String_Builder;

    for * regex.root.steps {
        step_to_string(*builder, it);
    }

    return builder_to_string(*builder);
}

#scope_file

step_to_string :: (builder: *String_Builder, step: *Step) {
    print_utf32 :: (char: int) #expand {
        codepoint: u32;
        codepoint_str: string;
        codepoint_str.data = xx *codepoint;
        codepoint_str.count = size_of(type_of(codepoint));

        character_utf32_to_utf8(xx char, *codepoint_str);

        if codepoint == {
            case #char "\n";
                append(builder, "\\n");
            case #char "\t";
                append(builder, "\\t");
            case #char "\r";
                append(builder, "\\r");
            case 11; // \v
                append(builder, "\\v");
            case 12; // \f
                append(builder, "\\f");
            case 0; // \0
                append(builder, "\\0");
            case 8; // \b
                append(builder, "\\b");
            case #char "\u00a0"; #through;
            case #char "\u1680"; #through;
            case #char "\u2000"; #through;
            case #char "\u2001"; #through;
            case #char "\u2002"; #through;
            case #char "\u2003"; #through;
            case #char "\u2004"; #through;
            case #char "\u2005"; #through;
            case #char "\u2006"; #through;
            case #char "\u2007"; #through;
            case #char "\u2008"; #through;
            case #char "\u2009"; #through;
            case #char "\u200a"; #through;
            case #char "\u2028"; #through;
            case #char "\u2029"; #through;
            case #char "\u202f"; #through;
            case #char "\u205f"; #through;
            case #char "\u3000"; #through;
            case #char "\ufeff";
                append(builder, #char " ");
            case;
                append(builder, codepoint_str);
        }
    }

    if step.symbol == {
        case .LITERAL;
            str: string;
            str.data = step.literal.chars.data;
            str.count = step.literal.count;
            append(builder, str);

        case .WILDCARD;
            append(builder, #char ".");

        case .DISJUNCTION;
            for * step.disjunction.left.steps {
                step_to_string(builder, it);
            }
            append(builder, #char "|");
            for * step.disjunction.right.steps {
                step_to_string(builder, it);
            }

        case .CHAR_CLASS;
            append(builder, #char "[");
            if step.char_class.negated then append(builder, "^");

            for step.char_class.chars {
                print_utf32(it.range_start);
                if it.range_start != it.range_end {
                    append(builder, #char "-");
                    print_utf32(it.range_end);
                }
            }
            append(builder, #char "]");

        case .GROUP;
            append(builder, #char "(");

            if step.group.is_non_capturing {
                append(builder, "?");

                // @TODO: we currently don't detect cases like this:
                // (?i:(?-i:foo))
                // Because to the inner groups's POV, there are no flags.
                if step.group.flags & .CASE_INSENSITIVE then append(builder, "i");
                if step.group.flags & .MULTILINE then append(builder, "m");
                if step.group.flags & .DOT_ALL then append(builder, "s");

                append(builder, ":");
            }

            if step.group.name.count > 0 {
                print(builder, "?<%>", step.group.name);
            }

            for * step.group.steps {
                step_to_string(builder, it);
            }
            append(builder, #char ")");

        case .BACKREFERENCE;
            if step.backreference.by_index {
                print(builder, "\\%", step.backreference.index);
            } else {
                print(builder, "\\k<%>", step.backreference.name);
            }

        case .ASSERTION;
            if step.assertion.type == {
                case .WORD_BOUNDARY;
                    if step.assertion.negated {
                        append(builder, "\\B");
                    } else {
                        append(builder, "\\b");
                    }

                case .LOOKAHEAD;
                    if !step.assertion.negated {
                        append(builder, "(?=");
                    } else {
                        append(builder, "(?!");
                    }

                    for * step.assertion.group.steps {
                        step_to_string(builder, it);
                    }
                    append(builder, #char ")");

                case .LOOKBEHIND;
                    if !step.assertion.negated {
                        append(builder, "(?<=");
                    } else {
                        append(builder, "(?<!");
                    }

                    for * step.assertion.group.steps {
                        step_to_string(builder, it);
                    }
                    append(builder, #char ")");


                case .ANCHOR_BEGIN;
                    append(builder, #char "^");
                case .ANCHOR_END;
                    append(builder, #char "$");

            }
    }

    min := step.quantifier.min_times;
    max := step.quantifier.max_times;
    if min == 1 && max == 1 {
        // Default
    } else if min == 0 && max == 1 {
        append(builder, #char "?");
    } else if min == 1 && max == S64_MAX {
        append(builder, #char "+");
    } else if min == 0 && max == S64_MAX {
        append(builder, #char "*");
    } else {
        print(builder, "{%", min);

        if min != max {
            append(builder, #char ",");
            if max < S64_MAX {
                print(builder, "%", max);
            }
        }
        append(builder, #char "}");
    }

    if !step.quantifier.greedy then append(builder, #char "?");
}

