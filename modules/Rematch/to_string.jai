
to_string :: (regex: *Regex) -> string {
    builder: String_Builder;

    for * regex.root.steps {
        step_to_string(*builder, it, regex.root.flags);
    }

    return builder_to_string(*builder);
}

#scope_file

step_to_string :: (builder: *String_Builder, step: *Step, flags: Group_Flags) {
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
                step_to_string(builder, it, step.disjunction.left.flags);
            }
            append(builder, #char "|");
            for * step.disjunction.right.steps {
                step_to_string(builder, it, step.disjunction.right.flags);
            }

        case .CHAR_CLASS;
            append(builder, #char "[");
            if step.char_class.negated then append(builder, "^");

            min_ascii := 255;
            max_ascii := 0;
            for 0..255 {
                if step.char_class.bitmask[it / 8] & (1 << (it % 8)) {
                    min_ascii = min(min_ascii, it);
                    max_ascii = max(max_ascii, it);
                }
            }

            if min_ascii != max_ascii {
                print_utf32(min_ascii);
                append(builder, "-");
                print_utf32(max_ascii);
            } else {
                print_utf32(max_ascii);
            }

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
                if flags != step.group.flags {
                    is_set :: (a: Group_Flags, b: Group_Flags, flag: Group_Flags) -> bool {
                        return (a & flag) && !(b & flag);
                    }


                    if step.group.flags != flags {
                        if is_set(step.group.flags, flags, .CASE_INSENSITIVE) then append(builder, "i");
                        if is_set(step.group.flags, flags, .MULTILINE) then append(builder, "m");
                        if is_set(step.group.flags, flags, .DOT_ALL) then append(builder, "s");

                        printed_hyphen := false;
                        if is_set(flags, step.group.flags, .CASE_INSENSITIVE) {
                            if !printed_hyphen {
                                printed_hyphen = true;
                                append(builder, "-");
                            }
                            append(builder, "i");
                        }
                        if is_set(flags, step.group.flags, .MULTILINE) {
                            if !printed_hyphen {
                                printed_hyphen = true;
                                append(builder, "-");
                            }
                            append(builder, "m");
                        }
                        if is_set(flags, step.group.flags, .DOT_ALL) {
                            if !printed_hyphen {
                                printed_hyphen = true;
                                append(builder, "-");
                            }
                            append(builder, "s");
                        }
                    }

                    append(builder, ":");
                }
            }

            if step.group.name.count > 0 {
                print(builder, "?<%>", step.group.name);
            }

            for * step.group.steps {
                step_to_string(builder, it, step.group.flags);
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
                        step_to_string(builder, it, step.assertion.group.flags);
                    }
                    append(builder, #char ")");

                case .LOOKBEHIND;
                    if !step.assertion.negated {
                        append(builder, "(?<=");
                    } else {
                        append(builder, "(?<!");
                    }

                    for * step.assertion.group.steps {
                        step_to_string(builder, it, step.assertion.group.flags);
                    }
                    append(builder, #char ")");


                case .ANCHOR_BEGIN;
                    append(builder, #char "^");
                case .ANCHOR_END;
                    append(builder, #char "$");

            }
    }

    min_times := step.quantifier.min_times;
    max_times := step.quantifier.max_times;
    if min_times == 1 && max_times == 1 {
        // Default
    } else if min_times == 0 && max_times == 1 {
        append(builder, #char "?");
    } else if min_times == 1 && max_times == S64_MAX {
        append(builder, #char "+");
    } else if min_times == 0 && max_times == S64_MAX {
        append(builder, #char "*");
    } else {
        print(builder, "{%", min_times);

        if min_times != max_times {
            append(builder, #char ",");
            if max_times < S64_MAX {
                print(builder, "%", max_times);
            }
        }
        append(builder, #char "}");
    }

    if !step.quantifier.greedy then append(builder, #char "?");
}

