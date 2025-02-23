
#import "String";

Match_Range :: struct {
    start: int;
    count: int;
}

Named_Match_Range :: struct {
    #as using range: Match_Range;
    name: string;
}

operator != :: (a: Match_Range, b: Match_Range) -> bool {
    return a.start == b.start && a.count == b.count;
}

Match_Ranges :: struct {
    captures: [..] Match_Range;
    named_captures: [..] Named_Match_Range;
}

Named_Match :: struct {
    name: string;
    text: string;
}

Match_Strings :: struct {
    captures: [..] string;
    named_captures: [..] Named_Match;
}

Match_Options :: struct {
    memory_limit  := 0;
    time_limit_ms := 0;
}

range_to_string :: inline (range: Match_Range, text: string) -> string {
    str := text;
    str.data += range.start;
    str.count = range.count;
    return str;
}

ranges_to_strings :: inline (ranges: [] Match_Range, text: string) -> [..] string {
    str_matches: [..] string;
    for ranges {
        array_add(*str_matches, range_to_string(it, text));
    }
    return str_matches;
}

match_ranges_to_strings :: (matches: [..] Match_Ranges, text: string) -> [..] Match_Strings {
    string_matches: [..] Match_Strings;

    for match: matches {
        m: Match_Strings;
        m.captures = ranges_to_strings(match.captures, text);
        for match.named_captures {
            str := range_to_string(it.range, text);
            array_add(*m.named_captures, .{ name=it.name, text=str });
        }
        array_add(*string_matches, m);
    }

    return string_matches;
}

match :: inline (regex: *Regex, text: string, options := Match_Options.{}) -> [..] Match_Strings, Match_Result {
    state: State;
    state.flags = regex.flags;
    state.max_capture_index = regex.max_capture_index;
    state.options = options;
    state.buffer.data = text.data;
    state.buffer.count = text.count;
    state.end_of_input = true;

    matches, match_result := match_internal(*state, regex);

    string_matches := match_ranges_to_strings(matches, text);
    free_matches(matches);

    return string_matches, match_result;
}

match :: inline (regex: *Regex, stream: Text_Stream, options := Match_Options.{}) -> [..] Match_Ranges, Match_Result {
    state: State;
    state.flags = regex.flags;
    state.max_capture_index = regex.max_capture_index;
    state.options = options;
    state.text_stream = stream;

    array_reserve(*state.buffer, 32);
    fetch(*state, 1);

    matches, match_result := match_internal(*state, regex);

    array_free(state.buffer);

    return matches, match_result;
}

free_matches :: (matches: [..] Match_Ranges) {
    for match: matches {
        array_free(match.captures);
        array_free(match.named_captures);
    }
    array_free(matches);
}

free_matches :: (matches: [..] Match_Strings) {
    for match: matches {
        array_free(match.captures);
        array_free(match.named_captures);
    }
    array_free(matches);
}

deinit :: (regex: *Regex) {
    free_group(*regex.root);
}

Text_Stream_Callback :: #type (buffer: *[..] u8, requested: int, user_data: *void) -> bytes_written: int;
Text_Stream :: struct {
    callback: Text_Stream_Callback;
    user_data: *void;
}

#scope_file

match_internal :: (state: *State, regex: *Regex) -> [..] Match_Ranges, Match_Result {
    root_group: Step;
    root_group.symbol = .GROUP;
    root_group.group = regex.root;

    matches: [..] Match_Ranges;

    defer {
        array_free(state.captures);
        array_free(state.group_stack);
        array_free(state.bt_stack);
    }

    //
    // Right now our search strategy is very naive
    // We try to match at our current position, if
    // we fail we go to the next codepoint and try again.
    //

    state.tracking_allocator.allocator = context.allocator;
    state.allocator = .{tracking_allocator_proc, *state.tracking_allocator};

    if regex.flags & .STICKY then advance(state, regex.last_index);

    total_index := 0;
    while !(state.index + 1 > state.buffer.count && state.end_of_input) {
        old_index := state.index;

        if state.buffer[state.index] == #char "\n" && !(regex.flags & .MULTILINE) {
            break;
        }

        result := try_match(state, *regex.root);
        defer array_reset_keeping_memory(*state.captures);

        if result == .MATCH {
            // Successful match!
            m := array_add(*matches);

            max_index := regex.max_capture_index;

            if regex.flags & .FOLD_GROUPS {
                max_index = -1;
                for state.captures {
                    max_index = max(max_index, it.index);
                }
            }

            array_resize(*m.captures, max_index + 1);

            for state.captures {
                range := Match_Range.{ start=it.range.start+total_index, count=it.range.count };
                m.captures[it.index] = range;
                if it.name.count > 0 {
                    array_add(*m.named_captures, .{ name=it.name, range=range });
                }
            }
            regex.last_index = total_index + state.index;

            if old_index == state.index then state.index += 1;

            // We don't need the preceding text anymore
            if !state.end_of_input {
                memcpy(state.buffer.data, state.buffer.data + state.index, state.buffer.count - state.index);
                state.buffer.count -= state.index;
                total_index += state.index;
                state.index = 0;
                fetch(state, 1);
            }

            if !(regex.flags & .GLOBAL) then break;
        } else if result == .NO_MATCH {
            state.index = old_index;
            next_codepoint(state);
            if regex.flags & .STICKY then break;
        } else {
            break;
        }
    }

    regex.total_steps = state.total_steps;
    return matches, ifx matches.count > 0 then .MATCH else .NO_MATCH;
}

Backtrack_Ownership :: enum_flags {
    OWNS_GROUP_STACK :: 1 << 0;
    OWNS_CAPTURES    :: 1 << 1;
}

Backtrack_State :: struct {
    index: int;
    group_stack: [..] Group_State;
    captures: [..] Capture;
    ownership: Backtrack_Ownership = 0;

    // A single step can generate multiple backtracking state.
    // \w{2,8} will create backtracking state each time it matches between 2 and 8 times.
    // To avoid then having 6 separate states, we take the single state and increase the
    // repeat count. On rewind, once the repeat count drops to 0, we can pop this state.
    repeat_count := 1;

    // For greedy quantifiers we want to match latest-to-earliest, but for lazy quantifiers
    // we want to go earliest-to-latest. If this is >0, it will do backwards_count - repeat_count
    // so we can iterate the other way.
    backwards_count := 0;
}

Group_Type :: enum {
    GROUP;
    LOOKAROUND;
    NEGATED_LOOKAROUND;
}

Group_State :: struct {
    group_start: int; // Index when group was entered
    step_index: int;
    group: *Group;
    quantifier: Quantifier;

    group_type: Group_Type;
    current_quantifier: int;
}

State :: struct {
    total_steps := 0;
    flags: Flags;
    max_capture_index: int;
    options: Match_Options;

    index := 0;
    end_of_input := false;
    buffer: [..] u8;
    text_stream: Text_Stream;

    group_stack: [..] Group_State;
    captures: [..] Capture;
    bt_stack: [..] Backtrack_State;

    tracking_allocator: Tracking_Allocator;
    allocator: Allocator;
}

Capture :: struct {
    index: int;
    name: string;
    range: Match_Range;
}

free_group :: (group: *Group) {
    for * group.steps {
        free_step(it);
    }
    array_free(group.steps);
}

free_step :: (step: *Step) {
    if step.symbol == .CHAR_CLASS then array_free(step.char_class.chars);
    if step.symbol == .GROUP then free_group(*step.group);
    if step.symbol == .DISJUNCTION {
        free_group(*step.disjunction.left);
        free_group(*step.disjunction.right);
    }
    if step.symbol == .ASSERTION then free_group(*step.assertion.group);
}

Single_Match_Result :: enum {
    SUCCESS;
    NEW_GROUP; // New group added, need to immediately handle that first
    FAILURE;
}

try_match_single :: (regex: *State, step: *Step) -> Single_Match_Result {
    regex.total_steps += 1;

    state_index := regex.group_stack.count - 1;
    get_state :: () -> *Group_State #expand {
        return *regex.group_stack.data[state_index];
    }

    if step.symbol == {
        case .LITERAL;
            if !fetch(regex, step.literal.count) then return .FAILURE;

            p_str := buffer_as_string(regex);
            p_str.count = step.literal.count;

            l_str: string;
            l_str.data = step.literal.chars.data;
            l_str.count = step.literal.count;

            is_equal := false;
            if get_state().group.flags & .CASE_INSENSITIVE {
                is_equal = equal_nocase(l_str, p_str);
            } else {
                is_equal = equal(l_str, p_str);
            }

            advance(regex, step.literal.count * xx is_equal);

            return ifx is_equal then .SUCCESS else .FAILURE;

        case .WILDCARD;
            char, char_size := get_codepoint(regex);
            if char != #char "\n" || get_state().group.flags & .DOT_ALL {
                advance(regex, char_size);
                return .SUCCESS;
            }

        case .DISJUNCTION;
            // Normally if a group gets matched, the step index will get incremented
            // +1 before switching to the newest group on top of the stack.
            // This will happen for the left side automatically once we exit this function
            // but not for the right side, so we do it here manually.
            get_state().step_index += 1;
            defer get_state().step_index -= 1;

            add_group_to_stack(regex, *step.disjunction.right, step.quantifier, .GROUP);
            peek_pointer(regex.group_stack).step_index = -1; // -1 because rewind increments the step index
            create_backtrack_state(regex);

            regex.group_stack.count -= 1;
            add_group_to_stack(regex, *step.disjunction.left, step.quantifier, .GROUP);

            return .NEW_GROUP;

        case .CHAR_CLASS;
            char, char_size := get_codepoint(regex);
            result: enum {
                UNKNOWN;
                MATCH;
                NO_MATCH;
            };
            if char <= 0xFF {
                c := char.(u8);
                match := (step.char_class.bitmask[c / 8] & (1 << (c % 8))) != 0;
                result = ifx match != step.char_class.negated then .MATCH else .NO_MATCH;
            } else {
                low := 0;
                high := step.char_class.chars.count;

                while low < high {
                    mid := (low + high) / 2;
                    range := step.char_class.chars[mid];

                    if char < range.range_start {
                        high = mid;
                    } else if char > range.range_end {
                        low = mid + 1;
                    } else {
                        result = ifx !step.char_class.negated then .MATCH else .NO_MATCH;
                        break;
                    }
                }
            }

            if result == .UNKNOWN {
                result = ifx step.char_class.negated then .MATCH else .NO_MATCH;
            }

            advance(regex, char_size * xx (result == .MATCH));
            return ifx result == .MATCH then .SUCCESS else .FAILURE;

        case .GROUP;
            add_group_to_stack(regex, *step.group, step.quantifier, .GROUP);
            return .NEW_GROUP;

        case .BACKREFERENCE;
            capture: Capture;
            found := false;

            if step.backreference.by_index {
                if step.backreference.index <= 0 then return .FAILURE;
                for regex.captures {
                    if it.index == step.backreference.index {
                        capture = it;
                        found = true;
                        break;
                    }
                }
            } else {
                for regex.captures {
                    if it.name == step.backreference.name {
                        capture = it;
                        found = true;
                        break;
                    }
                }
            }

            if !found {
                if step.backreference.index <= regex.max_capture_index {
                    // Group index valid but not encountered yet.
                    // Return true with empty group in this case
                    return .SUCCESS;
                }
                return .FAILURE;
            }

            match: string;
            match.data = regex.buffer.data + capture.range.start;
            match.count = capture.range.count;

            if !fetch(regex, match.count) then return .FAILURE;

            p_str := buffer_as_string(regex);
            p_str.count = match.count;

            is_equal := false;
            if get_state().group.flags & .CASE_INSENSITIVE {
                is_equal = equal_nocase(match, p_str);
            } else {
                is_equal = equal(match, p_str);
            }

            advance(regex, match.count * xx is_equal);

            return ifx is_equal then .SUCCESS else .FAILURE;

        case .ASSERTION;
            if step.assertion.type == {
                case .WORD_BOUNDARY;
                    consumed_count := regex.index;

                    curr := regex.buffer[regex.index];
                    prev := cast(u8) #char " "; // Out-of-bounds is considered a non-word character

                    if consumed_count > 0 {
                        prev = regex.buffer[regex.index-1];
                    }

                    curr_alnum := is_alnum(curr);
                    prev_alnum := is_alnum(prev);

                    is_boundary := (cast(int) curr_alnum + xx prev_alnum) == 1;

                    return ifx !step.assertion.negated == is_boundary then .SUCCESS else .FAILURE;

                case .LOOKAHEAD;
                    group_type: Group_Type;
                    if step.assertion.negated {
                        group_type = .NEGATED_LOOKAROUND;
                    } else {
                        group_type = .LOOKAROUND;
                    }
                    add_group_to_stack(regex, *step.assertion.group, step.quantifier, group_type);
                    return .NEW_GROUP;

                case .LOOKBEHIND;
                    group_type: Group_Type;
                    if step.assertion.negated {
                        group_type = .NEGATED_LOOKAROUND;
                    } else {
                        group_type = .LOOKAROUND;
                    }

                    // Calculate the min and max amount of characters the lookahead could be.
                    min_steps, max_steps := count_group_steps(*step.assertion.group, regex.captures);
                    prev_string_length := regex.index;

                    if prev_string_length < min_steps {
                        return ifx step.assertion.negated then .SUCCESS else .FAILURE;
                    }

                    max_steps = min(prev_string_length, max_steps);

                    group_start := regex.index;
                    add_group_to_stack(regex, *step.assertion.group, step.quantifier, group_type);
                    regex.index -= max_steps;

                    // Now create backtracking states for the min-max range
                    if max_steps > min_steps {
                        create_backtrack_state(regex);
                        bt := peek_pointer(regex.bt_stack);
                        bt.group_stack[bt.group_stack.count - 1].step_index = -1;
                        bt.group_stack[bt.group_stack.count - 2].step_index += 1;
                        bt.repeat_count = max_steps - min_steps;
                    }

                    regex.index += max_steps - min_steps;

                    return .NEW_GROUP;

                case .ANCHOR_BEGIN;
                    consumed_count := regex.index;

                    if consumed_count <= 0 then return .SUCCESS;

                    if regex.flags | .MULTILINE {
                        return ifx regex.buffer[regex.index-1] == #char "\n" then .SUCCESS else .FAILURE;
                    }

                    return .FAILURE;

                case .ANCHOR_END;
                    if !fetch(regex, 1) then return .SUCCESS;

                    if regex.flags | .MULTILINE {
                        return ifx regex.buffer[regex.index] == #char "\n" then .SUCCESS else .FAILURE;
                    }

                    return .FAILURE;

                case;
                    return .FAILURE;
            }
    }

    return .FAILURE;
}

add_group_to_stack :: inline (regex: *State, group: *Group, quantifier: Quantifier, group_type: Group_Type) {
    state: Group_State;
    state.group_start = regex.index;
    state.group = group;
    state.group_type = group_type;
    state.quantifier = quantifier;
    state.step_index = 0;

    array_add(*regex.group_stack, state);
}

create_backtrack_state :: (regex: *State) {
    last_captures: [..] Capture;
    last_group_stack: [..] Group_State;

    for < regex.bt_stack {
        if last_captures.data == null && it.ownership & .OWNS_CAPTURES then last_captures = it.captures;
        if last_group_stack.data == null && it.ownership & .OWNS_GROUP_STACK then last_group_stack = it.group_stack;

        if last_captures.data != null && last_group_stack.data != null then break;
    }

    state: Backtrack_State;
    state.index = regex.index;

    if last_captures.data && last_captures.count >= regex.captures.count {
        if memcmp(last_captures.data, regex.captures.data, regex.captures.count * size_of(Capture)) == 0 {
            state.captures = last_captures;
            state.captures.count = regex.captures.count;
        }
    }

    if state.captures.count == 0 && regex.captures.count > 0 {
        state.ownership |= .OWNS_CAPTURES;
        state.captures.allocator = regex.captures.allocator;
        array_reserve(*state.captures, regex.captures.allocated);
        memcpy(state.captures.data, regex.captures.data, size_of(Capture) * regex.captures.count);
        state.captures.count = regex.captures.count;
    }

    if last_group_stack.data && last_group_stack.count >= regex.group_stack.count {
        if memcmp(last_group_stack.data, regex.group_stack.data, regex.group_stack.count * size_of(Group_State)) == 0 {
            state.group_stack = last_group_stack;
            state.group_stack.count = regex.group_stack.count;
        }
    }

    if state.group_stack.count == 0 && regex.group_stack.count > 0 {
        state.ownership |= .OWNS_GROUP_STACK;
        state.group_stack.allocator = regex.group_stack.allocator;
        array_reserve(*state.group_stack, regex.group_stack.allocated);
        memcpy(state.group_stack.data, regex.group_stack.data, size_of(Group_State) * regex.group_stack.count);
        state.group_stack.count = regex.group_stack.count;
    }

    array_add(*regex.bt_stack, state);
}

Match_Result :: enum {
    MATCH;
    NO_MATCH;
    OUT_OF_MEMORY;
    OUT_OF_TIME;
}

try_match :: (regex: *State, initial_group: *Group) -> Match_Result {
    regex.bt_stack.allocator = temp;
    regex.captures.allocator = regex.allocator;
    regex.group_stack.allocator = regex.allocator;

    add_group_to_stack(regex, initial_group, .{}, .GROUP);

    defer {
        for regex.bt_stack {
            if it.ownership & .OWNS_CAPTURES then array_free(it.captures);
            if it.ownership & .OWNS_GROUP_STACK then array_free(it.group_stack);
        }

        array_reset_keeping_memory(*regex.group_stack);
        array_reset_keeping_memory(*regex.bt_stack);
    }

    INVALID_STATE_INDEX :: -1;
    dummy_state: Group_State;

    end_time := current_time_monotonic() + milliseconds_to_apollo(regex.options.time_limit_ms);

    while regex.group_stack.count > 0 {
        if regex.options.memory_limit > 0 {
            MEMORY_FULL_MARGIN :: 64;
            current := regex.tracking_allocator.current_allocated;

            if current + MEMORY_FULL_MARGIN > regex.options.memory_limit {
                return .OUT_OF_MEMORY;
            }
        }

        if regex.options.time_limit_ms > 0 {
            curr_time := current_time_monotonic();
            if curr_time > end_time then return .OUT_OF_TIME;
        }

        // We used to cache the pointer directly, but if the group stack
        // grows and reallocates, the pointer will be invalid and we'll be in trouble!
        state_index := regex.group_stack.count - 1;
        get_state :: () -> *Group_State #expand {
            if state_index == INVALID_STATE_INDEX then return *dummy_state;

            return *regex.group_stack[state_index];
        }

        rewind :: () -> bool #expand {
            if get_state().group_type == .NEGATED_LOOKAROUND {
                regex.index = get_state().group_start;
                pop(*regex.group_stack);
                state_index = INVALID_STATE_INDEX;
                return true;
            }

            if regex.bt_stack.count > 0 {
                if get_state().group_type == .LOOKAROUND || get_state().group_type == .NEGATED_LOOKAROUND {
                    regex.index = get_state().group_start;
                }

                bt := peek_pointer(regex.bt_stack);
                regex.index = bt.index;

                advance_amount := bt.repeat_count - 1;
                if bt.backwards_count > 0 {
                    advance_amount = bt.backwards_count - bt.repeat_count;
                }

                for 1..advance_amount {
                    next_codepoint(regex);
                }

                array_reserve(*regex.captures, bt.captures.allocated);
                memcpy(regex.captures.data, bt.captures.data, size_of(Capture) * bt.captures.count);
                regex.captures.count = bt.captures.count;

                array_reserve(*regex.group_stack, bt.group_stack.allocated);
                memcpy(regex.group_stack.data, bt.group_stack.data, size_of(Group_State) * bt.group_stack.count);
                regex.group_stack.count = bt.group_stack.count;

                peek_pointer(regex.group_stack).step_index += 1;
                state_index = INVALID_STATE_INDEX;

                bt.repeat_count -= 1;
                if bt.repeat_count <= 0 {
                    if bt.ownership & .OWNS_CAPTURES then array_free(bt.captures);
                    if bt.ownership & .OWNS_GROUP_STACK then array_free(bt.group_stack);
                    pop(*regex.bt_stack);
                }

                return true;
            }
            return false;
        }

        if !fetch(regex, 1) || get_state().step_index < 0 ||
           get_state().step_index >= get_state().group.steps.count {
            matched_necessary := true;
            new_group := false;

            for get_state().step_index..get_state().group.steps.count-1 {
                step := *get_state().group.steps[it];

                if step.symbol == .ASSERTION {
                    if step.assertion.type == .ANCHOR_END {
                        // OK!
                        continue;
                    } else if step.assertion.type == .WORD_BOUNDARY {
                        // Out-of-bounds is considered a non-word character
                        curr_alnum := false;
                        prev_alnum := is_alnum(regex.buffer[regex.index-1]);

                        is_boundary := (cast(int) curr_alnum + xx prev_alnum) == 1;

                        if !step.assertion.negated == is_boundary {
                            continue;
                        }
                    } else if step.assertion.type == .LOOKBEHIND {
                        // Match trailing lookbehind
                        if try_match_single(regex, step) == .NEW_GROUP {
                            new_group = true;
                        }
                    }
                    matched_necessary = false;
                    break;
                } else if step.quantifier.min_times > 0 {
                    matched_necessary = false;
                    break;
                }
            }

            if new_group then continue;

            if matched_necessary && get_state().group_type == .NEGATED_LOOKAROUND {
                // Rewind twice. First one will simply pop the lookaround, second one will actually rewind.
                if !rewind() return .NO_MATCH;
                if !rewind() return .NO_MATCH;
                continue;
            }

            if matched_necessary {
                get_state().step_index = 0;
                get_state().current_quantifier += 1;
                old_group_start := get_state().group_start;
                get_state().group_start = regex.index;

                if !get_state().group.is_non_capturing {
                    range: Match_Range;
                    range.start = old_group_start;
                    range.count = regex.index - range.start;
                    add_or_update_capture(*regex.captures, get_state().group, range);
                }

                if get_state().current_quantifier >= get_state().quantifier.min_times &&
                   get_state().current_quantifier <= get_state().quantifier.max_times {

                    is_greedy_and_max_matched := get_state().quantifier.greedy  && get_state().current_quantifier == get_state().quantifier.max_times;
                    is_lazy_and_min_matched := !get_state().quantifier.greedy && get_state().current_quantifier >= get_state().quantifier.min_times;

                    pop_group := is_greedy_and_max_matched || is_lazy_and_min_matched;

                    if pop_group {
                        if get_state().group_type == .LOOKAROUND || get_state().group_type == .NEGATED_LOOKAROUND {
                            regex.index = old_group_start;
                        }

                        pop(*regex.group_stack);
                    } else {
                        // Since we already matched in a valid state, create a backtracking state where
                        // we already popped this group.
                        regex.group_stack.count -= 1;
                        create_backtrack_state(regex);
                        regex.group_stack.count += 1;

                        // Rewinding adds 1 to the step index while just popping it does not
                        // so we need to -1 to offset this.
                        bt_group := peek_pointer(peek_pointer(regex.bt_stack).group_stack);
                        bt_group.step_index = max(-1, bt_group.step_index - 1); // :LookBehindBandaid

                        // @TODO: find out why sometimes this is already at -1... It shouldn't happen,
                        // but can be bandaided by the max(-1, ...) above.

                        // Remove all captures we made for this group and child groups.
                        // This is necessary because cases like ((cat)|(dog))+ exist
                        // and otherwise in the input "catdog" capture group 2, "cat", stays matched.
                        remove_group_child_captures(*regex.captures, get_state().group);
                    }

                    continue;
                }
            } else {
                if !rewind() return .NO_MATCH;
            }

            continue;
        }

        step := *get_state().group.steps[get_state().step_index];

        match_times := 0;
        match_result := Single_Match_Result.FAILURE;
        bt_count_before := regex.bt_stack.count;

        can_create_backtrack := step.quantifier.min_times != step.quantifier.max_times;
        bt: *Backtrack_State;

        if can_create_backtrack && step.quantifier.min_times == 0 {
            // Create backtrack state for 0 matches
            create_backtrack_state(regex);
            bt = peek_pointer(regex.bt_stack);
        }

        for 1..step.quantifier.max_times {
            if !fetch(regex, 1) then break;

            match_result = try_match_single(regex, step);

            if match_result != .SUCCESS then break;

            match_times += 1;

            if can_create_backtrack && match_times >= step.quantifier.min_times {
                if !bt {
                    create_backtrack_state(regex);
                    bt = peek_pointer(regex.bt_stack);
                } else {
                    bt.repeat_count += 1;
                }
            }
        }

        // If we are lazy, reverse the backtracking state so it is from earliest to latest
        if !step.quantifier.greedy && bt {
            bt.backwards_count = bt.repeat_count;

            // Now rewind to the top of the stack (earliest valid matches)
            rewind();
            continue;
        }

        if match_result != .NEW_GROUP {
            if (match_times < step.quantifier.min_times || match_times > step.quantifier.max_times) {
                if !rewind() return .NO_MATCH;
                continue;
            }
        }

        get_state().step_index += 1;
    }

    return .MATCH;
}

add_or_update_capture :: (captures: *[..] Capture, group: *Group, range: Match_Range) {
    for * captures.* {
        if it.index == group.index {
            it.name = group.name;
            it.range = range;
            return;
        }
    }

    array_add(captures, .{ index=group.index, name=group.name, range=range });
}

remove_capture :: (captures: *[..] Capture, index: int) {
    for * captures.* {
        if it.index == index {
            remove it;
            break;
        }
    }
}

remove_group_child_captures :: (captures: *[..] Capture, group: *Group) {
    // @TODO: can probably be sped up by first getting all the unique indices
    // and then going over the array and removing them
    if !group.is_non_capturing then remove_capture(captures, group.index);
    for group.steps {
        if it.symbol == {
            case .GROUP;
                remove_group_child_captures(captures, *it.group);

            case .DISJUNCTION;
                remove_group_child_captures(captures, *it.disjunction.left);
                remove_group_child_captures(captures, *it.disjunction.right);
        }
    }
}

#scope_module

fetch :: inline (regex: *State, count: int) -> bool {
    necessary := count - (regex.buffer.count - regex.index);

    if necessary <= 0 || regex.end_of_input then return necessary <= 0;

    array_reserve(*regex.buffer, regex.buffer.count + necessary);
    read := regex.text_stream.callback(*regex.buffer, necessary, regex.text_stream.user_data);

    regex.end_of_input = necessary > read;
    return necessary <= read;
}

advance :: inline (regex: *State, count := 1) {
    fetch(regex, count);
    regex.index += count;
}

buffer_as_string :: inline (regex: *State) -> string {
    return string.{ regex.buffer.count - regex.index, regex.buffer.data + regex.index };
}

get_codepoint :: inline (regex: *State) -> int, int {
    fetch(regex, 4);
    str := buffer_as_string(regex);
    char, char_size := inline character_utf8_to_utf32(str.data, str.count);
    return char, char_size;
}

next_codepoint :: inline (regex: *State) {
    _, char_size := get_codepoint(regex);
    advance(regex, char_size);
}

// Actually counts codepoints
count_group_steps :: (group: *Group, captures: [] Capture) -> int, int {
    min_steps := 0;
    max_steps := 0;

    add_max :: (i: int) #expand {
        if i == S64_MAX {
            max_steps = i;
        } else {
            max_steps += i;
        }
    }

    for group.steps {
        if it.symbol == {
            case .LITERAL;
                min_bytes := it.quantifier.min_times * it.literal.count;
                max_bytes := it.quantifier.max_times * it.literal.count;
                min_steps += min_bytes;
                add_max(max_bytes);

            case .WILDCARD; #through;
            case .CHAR_CLASS;
                min_steps += it.quantifier.min_times;
                add_max(it.quantifier.max_times);

            case .GROUP;
                min_group, max_group := count_group_steps(*it.group, captures);
                min_steps += min_group;
                add_max(max_group);

            case .DISJUNCTION;
                min_left, max_left := count_group_steps(*it.disjunction.left, captures);
                min_right, max_right := count_group_steps(*it.disjunction.right, captures);

                min_steps += min(min_left, min_right);
                add_max(max(max_left, max_right));

            case .BACKREFERENCE;
                found := false;
                capture: Capture;

                if it.backreference.by_index {
                    for c: captures {
                        if c.index == it.backreference.index {
                            found = true;
                            capture = c;
                            break;
                        }
                    }
                } else {
                    for c: captures {
                        if c.name == it.backreference.name {
                            found = true;
                            capture = c;
                            break;
                        }
                    }
                }

                if found {
                    min_steps += capture.range.count;
                    max_steps += capture.range.count;
                }
        }
    }

    return min_steps, max_steps;
}

