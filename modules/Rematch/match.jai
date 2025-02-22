
#import "String";

Match :: struct {
    captures: [..] string;
    named_captures: Table(string, string);
}

Match_Options :: struct {
    memory_limit  := 0;
    time_limit_ms := 0;
}

match :: (regex: *Regex, text: string, options := Match_Options.{}) -> [..] Match, Match_Result {
    root_group: Step;
    root_group.symbol = .GROUP;
    root_group.group = regex.root;

    matches: [..] Match;

    //
    // Right now our search strategy is very naive
    // We try to match at our current position, if
    // we fail we go to the next codepoint and try again.
    //
    state: State;
    state.p_base = text;
    state.flags = regex.flags;
    state.max_capture_index = regex.max_capture_index;
    state.options = options;

    state.tracking_allocator.allocator = context.allocator;
    state.allocator = .{tracking_allocator_proc, *state.tracking_allocator};

    p_start := text;
    p := *text;

    if regex.flags & .STICKY then advance(p, regex.last_index);

    while p.count > 0 {
        defer state.tracking_allocator.high_water_mark = 0;
        old_p := p.*;

        if p.data[0] == #char "\n" && !(regex.flags & .MULTILINE) {
            break;
        }

        result, captures := try_match(*state, *regex.root, p.*);
        defer array_free(captures);

        if result == .MATCH {
            // Successful match!
            m := array_add(*matches);

            max_index := regex.max_capture_index;

            if regex.flags & .FOLD_GROUPS {
                max_index = -1;
                for captures {
                    max_index = max(max_index, it.index);
                }
            }

            array_resize(*m.captures, max_index + 1);

            for captures {
                m.captures[it.index] = it.text;
                if it.name.count > 0 {
                    table_add(*m.named_captures, it.name, it.text);
                }
            }

            advance(p, max(1, m.captures[0].count));
            regex.last_index = p.data - p_start.data;

            if !(regex.flags & .GLOBAL) then break;
        } else if result == .NO_MATCH {
            p.* = old_p;
            next_codepoint(p);
            if regex.flags & .STICKY then break;
        } else {
            array_reset(*matches);
            return matches, result;
        }
    }

    regex.total_steps = state.total_steps;
    return matches, ifx matches.count > 0 then .MATCH else .NO_MATCH;
}

free_matches :: (matches: [..] Match) {
    for match: matches {
        array_free(match.captures);
        deinit(*match.named_captures);
    }
    array_free(matches);
}

deinit :: (regex: *Regex) {
    free_group(*regex.root);
    free(regex.pattern);
}

#scope_file

Backtrack_Ownership :: enum_flags {
    OWNS_GROUP_STACK :: 1 << 0;
    OWNS_CAPTURES    :: 1 << 1;
}

Backtrack_State :: struct {
    p: string;
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
    p_group_start: string; // Pattern when group was entered
    step_index: int;
    group: *Group;
    quantifier: Quantifier;

    group_type: Group_Type;
    current_quantifier: int;
}

State :: struct {
    total_steps := 0;
    flags: Flags;
    p_base: string;
    max_capture_index: int;
    options: Match_Options;

    tracking_allocator: Tracking_Allocator;
    allocator: Allocator;
}

Capture :: struct {
    index: int;
    name: string;
    text: string;
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

try_match_single :: (regex: *State, step: *Step, p: *string, group_stack: *[..] Group_State, captures: *[..] Capture, bt_stack: *[..] Backtrack_State) -> Single_Match_Result {
    regex.total_steps += 1;

    state_index := group_stack.count - 1;
    get_state :: () -> *Group_State #expand {
        return *group_stack.data[state_index];
    }

    if step.symbol == {
        case .LITERAL;
            if p.count < step.literal.count then return .FAILURE;

            p_str := p.*;
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

            advance(p, step.literal.count * xx is_equal);

            return ifx is_equal then .SUCCESS else .FAILURE;

        case .WILDCARD;
            char, char_size := get_codepoint(p);
            if char != #char "\n" || get_state().group.flags & .DOT_ALL {
                next_codepoint(p, char_size);
                return .SUCCESS;
            }

        case .DISJUNCTION;
            // Normally if a group gets matched, the step index will get incremented
            // +1 before switching to the newest group on top of the stack.
            // This will happen for the left side automatically once we exit this function
            // but not for the right side, so we do it here manually.
            get_state().step_index += 1;
            defer get_state().step_index -= 1;

            add_group_to_stack(group_stack, *step.disjunction.right, p.*, step.quantifier, .GROUP);
            peek_pointer(group_stack.*).step_index = -1; // -1 because rewind increments the step index
            create_backtrack_state(bt_stack, group_stack.*, captures.*, p.*);

            group_stack.count -= 1;
            add_group_to_stack(group_stack, *step.disjunction.left, p.*, step.quantifier, .GROUP);

            return .NEW_GROUP;

        case .CHAR_CLASS;
            char, char_size := get_codepoint(p);
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

            advance(p, char_size * xx (result == .MATCH));
            return ifx result == .MATCH then .SUCCESS else .FAILURE;

        case .GROUP;
            add_group_to_stack(group_stack, *step.group, p.*, step.quantifier, .GROUP);
            return .NEW_GROUP;

        case .BACKREFERENCE;
            capture: Capture;
            found := false;

            if step.backreference.by_index {
                if step.backreference.index <= 0 then return .FAILURE;
                for captures.* {
                    if it.index == step.backreference.index {
                        capture = it;
                        found = true;
                        break;
                    }
                }
            } else {
                for captures.* {
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

            // We don't want to affect the actual match
            match := *capture.text;

            if capture.text.count > p.count then return .FAILURE;

            p_str := p.*;
            p_str.count = match.count;

            is_equal := false;
            if get_state().group.flags & .CASE_INSENSITIVE {
                is_equal = equal_nocase(capture.text, p_str);
            } else {
                is_equal = equal(capture.text, p_str);
            }

            advance(p, capture.text.count * xx is_equal);

            return ifx is_equal then .SUCCESS else .FAILURE;

        case .ASSERTION;
            if step.assertion.type == {
                case .WORD_BOUNDARY;
                    consumed_count := p.data - regex.p_base.data;

                    curr := p.data[0];
                    prev := cast(u8) #char " "; // Out-of-bounds is considered a non-word character

                    if consumed_count > 0 {
                        prev = p.data[-1];
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
                    add_group_to_stack(group_stack, *step.assertion.group, p.*, step.quantifier, group_type);
                    return .NEW_GROUP;

                case .LOOKBEHIND;
                    group_type: Group_Type;
                    if step.assertion.negated {
                        group_type = .NEGATED_LOOKAROUND;
                    } else {
                        group_type = .LOOKAROUND;
                    }

                    // Calculate the min and max amount of characters the lookahead could be.
                    min_steps, max_steps := count_group_steps(*step.assertion.group, captures.*);
                    prev_string_length := p.data - regex.p_base.data;

                    if prev_string_length < min_steps {
                        return ifx step.assertion.negated then .SUCCESS else .FAILURE;
                    }

                    max_steps = min(prev_string_length, max_steps);

                    p_group_start := p.*;

                    // Now create backtracking states for the min-max range
                    p.data -= max_steps;
                    p.count += max_steps;

                    add_group_to_stack(group_stack, *step.assertion.group, p_group_start, step.quantifier, group_type);

                    if max_steps > min_steps {
                        create_backtrack_state(bt_stack, group_stack.*, captures.*, p.*);
                        bt := peek_pointer(bt_stack.*);
                        bt.group_stack[bt.group_stack.count - 1].step_index = -1;
                        bt.group_stack[bt.group_stack.count - 2].step_index += 1;
                        bt.repeat_count = max_steps - min_steps;
                    }

                    p.data += max_steps - min_steps;
                    p.count -= max_steps - min_steps;

                    return .NEW_GROUP;

                case .ANCHOR_BEGIN;
                    consumed_count := p.data - regex.p_base.data;

                    if consumed_count <= 0 then return .SUCCESS;

                    if regex.flags | .MULTILINE {
                        return ifx p.data[-1] == #char "\n" then .SUCCESS else .FAILURE;
                    }

                    return .FAILURE;

                case .ANCHOR_END;
                    if p.count <= 0 then return .SUCCESS;

                    if regex.flags | .MULTILINE {
                        return ifx p.data[0] == #char "\n" then .SUCCESS else .FAILURE;
                    }

                    return .FAILURE;

                case;
                    return .FAILURE;
            }
    }

    return .FAILURE;
}

add_group_to_stack :: inline (group_stack: *[..] Group_State, group: *Group, p: string,
                       quantifier: Quantifier, group_type: Group_Type) {
    state: Group_State;
    state.p_group_start = p;
    state.group = group;
    state.group_type = group_type;
    state.quantifier = quantifier;
    state.step_index = 0;

    array_add(group_stack, state);
}

create_backtrack_state :: (bt_stack: *[..] Backtrack_State, group_stack: [..] Group_State, captures: [..] Capture, p: string) {
    last_captures: [..] Capture;
    last_group_stack: [..] Group_State;

    for < bt_stack.* {
        if last_captures.data == null && it.ownership & .OWNS_CAPTURES then last_captures = it.captures;
        if last_group_stack.data == null && it.ownership & .OWNS_GROUP_STACK then last_group_stack = it.group_stack;

        if last_captures.data != null && last_group_stack.data != null then break;
    }

    state: Backtrack_State;
    state.p = p;

    if last_captures.data && last_captures.count >= captures.count {
        if memcmp(last_captures.data, captures.data, captures.count * size_of(Capture)) == 0 {
            state.captures = last_captures;
            state.captures.count = captures.count;
        }
    }

    if state.captures.count == 0 && captures.count > 0 {
        state.ownership |= .OWNS_CAPTURES;
        state.captures.allocator = captures.allocator;
        array_reserve(*state.captures, captures.allocated);
        memcpy(state.captures.data, captures.data, size_of(Capture) * captures.count);
        state.captures.count = captures.count;
    }

    if last_group_stack.data && last_group_stack.count >= group_stack.count {
        if memcmp(last_group_stack.data, group_stack.data, group_stack.count * size_of(Group_State)) == 0 {
            state.group_stack = last_group_stack;
            state.group_stack.count = group_stack.count;
        }
    }

    if state.group_stack.count == 0 && group_stack.count > 0 {
        state.ownership |= .OWNS_GROUP_STACK;
        state.group_stack.allocator = group_stack.allocator;
        array_reserve(*state.group_stack, group_stack.allocated);
        memcpy(state.group_stack.data, group_stack.data, size_of(Group_State) * group_stack.count);
        state.group_stack.count = group_stack.count;
    }

    array_add(bt_stack, state);
}

Match_Result :: enum {
    MATCH;
    NO_MATCH;
    OUT_OF_MEMORY;
    OUT_OF_TIME;
}

try_match :: (regex: *State, initial_group: *Group, text: string) -> Match_Result, [..] Capture {
    bt_stack: [..] Backtrack_State;
    bt_stack.allocator = temp;

    captures: [..] Capture;
    captures.allocator = regex.allocator;

    group_stack: [..] Group_State;
    group_stack.allocator = regex.allocator;
    add_group_to_stack(*group_stack, initial_group, text, .{}, .GROUP);

    defer {
        for bt_stack {
            if it.ownership & .OWNS_CAPTURES then array_free(it.captures);
            if it.ownership & .OWNS_GROUP_STACK then array_free(it.group_stack);
        }

        array_free(group_stack);
        array_free(bt_stack);
    }

    p := *text;

    INVALID_STATE_INDEX :: -1;
    dummy_state: Group_State;

    end_time := current_time_monotonic() + milliseconds_to_apollo(regex.options.time_limit_ms);

    while group_stack.count > 0 {
        if regex.options.memory_limit > 0 {
            MEMORY_FULL_MARGIN :: 64;
            current := regex.tracking_allocator.current_allocated;

            if current + MEMORY_FULL_MARGIN > regex.options.memory_limit {
                return .OUT_OF_MEMORY, captures;
            }
        }

        if regex.options.time_limit_ms > 0 {
            curr_time := current_time_monotonic();
            if curr_time > end_time then return .OUT_OF_TIME, captures;
        }

        // We used to cache the pointer directly, but if the group stack
        // grows and reallocates, the pointer will be invalid and we'll be in trouble!
        state_index := group_stack.count - 1;
        get_state :: () -> *Group_State #expand {
            if state_index == INVALID_STATE_INDEX then return *dummy_state;

            return *group_stack[state_index];
        }

        rewind :: () -> bool #expand {
            if get_state().group_type == .NEGATED_LOOKAROUND {
                p.* = get_state().p_group_start;
                pop(*group_stack);
                state_index = INVALID_STATE_INDEX;
                return true;
            }

            if bt_stack.count > 0 {
                if get_state().group_type == .LOOKAROUND || get_state().group_type == .NEGATED_LOOKAROUND {
                    p.* = get_state().p_group_start;
                }

                bt := peek_pointer(bt_stack);
                p.* = bt.p;

                advance_amount := bt.repeat_count - 1;
                if bt.backwards_count > 0 {
                    advance_amount = bt.backwards_count - bt.repeat_count;
                }

                for 1..advance_amount {
                    next_codepoint(p);
                }

                array_reserve(*captures, bt.captures.allocated);
                memcpy(captures.data, bt.captures.data, size_of(Capture) * bt.captures.count);
                captures.count = bt.captures.count;

                array_reserve(*group_stack, bt.group_stack.allocated);
                memcpy(group_stack.data, bt.group_stack.data, size_of(Group_State) * bt.group_stack.count);
                group_stack.count = bt.group_stack.count;

                peek_pointer(group_stack).step_index += 1;
                state_index = INVALID_STATE_INDEX;

                bt.repeat_count -= 1;
                if bt.repeat_count <= 0 {
                    if bt.ownership & .OWNS_CAPTURES then array_free(bt.captures);
                    if bt.ownership & .OWNS_GROUP_STACK then array_free(bt.group_stack);
                    pop(*bt_stack);
                }

                return true;
            }
            return false;
        }

        if p.count == 0 || get_state().step_index < 0 || get_state().step_index >= get_state().group.steps.count {
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
                        prev_alnum := is_alnum(p.data[-1]);

                        is_boundary := (cast(int) curr_alnum + xx prev_alnum) == 1;

                        if !step.assertion.negated == is_boundary {
                            continue;
                        }
                    } else if step.assertion.type == .LOOKBEHIND {
                        // Match trailing lookbehind
                        if try_match_single(regex, step, p, *group_stack, *captures, *bt_stack) == .NEW_GROUP {
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
                if !rewind() return .NO_MATCH, captures;
                if !rewind() return .NO_MATCH, captures;
                continue;
            }

            if matched_necessary {
                get_state().step_index = 0;
                get_state().current_quantifier += 1;
                old_p_group_start := get_state().p_group_start;
                get_state().p_group_start = p.*;

                if !get_state().group.is_non_capturing {
                    match := old_p_group_start;
                    match.count = p.data - match.data;
                    add_or_update_capture(*captures, get_state().group, match);
                }

                if get_state().current_quantifier >= get_state().quantifier.min_times &&
                   get_state().current_quantifier <= get_state().quantifier.max_times {

                    is_greedy_and_max_matched := get_state().quantifier.greedy  && get_state().current_quantifier == get_state().quantifier.max_times;
                    is_lazy_and_min_matched := !get_state().quantifier.greedy && get_state().current_quantifier >= get_state().quantifier.min_times;

                    pop_group := is_greedy_and_max_matched || is_lazy_and_min_matched;

                    if pop_group {
                        if get_state().group_type == .LOOKAROUND || get_state().group_type == .NEGATED_LOOKAROUND {
                            p.* = old_p_group_start;
                        }

                        pop(*group_stack);
                    } else {
                        // Since we already matched in a valid state, create a backtracking state where
                        // we already popped this group.
                        group_stack.count -= 1;
                        create_backtrack_state(*bt_stack, group_stack, captures, p.*);
                        group_stack.count += 1;

                        // Rewinding adds 1 to the step index while just popping it does not
                        // so we need to -1 to offset this.
                        bt_group := peek_pointer(peek_pointer(bt_stack).group_stack);
                        bt_group.step_index = max(-1, bt_group.step_index - 1); // :LookBehindBandaid

                        // @TODO: find out why sometimes this is already at -1... It shouldn't happen,
                        // but can be bandaided by the max(-1, ...) above.

                        // Remove all captures we made for this group and child groups.
                        // This is necessary because cases like ((cat)|(dog))+ exist
                        // and otherwise in the input "catdog" capture group 2, "cat", stays matched.
                        remove_group_child_captures(*captures, get_state().group);
                    }

                    continue;
                }
            } else {
                if !rewind() return .NO_MATCH, captures;
            }

            continue;
        }

        step := *get_state().group.steps[get_state().step_index];

        match_times := 0;
        match_result := Single_Match_Result.FAILURE;
        bt_count_before := bt_stack.count;

        can_create_backtrack := step.quantifier.min_times != step.quantifier.max_times;
        bt: *Backtrack_State;

        if can_create_backtrack && step.quantifier.min_times == 0 {
            // Create backtrack state for 0 matches
            create_backtrack_state(*bt_stack, group_stack, captures, p.*);
            bt = peek_pointer(bt_stack);
        }

        for 1..step.quantifier.max_times {
            if p.count == 0 then break;

            match_result = try_match_single(regex, step, p, *group_stack, *captures, *bt_stack);

            if match_result != .SUCCESS then break;

            match_times += 1;

            if can_create_backtrack && match_times >= step.quantifier.min_times {
                if !bt {
                    create_backtrack_state(*bt_stack, group_stack, captures, p.*);
                    bt = peek_pointer(bt_stack);
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
                if !rewind() return .NO_MATCH, captures;
                continue;
            }
        }

        get_state().step_index += 1;
    }

    return .MATCH, captures;
}

add_or_update_capture :: (captures: *[..] Capture, group: *Group, match: string) {
    for * captures.* {
        if it.index == group.index {
            it.text = match;
            it.name = group.name;
            return;
        }
    }

    array_add(captures, .{ index=group.index, name=group.name, text=match });
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
                    min_steps += capture.text.count;
                    max_steps += capture.text.count;
                }
        }
    }

    return min_steps, max_steps;
}

