
Regex_Match :: struct {
    captures: [..] string;
    named_captures: Table(string, string);
}

// `memory_limit` must be at least 1 page size. It will count as OUT_OF_MEMORY if there is less than 1024 bytes available.
// If `time_limit_ms` is greater than 0 it will abort if matching the text takes longer than that.
Match_Options :: struct {
    DEFAULT_MEMORY_LIMIT :: 256 * 1024 * 1024;

    memory_limit  := DEFAULT_MEMORY_LIMIT;
    time_limit_ms := 0;
}

match :: (regex: *Regex, text: string, options := Match_Options.{}) -> [..] Regex_Match, Match_Result {
    root_group: Regex_Step;
    root_group.symbol = .GROUP;
    root_group.group = regex.root;

    matches: [..] Regex_Match;

    //
    // Right now our search strategy is very naive
    // We try to match at our current position, if
    // we fail we go to the next codepoint and try again.
    //
    state: Regex_State;
    state.p_base = text;
    state.flags = regex.flags;
    state.max_capture_index = regex.max_capture_index;
    state.max_groups = regex.max_groups;
    state.options = options;

    state.allocator = .{ proc=flat_pool_allocator_proc, data=*state.pool };
    init(*state.pool, reserve=state.options.memory_limit);
    defer fini(*state.pool);

    p_start := text;
    p := *text;

    if regex.flags & .STICKY then advance(p, regex.last_index);

    while p.count > 0 {
        defer reset(*state.pool);

        old_p := p.*;

        if p.data[0] == #char "\n" && !(regex.flags & .MULTILINE) {
            break;
        }

        result, captures := try_match(*state, *regex.root, p.*);

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

free_matches :: (matches: [..] Regex_Match) {
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

Backtrack_State :: struct {
    p: string;
    group_stack: [..] Group_State;
    captures: [..] Regex_Capture;
}

Group_Type :: enum {
    GROUP;
    LOOKAROUND;
    NEGATED_LOOKAROUND;
}

Group_State :: struct {
    p_group_start: string; // Pattern when group was entered
    step_index: int;
    group: *Regex_Group;
    quantifier: Regex_Quantifier;

    group_type: Group_Type;
    bt_count: int;
    current_quantifier := 0;
}

Regex_State :: struct {
    total_steps := 0;
    flags: Regex_Flags;
    p_base: string;
    max_capture_index: int;
    max_groups: int;
    options: Regex_Match_Options;

    pool: Flat_Pool;
    allocator: Allocator;
}

Regex_Capture :: struct {
    index: int;
    name: string;
    text: string;
}

free_group :: (group: *Regex_Group) {
    for * group.steps {
        free_step(it);
    }
    array_free(group.steps);
}

free_step :: (step: *Regex_Step) {
    array_free(step.char_class.chars);
    free_group(*step.group);
    free_group(*step.disjunction.left);
    free_group(*step.disjunction.right);
    free_group(*step.assertion.group);
}

Single_Match_Result :: enum {
    SUCCESS;
    NEW_GROUP; // New group added, need to immediately handle that first
    FAILURE;
}

try_match_single :: (regex: *Regex_State, step: *Regex_Step, p: *string, group_stack: *[..] Group_State, captures: *[..] Regex_Capture, bt_stack: *[..] Backtrack_State) -> Single_Match_Result {
    regex.total_steps += 1;

    state_index := group_stack.count - 1;
    get_state :: () -> *Group_State #expand {
        return *group_stack.data[state_index];
    }

    if step.symbol == {
        case .LITERAL;
            step_char := step.literal.char;
            char, char_size := get_codepoint(p);

            // @TODO: unicode tolower instead of ascii
            if get_state().group.flags & .CASE_INSENSITIVE && char < 255 && step_char < 255 {
                char = to_lower(xx char);
                step_char = to_lower(xx step_char);
            }

            if step_char == char {
                next_codepoint(p, char_size);
                return .SUCCESS;
            }

        case .WILDCARD;
            char, char_size := get_codepoint(p);
            if char != #char "\n" || get_state().group.flags & .DOT_ALL {
                next_codepoint(p, char_size);
                return .SUCCESS;
            }

        case .DISJUNCTION;
            group: Regex_Step;
            group.symbol = .GROUP;
            group.group = step.disjunction.left;

            old_p := p.*;

            // Normally if a group gets matched, the step index will get incremented
            // +1 before switching to the newest group on top of the stack.
            // This will happen for the left side automatically once we exit this function
            // but not for the right side, so we do it here manually.
            get_state().step_index += 1;
            defer get_state().step_index -= 1;

            add_group_to_stack(group_stack, *step.disjunction.right, p.*, step.quantifier, .GROUP, bt_stack.*);
            peek_pointer(group_stack.*).step_index = -1; // -1 because rewind increments the step index
            create_backtrack_state(bt_stack, group_stack.*, captures.*, p.*, regex.allocator);

            group_stack.count -= 1;
            add_group_to_stack(group_stack, *step.disjunction.left, p.*, step.quantifier, .GROUP, bt_stack.*);

            return .NEW_GROUP;

        case .CHAR_CLASS;
            any_match := false;
            for step.char_class.chars {
                char, char_size := get_codepoint(p);
                match := char >= it.range_start && char <= it.range_end;
                any_match |= match;

                if match && !step.char_class.negated {
                    next_codepoint(p, char_size);
                    return .SUCCESS;
                }
            }

            success := step.char_class.negated && !any_match;
            if success {
                next_codepoint(p);
                return .SUCCESS;
            }
            return .FAILURE;

        case .GROUP;
            add_group_to_stack(group_stack, *step.group, p.*, step.quantifier, .GROUP, bt_stack.*);
            return .NEW_GROUP;

        case .BACKREFERENCE;
            capture: Regex_Capture;
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

            while match.count > 0 && p.count > 0 {
                m_char, m_char_size := get_codepoint(match);
                p_char, p_char_size := get_codepoint(p);

                // @TODO: unicode tolower instead of ascii
                if get_state().group.flags & .CASE_INSENSITIVE && m_char < 255 && p_char < 255 {
                    m_char = to_lower(xx m_char);
                    p_char = to_lower(xx p_char);
                }

                if p_char != m_char then return .FAILURE;

                next_codepoint(match, m_char_size);
                next_codepoint(p, p_char_size);
            }

            return ifx match.count == 0 then .SUCCESS else .FAILURE;

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
                    add_group_to_stack(group_stack, *step.assertion.group, p.*, step.quantifier, group_type, bt_stack.*);
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
                    p.data -= min_steps;
                    p.count += min_steps;

                    // @TODO: we can try to at least match the first step of the LOOKBEHIND group
                    // to decide whether to create backtrack state for it.
                    for min_steps..max_steps {
                        add_group_to_stack(group_stack, *step.assertion.group, p_group_start, step.quantifier, group_type, bt_stack.*);

                        if it + 1 <= max_steps {
                            peek_pointer(group_stack.*).step_index = -1;

                            create_backtrack_state(bt_stack, group_stack.*, captures.*, p.*, regex.allocator);
                            bt := peek_pointer(bt_stack.*);
                            bt.group_stack[bt.group_stack.count - 2].step_index += 1;

                            group_stack.count -= 1;
                            p.data -= 1;
                            p.count += 1;
                        }
                    }

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

add_group_to_stack :: (group_stack: *[..] Group_State, group: *Regex_Group, p: string,
                       quantifier: Regex_Quantifier, group_type: Group_Type,
                       bt_stack: [] Backtrack_State) {
    state := array_add(group_stack);
    state.p_group_start = p;
    state.group = group;
    state.group_type = group_type;
    state.quantifier = quantifier;
    state.bt_count = bt_stack.count;

    state.step_index = 0;
}

create_backtrack_state :: (bt_stack: *[..] Backtrack_State, group_stack: [..] Group_State, matches: [..] Regex_Capture, p: string, allocator: Allocator) {
    state := array_add(bt_stack);
    state.p = p;

    state.captures.allocator = allocator;
    array_resize(*state.captures, matches.count, initialize=false);
    for matches state.captures[it_index] = it;

    state.group_stack.allocator = allocator;
    array_resize(*state.group_stack, group_stack.count, initialize=false);
    for group_stack state.group_stack[it_index] = it;
}

Match_Result :: enum {
    MATCH;
    NO_MATCH;
    OUT_OF_MEMORY;
    OUT_OF_TIME;
}

try_match :: (regex: *Regex_State, initial_group: *Regex_Group, text: string) -> Match_Result, [] Regex_Capture {
    bt_stack: [..] Backtrack_State;
    bt_stack.allocator = regex.allocator;

    captures: [..] Regex_Capture;
    captures.allocator = regex.allocator;

    group_stack: [..] Group_State;
    group_stack.allocator = regex.allocator;

    p := *text;

    array_reserve(*group_stack, regex.max_groups);

    add_group_to_stack(*group_stack, initial_group, text, .{}, .GROUP, bt_stack);

    add_or_update_capture :: (captures: *[..] Regex_Capture, group: *Regex_Group, match: string) {
        found := false;
        for * captures.* {
            if it.index == group.index {
                it.text = match;
                it.name = group.name;
                found = true;
                break;
            }
        }

        if !found {
            array_add(captures, .{ index=group.index, name=group.name, text=match });
        }
    }

    remove_capture :: (captures: *[..] Regex_Capture, index: int) {
        for * captures.* {
            if it.index == index {
                remove it;
                break;
            }
        }
    }

    INVALID_STATE_INDEX :: -1;
    dummy_state: Group_State;

    end_time := current_time_monotonic() + milliseconds_to_apollo(regex.options.time_limit_ms);

    while group_stack.count > 0 {
        {
            MEMORY_FULL_MARGIN :: 1024;
            current := regex.pool.current_point - regex.pool.memory_base;
            limit := regex.pool.address_limit - regex.pool.memory_base;

            if current + MEMORY_FULL_MARGIN > limit {
                return .OUT_OF_MEMORY, .[];
            }
        }

        if regex.options.time_limit_ms > 0 {
            curr_time := current_time_monotonic();
            if curr_time > end_time then return .OUT_OF_TIME, .[];
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

                bt := pop(*bt_stack);
                p.* = bt.p;
                group_stack = bt.group_stack;
                captures = bt.captures;
                peek_pointer(group_stack).step_index += 1;
                state_index = INVALID_STATE_INDEX;
                return true;
            }
            return false;
        }

        if get_state().step_index < 0 || get_state().step_index >= get_state().group.steps.count || p.count == 0 {
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
                // @HACK: rewind twice. First one will simply pop the lookaround, second one will actually rewind.
                if !rewind() return .NO_MATCH, .[];
                if !rewind() return .NO_MATCH, .[];
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
                        create_backtrack_state(*bt_stack, group_stack, captures, p.*, regex.allocator);

                        // Rewinding adds 1 to the step index while just popping it does not
                        // so we need to -1 to offset this.
                        peek_pointer(peek_pointer(bt_stack).group_stack).step_index -= 1;
                        group_stack.count += 1;

                        // Remove all captures we made for this group and child groups.
                        // This is necessary because cases like ((cat)|(dog))+ exist
                        // and otherwise in the input "catdog" capture group 2, "cat", stays matched.
                        remove_group_child_captures :: (captures: *[..] Regex_Capture, group: *Regex_Group) {
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
                        remove_group_child_captures(*captures, get_state().group);
                    }

                    continue;
                }
            } else {
                if !rewind() return .NO_MATCH, .[];
            }

            continue;
        }

        step := *get_state().group.steps[get_state().step_index];

        match_times := 0;
        match_result := Single_Match_Result.FAILURE;
        bt_count_before := bt_stack.count;

        can_create_backtrack := step.quantifier.min_times != step.quantifier.max_times;

        if can_create_backtrack && step.quantifier.min_times == 0 {
            // Create backtrack state for 0 matches
            create_backtrack_state(*bt_stack, group_stack, captures, p.*, regex.allocator);
        }

        for 1..step.quantifier.max_times {
            if p.count == 0 then break;

            match_result = try_match_single(regex, step, p, *group_stack, *captures, *bt_stack);

            if match_result != .SUCCESS then break;

            match_times += 1;

            if can_create_backtrack && match_times >= step.quantifier.min_times {
                // Save state since we successfully matched
                create_backtrack_state(*bt_stack, group_stack, captures, p.*, regex.allocator);
            }
        }

        // If we are lazy, reverse the backtracking state so it is from earliest to latest
        if !step.quantifier.greedy && bt_stack.count - bt_count_before > 1 {
            lazy_bts := array_view(bt_stack, bt_count_before);
            lazy_bts_copy := array_copy(lazy_bts,, temp);

            for < lazy_bts_copy {
                lazy_bts[lazy_bts.count - 1 - it_index] = it;
            }

            // Now rewind to the top of the stack (earliest valid matches)
            rewind();
            continue;
        }

        if match_result != .NEW_GROUP {
            if (match_times < step.quantifier.min_times || match_times > step.quantifier.max_times) {
                if !rewind() return .NO_MATCH, .[];
                continue;
            }
        }

        get_state().step_index += 1;
    }

    return .MATCH, captures;
}

count_group_steps :: (group: *Regex_Group, captures: [] Regex_Capture) -> int, int {
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
            case .WILDCARD; #through;
            case .LITERAL; #through;
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
                capture: Regex_Capture;

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

