
USE_MEMORY_DEBUGGER :: DEBUG;
USE_TRACY :: false;

#import "Basic"()(MEMORY_DEBUGGER=USE_MEMORY_DEBUGGER);
#import "Print_Color";
#import "TM_JSON";
#import "File_Utilities";
#import "Sort";
#import "String";
#import "File";

Rematch :: #import "Rematch";
#if USE_TRACY then Tracy :: #import "tracy"(.CLIENT);

Test_Input :: struct {
    text: string;
    matches: [..] [..] string;
}

Test_Case :: struct {
    pattern: string;
    inputs: [..] Test_Input;
}

// @TODO: add tests for replace, other flags, named captures, etc

main :: () {
    #if USE_MEMORY_DEBUGGER then defer report_memory_leaks();
    #if USE_TRACY then Tracy.FrameMark();
    #if USE_TRACY then defer Tracy.FrameMark();

    files := file_list("tests");
    defer array_free(files);

    files = quick_sort(files, (a: string, b: string) -> int {
        file_a := path_filename(a);
        file_b := path_filename(b);

        a_num := file_a;
        a_num.count = find_index_from_left(file_a, #char "-");
        b_num := file_b;
        b_num.count = find_index_from_left(file_b, #char "-");

        a_int := to_integer(a_num);
        b_int := to_integer(b_num);

        if a_int > b_int return 1;
        if a_int < b_int return -1;
        return 0;
    });

    log("Starting tests...");
    total_compile_time, total_match_time: Apollo_Time;

    log("[Test Cases]");
    for files {
        defer free(it);

        if path_extension(it) != "json" then continue;
        compile_time, match_time := perform_tests(it);
        total_compile_time += compile_time;
        total_match_time += match_time;
    }

    #if true {
        log("\n[Large Input]\n\n");
        {
            compile_time, match_time := large_input_test();
            total_compile_time += compile_time;
            total_match_time += match_time;

            print_color("All Pass!", color=.GREEN);
            log(" (compiling: % μs, matching: % μs)\n",
                to_microseconds(compile_time), to_microseconds(match_time));
        }
    }

    compile_time_us := to_microseconds(total_compile_time);
    match_time_us := to_microseconds(total_match_time);
    log("\nFinished in % μs! (compiling: % μs, matching: % μs)",
        compile_time_us + match_time_us, compile_time_us, match_time_us);
}

test_single :: (pattern: string, text: string, flags: Rematch.Flags = .GLOBAL) {
    r, success := Rematch.compile(pattern, flags);
    log("%", Rematch.to_string(*r,, temp));
    if success {
        matches, match_result := Rematch.match(*r, text);

        if match_result == .MATCH {
            log("[");
            for matches {
                log("  %", it.captures);
            }
            log("]");
            Rematch.free_matches(matches);
        } else {
            log_error("Regex result: %", match_result);
        }
    } else {
        log_error("Regex failed to compile");
    }
    Rematch.deinit(*r);
}

perform_tests :: (path: string) -> (compile_time: Apollo_Time, match_time: Apollo_Time) {
    defer reset_temporary_storage();

    #if USE_TRACY then Tracy.ZoneScoped(text=path);
    tests_json, success := json_from_file(path);
    defer json_free(tests_json);

    compile_time, match_time: Apollo_Time;

    if !success {
        log_error("Failed to open '%'", path);
        return .{}, .{};
    }

    log("\n--- % ---\n\n", path);

    test_cases: [..] Test_Case;
    json_deserialize(test_cases, tests_json,, temp);

    pass_count := 0;

    for test_case, test_index: test_cases {
        start_time := current_time_monotonic();
        regex, compile_success := Rematch.compile(test_case.pattern, .MULTILINE | .GLOBAL);
        end_time := current_time_monotonic();
        defer Rematch.deinit(*regex);

        compile_time += end_time - start_time;

        if !compile_success {
            log("=> %", test_case.pattern);
            print("  ! ");
            print_color("Failed to compile!\n", color=.RED);
            continue;
        }

        pattern := Rematch.to_string(*regex);
        defer free(pattern);

        pass := true;
        for input, input_index: test_case.inputs {
            start_time = current_time_monotonic();
            matches, match_result := Rematch.match(*regex, input.text);
            end_time = current_time_monotonic();

            match_time += end_time - start_time;

            defer Rematch.free_matches(matches);

            if match_result == .OUT_OF_MEMORY {
                log("=> %. %", test_index, pattern);
                log("  > %", input.text);
                print("  ! ");
                print_color("Out of memory!\n", color=.RED);
                pass = false;
            } else if match_result == .OUT_OF_TIME {
                log("=> %. %", test_index, pattern);
                log("  > %", input.text);
                print("  ! ");
                print_color("Out of time!\n", color=.RED);
                pass = false;
            }

            // Check matches
            min_matches := min(matches.count, input.matches.count);

            for match_index: 0..min_matches-1 {
                expected := input.matches[match_index];
                received := matches[match_index].captures;

                if expected.count != received.count {
                    log("=> %. %", test_index, pattern);
                    log("  ! Mismatch in match count, expected %, received %", expected.count, received.count);
                    pass = false;
                }

                capture_ok := true;
                min_captures := min(expected.count, received.count);
                for capture_index: 0..min_captures-1 {
                    if expected[capture_index] != received[capture_index] {
                        capture_ok = false;
                        break;
                    }
                }

                if !capture_ok {
                    log("=> %. %", test_index, pattern);
                    log("  > %", input.text);
                    log("  ! Received: %", received);
                    log("  ! Expected: %", expected);
                    pass = false;
                }

            }

            for min_matches..matches.count-1 {
                log("=> %. %", test_index, pattern);
                log("  > %", input.text);
                log("  ! Received: %", matches[it].captures);
                log("  ! Expected: <none>");
                pass = false;
            }

            for min_matches..input.matches.count-1 {
                log("=> %. %", test_index, pattern);
                log("  > %", input.text);
                log("  ! Received: <none>");
                log("  ! Expected: %", input.matches[it]);
                pass = false;
            }
        }

        if pass {
            pass_count += 1;
        }
    }

    if pass_count == test_cases.count {
        print("  : ");
        print_color("All Pass!", color=.GREEN);
        log(" (compiling: % μs, matching: % μs)",
            to_microseconds(compile_time), to_microseconds(match_time));
    }

    return compile_time, match_time;
}

large_input_test :: () -> Apollo_Time, Apollo_Time {
    #if USE_TRACY then Tracy.ZoneScoped();
    PATTERNS :: string.[
        "a",
        "[\\w\\.+-]+@[\\w\\.-]+\\.[\\w\\.-]+",
        "[\\w]+://[^/\\s?#]+[^\\s?#]+(?:\\?[^\\s#]*)?(?:#[^\\s]*)?",
        "(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9])\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9])"
    ];

    PATTERN_NAMES :: string.[
        "a",
        "e-mail",
        "url",
        "ip"
    ];

    compile_time, match_time, start_time, end_time: Apollo_Time;

    input_text := read_entire_file("tests/input-text.txt");
    defer free(input_text);

    for pattern: PATTERNS {
        start_time = current_time_monotonic();
        regex := Rematch.compile(pattern, .MULTILINE | .GLOBAL);
        defer Rematch.deinit(*regex);
        end_time = current_time_monotonic();

        current_compile_time := end_time - start_time;
        compile_time += current_compile_time;

        start_time = current_time_monotonic();
        matches, match_result := Rematch.match(*regex, input_text);
        defer Rematch.free_matches(matches);
        end_time = current_time_monotonic();

        current_match_time := end_time - start_time;
        match_time += current_match_time;

        print("  %:", PATTERN_NAMES[it_index]);
        for 1..8-PATTERN_NAMES[it_index].count {
            print(" ");
        }
        print_color("Pass!", color=.GREEN);
        log(" (compiling: % μs, matching: % μs)\n",
            to_microseconds(current_compile_time), to_microseconds(current_match_time));
    }

    log("\n\n");

    return compile_time, match_time;
}

