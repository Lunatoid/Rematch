
USE_MEMORY_DEBUGGER :: DEBUG;

#import "Basic"()(MEMORY_DEBUGGER=USE_MEMORY_DEBUGGER);
#import "Print_Color";
#import "TM_JSON";
#import "File_Utilities";
#import "Sort";
#import "String";

Rematch :: #import "Rematch";

Test_Input :: struct {
    text: string;
    matches: [..] [..] string;
}

Test_Case :: struct {
    pattern: string;
    inputs: [..] Test_Input;
}

main :: () {
    #if USE_MEMORY_DEBUGGER then defer report_memory_leaks();

    files := file_list("tests",, temp);
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

    for files {
        perform_tests(it);
    }
    log("\n");
}

test_single :: (pattern: string, text: string, flags: Rematch.Regex_Flags = .GLOBAL) {
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

perform_tests :: (path: string) {
    tests_json, success := json_from_file(path,, temp);
    defer json_free(tests_json);

    if !success {
        log_error("Failed to open '%'", path);
        return;
    }

    log("\n--- % ---\n\n", path);

    test_cases: [..] Test_Case;
    defer array_free(test_cases);

    json_deserialize(test_cases, tests_json,, temp);

    pass_count := 0;

    for test_case, test_index: test_cases {
        regex, compile_success := Rematch.compile(test_case.pattern, .MULTILINE | .GLOBAL);
        defer Rematch.deinit(*regex);

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
            matches, match_result := Rematch.match(*regex, input.text);
            defer Rematch.free_matches(matches);

            if match_result == .OUT_OF_MEMORY {
                log("=> %", pattern);
                log("  > %", input.text);
                print("  ! ");
                print_color("Out of memory!\n", color=.RED);
                pass = false;
            } else if match_result == .OUT_OF_TIME {
                log("=> %", pattern);
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
                    log("=> %", pattern);
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
                    log("=> %", pattern);
                    log("  > %", input.text);
                    log("  ! Received: %", received);
                    log("  ! Expected: %", expected);
                    pass = false;
                }

            }

            for min_matches..matches.count-1 {
                log("=> %", pattern);
                log("  > %", input.text);
                log("  ! Received: %", matches[it].captures);
                log("  ! Expected: <none>");
                pass = false;
            }

            for min_matches..input.matches.count-1 {
                log("=> %", pattern);
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
        print_color("All Pass!\n", color=.GREEN);
    }
}

