
#import "Compiler";
#import "Basic";
#import "String";

RELEASE :: true;

#run build();

build :: () {
    set_build_options_dc(.{do_output=false});

    w := compiler_create_workspace("Rematch");

    options := get_build_options(w);

    options.output_executable_name = "rematch";
    options.output_type = .EXECUTABLE;
    options.output_path = "bin/";
    options.text_output_flags = .OUTPUT_TIMING_INFO;

    #if RELEASE {
        set_optimization(*options, .VERY_OPTIMIZED, preserve_debug_info=true);
        options.backend = .LLVM;
    } else {
        set_optimization(*options, .DEBUG, preserve_debug_info=true);
        options.backend = .X64;
    }

    options.additional_linker_arguments = string.[
        "/usr/lib/gcc/x86_64-pc-linux-gnu/14.2.1/libgcc_eh.a",
        "/usr/lib/gcc/x86_64-pc-linux-gnu/14.2.1/libgcc.a",
    ];

    set_build_options(options, w);

    compiler_begin_intercept(w);

    add_build_file("src/main.jai", w);
    add_build_string(tprint("DEBUG :: %;", !RELEASE), w);

    while message_loop := true {
        message := compiler_wait_for_message();

        if !message break;

        if message.kind == {
            case .COMPLETE;
                mc := cast(*Message_Complete) message;
                if mc.error_code != .NONE {
                    print("Compilation failed; exiting without compiling the main program.\n");
                    return;
                }
                break message_loop;

        }

    }

    compiler_end_intercept(w);
}

