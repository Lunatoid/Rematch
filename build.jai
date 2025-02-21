
#import "Compiler";
#import "Basic";
#import "String";

RELEASE :: true;

ENABLE_PLUGINS :: true;

PLUGIN_NAMES :: string.[
    "Check"
];

#run build();

#if ENABLE_PLUGINS {
    #import "Metaprogram_Plugins";
}

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

    #if ENABLE_PLUGINS {
        plugins: [..] *Metaprogram_Plugin;
        init_plugins(PLUGIN_NAMES, *plugins, w);
    }

    set_build_options(options, w);

    intercept_flags: Intercept_Flags;
    #if ENABLE_PLUGINS {
        for plugins if it.before_intercept then it.before_intercept(it, *intercept_flags);
    }

    compiler_begin_intercept(w, intercept_flags);

    #if ENABLE_PLUGINS {
        for plugins if it.add_source then it.add_source(it);
    }

    add_build_file("src/main.jai", w);
    add_build_string(tprint("DEBUG :: %;", !RELEASE), w);

    while message_loop := true {
        message := compiler_wait_for_message();

        #if ENABLE_PLUGINS {
            for plugins if it.message it.message(it, message);
        }

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

    #if ENABLE_PLUGINS {
        for plugins if it.finish   then it.finish(it);
        for plugins if it.shutdown then it.shutdown(it);
    }
}

