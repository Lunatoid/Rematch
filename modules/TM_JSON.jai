
//
// Quick and dirty JSON parser.
// @NOTE: this JSON parser is NOT 100% spec-compliant.
// It is based on the Jai_Lexer module which parses more stuff than is allowed by
// the JSON spec (for example \U for 8-digit unicode literals). It should however
// be able to parse valid JSON correctly.
//

#module_parameters ();

#import "Basic";
#import "Hash_Table";
#import "Flat_Pool";
#import "Reflection";
#import "File";
#import "Jai_Lexer";
#import "String";

JSON_Type :: enum {
    NULL;
    STRING;
    NUMBER;
    OBJECT;
    ARRAY;
    BOOL;
}

// For simplicity, we don't use an union here.
JSON_Value :: struct {
    type := JSON_Type.NULL;

    value_string: string;
    value_number: float64;
    value_object: *JSON_Object;
    value_array: JSON_Array;
    value_bool: bool;

    allocator: Allocator;
}

JSON_Object :: Table(string, JSON_Value); // Hash map or key/pair array?
JSON_Array  :: [..] JSON_Value;

//
// Currently, json_from_file and json_from_string both allow
// line and block comments, since it's using Jai_Lexer which
// automatically parses comments.
//

// String to JSON

json_from_file :: (path: string) -> JSON_Value, bool {
    root, success := start_parse(path, from_file=true);
    return root, success;
}

json_from_string :: (source: string) -> JSON_Value, bool {
    root, success := start_parse(source, from_file=false);
    return root, success;
}

json_free :: (json: JSON_Value) {
    free(json.value_string,, json.allocator);

    if json.value_object {
        for * json.value_object.* {
            free(it_index,, json.allocator);
            json_free(it);
        }
        deinit(json.value_object);
        free(json.value_object,, json.allocator);
    }

    for * json.value_array {
        json_free(it);
    }
    array_free(json.value_array);
}

// Constructing

json_null :: () -> JSON_Value {
    return .{ type=.NULL, allocator=context.allocator };
}

// Currently copies the string using the context allocator.
json_string :: (str: string) -> JSON_Value {
    return .{ type=.STRING, value_string=copy_string(str,, context.allocator), allocator=context.allocator };
}

json_number :: (number: float64) -> JSON_Value {
    return .{ type=.NUMBER, value_number=number, allocator=context.allocator };
}

json_object :: () -> JSON_Value {
    json := JSON_Value.{ type=.OBJECT, allocator=context.allocator };
    json.value_object = New(JSON_Object);
    json.value_object.allocator = json.allocator;
    return json;
}

json_array :: () -> JSON_Value {
    return .{ type=.ARRAY, value_array.allocator=context.allocator, allocator=context.allocator };
}

json_bool :: (b: bool) -> JSON_Value {
    return .{ type=.BOOL, value_bool=b, allocator=context.allocator };
}

json_object_set :: (json: JSON_Value, key: string, value: JSON_Value) {
    table_set(json.value_object, copy_string(key,, json.allocator), value);
}

json_array_add :: (json: JSON_Value, value: JSON_Value) {
    array_add(*json.value_array, value);
}

// JSON to string
json_to_string :: (json: JSON_Value, $minified := true, indent := 0, indent_step := 2) -> string {
    print_value :: (builder: *String_Builder, json: JSON_Value, $minified: bool, indent: int, indent_step: int) {
        print_indent :: (indent: int) #expand {
            for 0..indent-1 {
                for 0..`indent_step-1 print(*builder, " ");
            }
        }

        if json.type == {
            case .NULL;
                print(builder, "null");

            case .STRING;
                print(builder, "\"%\"", json.value_string);

            case .NUMBER;
                print(builder, "%", json.value_number);

            case .BOOL;
                print(builder, "%", json.value_bool);

            case .ARRAY;
                print(builder, "[");
                #if !minified then print(builder, "\n");
                for * json.value_array {
                    #if !minified then print_indent(indent + 1);
                    print_value(builder, it, minified, indent + 1, indent_step);
                    if it_index + 1 < json.value_array.count {
                        print(builder, ",");
                    }
                    #if !minified then print(builder, "\n");
                }
                #if !minified then print_indent(indent);
                print(builder, "]");

            case .OBJECT;
                print(builder, "{");
                #if !minified then print(builder, "\n");
                index := 0;
                for * json.value_object.* {
                    defer index += 1;

                    #if !minified then print_indent(indent + 1);
                    print(builder, "\"%\":", it_index);
                    #if !minified then print(builder, " ");
                    print_value(builder, it, minified, indent + 1, indent_step);

                    if index + 1 < json.value_object.count {
                        print(builder, ",");
                    }
                    #if !minified then print(builder, "\n");
                }
                #if !minified then print_indent(indent);
                print(builder, "}");
        }
    }

    builder: String_Builder;
    print_value(*builder, json, minified, indent, indent_step);
    return builder_to_string(*builder);
}

json_print :: (json: JSON_Value, $minified := true, indent := 0, indent_step := 2) {
    print("%", json_to_string(json, minified, indent, indent_step,, temp));
}

json_serialize :: (any: Any) -> JSON_Value, bool {

    integer_as_s64 :: (ti: *Type_Info_Integer, ptr: *void) -> s64 {
        value: s64;

        if ti.signed {
            if ti.runtime_size == {
                case 1; value = << cast(*s8)  ptr;
                case 2; value = << cast(*s16) ptr;
                case 4; value = << cast(*s32) ptr;
                case 8; value = << cast(*s64) ptr;
            }
        } else {
            if ti.runtime_size == {
                case 1; value = << cast(*u8)  ptr;
                case 2; value = << cast(*u16) ptr;
                case 4; value = << cast(*u32) ptr;
                case 8; value = cast(s64) << cast(*u64) ptr;
            }
        }

        return value;
    }

    json: JSON_Value;

    if any.type.type == {
        case .INTEGER;
            ti := cast(*Type_Info_Integer) any.type;

            json = json_number(xx integer_as_s64(ti, any.value_pointer));

        case .FLOAT;
            value: float64;
            if any.type.runtime_size == {
                case 4; value = << cast(*float32) any.value_pointer;
                case 8; value = << cast(*float64) any.value_pointer;
            }

            json = json_number(value);

        case .BOOL;
            value := << cast(*bool) any.value_pointer;

            json = json_bool(value);

        case .STRING;
            str := << cast(*string) any.value_pointer;

            json = json_string(str);

        case .STRUCT;
            ti := cast(*Type_Info_Struct) any.type;

            json = json_object();

            for ti.members {
                if array_find(it.notes, "NoSerialize") then continue;

                field_name := it.name;
                for note: it.notes {
                    // Check for special serialization
                    // @SerializeAs(NEW_NAME)
                    // @TODO: right now does not check validity of key
                    SERIALIZE_NOTE_START :: "SerializeAs(";
                    if begins_with(note, SERIALIZE_NOTE_START) && ends_with(note, ")") {
                        field_name = slice(note, SERIALIZE_NOTE_START.count, note.count - SERIALIZE_NOTE_START.count - 1);
                        break;
                    }
                }

                field_any: Any;
                field_any.type = it.type;
                field_any.value_pointer = any.value_pointer + it.offset_in_bytes;

                field, field_success := json_serialize(field_any);

                if !field_success {
                    json_free(*field);
                    continue;
                }

                json_object_set(*json, field_name, field);
            }

        case .ARRAY;
            ti := cast(*Type_Info_Array) any.type;

            json = json_array();

            count, data := get_array_count_and_data(any.value_pointer, ti);

            for 0..count-1 {
                element_any: Any;
                element_any.type = ti.element_type;
                element_any.value_pointer = data + it * ti.element_type.runtime_size;

                elem, elem_success := json_serialize(element_any);

                if !elem_success {
                    json_free(*elem);
                    continue;
                }

                json_array_add(*json, elem);
            }

        case .ENUM;
            ti := cast(*Type_Info_Enum) any.type;

            value := integer_as_s64(ti.internal_type, any.value_pointer);
            for ti.values {
                if it == xx value {
                    json = json_string(ti.names[it_index]);
                    break;
                }
            }

        case .POINTER;
            json = json_null();

        case;
            return json_null(), false;
    }

    return json, true;
}

json_deserialize :: (any: Any, json: JSON_Value) -> bool {
    assert(any.value_pointer != null);

    if any.type.type == {
        case .INTEGER;
            if json.type != .NUMBER then return false;
            value := cast(s64) json.value_number;
            range_check_and_store(value, xx any.type, any.value_pointer);

        case .FLOAT;
            if json.type != .NUMBER then return false;

            if any.type.runtime_size == {
                case 4; << cast(*float32) any.value_pointer = xx json.value_number;
                case 8; << cast(*float64) any.value_pointer = json.value_number;
            }

        case .BOOL;
            if json.type != .BOOL then return false;
            << cast(*bool) any.value_pointer = json.value_bool;

        case .STRING;
            if json.type != .STRING then return false;
            << cast(*string) any.value_pointer = copy_string(json.value_string);

        case .STRUCT;
            if json.type != .OBJECT then return false;

            ti := cast(*Type_Info_Struct) any.type;

            for ti.members {
                if array_find(it.notes, "NoSerialize") then continue;

                field_name := it.name;

                for note: it.notes {
                    // @SerializeAs(NEW_NAME)
                    // @TODO: right now does not check validity of key
                    SERIALIZE_NOTE_START :: "SerializeAs(";
                    if begins_with(note, SERIALIZE_NOTE_START) && ends_with(note, ")") {
                        field_name = slice(note, SERIALIZE_NOTE_START.count, note.count - SERIALIZE_NOTE_START.count - 1);
                        break;
                    }
                }

                field := table_find_pointer(json.value_object, field_name);

                if !field then continue;

                field_any: Any;
                field_any.type = it.type;
                field_any.value_pointer = any.value_pointer + it.offset_in_bytes;

                if !json_deserialize(field_any, field) then continue;
            }

        case .ARRAY;
            if json.type != .ARRAY then return false;

            ti := cast(*Type_Info_Array) any.type;

            if ti.array_type == .RESIZABLE {
                array := cast(*Resizable_Array) any.value_pointer;
                array_reserve(array, json.value_array.count, ti.element_type.runtime_size);
                array.count = json.value_array.count;
            }

            count, data := get_array_count_and_data(any.value_pointer, ti);

            for json.value_array {
                element_any: Any;
                element_any.type = ti.element_type;
                element_any.value_pointer = data + it_index * ti.element_type.runtime_size;

                if !json_deserialize(element_any, it) then continue;
            }

        case .ENUM;
            ti := cast(*Type_Info_Enum) any.type;
            if json.type == {
                case .STRING;
                    // Enum via string
                    for ti.names {
                        if it == json.value_string {
                            range_check_and_store(ti.values[it_index], ti.internal_type, any.value_pointer);
                            break;
                        }
                    }


                case .NUMBER;
                    // Enum via number
                    for ti.values {
                        if it == xx json.value_number {
                            range_check_and_store(ti.values[it_index], ti.internal_type, any.value_pointer);
                            break;
                        }
                    }

                case;
                    return false;
            }

        case .POINTER;
            << cast(**void) any.value_pointer = null;

        case;
            return false;
    }

    return true;
}

#scope_file

Parsing_Context :: struct {
    lexer: Lexer;
}

start_parse :: (data: string, from_file: bool) -> JSON_Value, bool {
    c: Parsing_Context;

    // Non-standard addition to the Jai_Lexer module
    #if #exists(c.lexer.parse_escape_sequences) {
        c.lexer.parse_escape_sequences = false;
    }

    if from_file {
        set_input_from_file(*c.lexer, data);
    } else {
        set_input_from_string(*c.lexer, data);
    }

    defer set_input_from_string(*c.lexer, "");

    root: JSON_Value;
    success: bool;


    proxy_alloc :: (mode: Allocator_Mode, requested_size: s64, old_size: s64, old_memory: *void, allocator_data: *void) -> *void {
        if mode == .ALLOCATE || mode == .RESIZE {
            total_allocated += requested_size;
        }

        return flat_pool_allocator_proc(mode, requested_size, old_size, old_memory, allocator_data);
    }

    root, success = parse_value(*c);

    return root, success;
}
total_allocated := 0;

parse_value :: (c: *Parsing_Context) -> JSON_Value, bool {
    next := peek_next_token(*c.lexer,, temp);

    value: JSON_Value;
    value.allocator = context.allocator;
    success := false;

    expect :: (t: Token_Type) -> bool #expand {
        next := peek_next_token(*`c.lexer,, temp);
        if next.type == t {
            eat_token(*`c.lexer);
            return true;
        }
        return false;
    }

    number_is_negative := false;
    if next.type == {
        // Object
        case xx #char "{";
            eat_token(*c.lexer);
            success = true;

            value.type = .OBJECT;
            value.value_object = New(JSON_Object);
            value.value_object.allocator = context.allocator;

            while true {
                next = peek_next_token(*c.lexer,, temp);

                key: string;
                if next.type == .STRING then key = copy_string(next.string_value);
                if !expect(.STRING) {
                    report_parse_error(*c.lexer, "JSON: expected 'string', got '%' at %:%", next.type, next.l0, next.c0);
                    return value, false;
                }

                if !expect(xx #char ":") {
                    report_parse_error(*c.lexer, "JSON: expected ':', got '%' at %:%", next.type, next.l0, next.c0);
                    return value, false;
                }
                field, field_success := parse_value(c);

                if !field_success then return value, false;

                table_set(value.value_object, key, field);

                if expect(xx #char ",") then continue;
                if expect(xx #char "}") then break;

                next = peek_next_token(*c.lexer,, temp);
                if xx next.type < 128 {
                    char: string;
                    char.count = 1;
                    char.data = xx *next.type;
                    report_parse_error(*c.lexer, "JSON: unknown token while parsing object: '%' at %:%", char, next.l0, next.c0);
                } else {
                    report_parse_error(*c.lexer, "JSON: unknown token while parsing object: '%' at %:%", next.type, next.l0, next.c0);
                }
                success = false;
                break;
            }

        // Array
        case xx #char "[";
            eat_token(*c.lexer);
            success = true;

            value.type = .ARRAY;
            value.value_array.allocator = context.allocator;

            if !expect(xx #char "]") {
                while true {
                    elem, elem_success := parse_value(c);
                    if !elem_success then return value, false;

                    array_add(*value.value_array, elem);

                    if expect(xx #char ",") then continue;
                    if expect(xx #char "]") then break;

                    next = peek_next_token(*c.lexer,, temp);
                    if xx next.type < 128 {
                        char: string;
                        char.count = 1;
                        char.data = xx *next.type;
                        report_parse_error(*c.lexer, "JSON: unknown token while parsing array: '%' at %:%", char, next.l0, next.c0);
                    } else {
                        report_parse_error(*c.lexer, "JSON: unknown token while parsing array: '%' at %:%", next.type, next.l0, next.c0);
                    }
                    success = false;
                    break;
                }
            }

        // String
        case .STRING;
            #if #exists(c.lexer.parse_escape_sequences) {
                input := next.string_value;

                eat_token(*c.lexer);
                value.type = .STRING;
                value.value_string = parse_escape_sequences(input);
                success = true;
            } else {
                // Just accept string as-is
                eat_token(*c.lexer);
                value.type = .STRING;
                value.value_string = copy_string(next.string_value);
                success = true;
            }

        // Boolean
        case .KEYWORD_TRUE;
            eat_token(*c.lexer);
            success = true;
            value.type = .BOOL;
            value.value_bool = true;

        // Boolean
        case .KEYWORD_FALSE;
            eat_token(*c.lexer);
            success = true;
            value.type = .BOOL;
            value.value_bool = false;

        // Null
        case .KEYWORD_NULL;
            eat_token(*c.lexer);
            success = true;
            value.type = .NULL;

        // Number
        case xx #char "-";
            eat_token(*c.lexer);
            number_is_negative = true;
            next = peek_next_token(*c.lexer,, temp);
            if next.type != .NUMBER then return value, false;
            #through;

        // Number
        case .NUMBER;
            eat_token(*c.lexer);
            success = true;
            value.type = .NUMBER;

            if next.value_flags & .FLOAT {
                value.value_number = next.float64_value;
            } else {
                value.value_number = xx next.integer_value;
            }

            if number_is_negative then value.value_number *= -1.0;

        case;
            if xx next.type < 128 {
                char: string;
                char.count = 1;
                char.data = xx *next.type;
                report_parse_error(*c.lexer, "JSON: unknown token while parsing value: '%' at %:%", char, next.l0, next.c0);
            } else {
                report_parse_error(*c.lexer, "JSON: unknown token while parsing value: '%' at %:%", next.type, next.l0, next.c0);
            }
    }

    return value, success;
}

parse_escape_sequences :: (input: string) -> string {
    #import "Unicode";

    builder: String_Builder;
    builder.allocator = temp;

    it_index := 0;
    while it_index < input.count {
        peek_next_character :: () -> u8 #expand {
            return input[it_index];
        }

        eat_character :: () -> bool #expand {
            if it_index + 1 <= input.count {
                it_index += 1;
                return true;
            }
            return false;
        }

        get_hex_digit :: (type: Hex_Digit_Type) -> (value: u8, success: bool) #expand {
            c := peek_next_character();

            if      (c >= #char "a") && (c <= #char "f") { eat_character(); return cast(u8)(10 + c - #char "a"), true; }
            else if (c >= #char "A") && (c <= #char "F") { eat_character(); return cast(u8)(10 + c - #char "A"), true; }
            else if (c >= #char "0") && (c <= #char "9") { eat_character(); return cast(u8)(c - #char "0"), true; }

            return 0, false;
        }

        c := peek_next_character();
        if !eat_character() then break;

        if c == #char "\\" {
            n := peek_next_character();

            if n == #char "\"" {
                c = #char "\"";
                eat_character();
            } else if n == #char "\\" {
                c = #char "\\";
                eat_character();
            } else if n == #char "/" {
                c = #char "/";
                eat_character();
            } else if n == #char "b" {
                c = 0x08;
                eat_character();
            } else if n == #char "f" {
                c = 0x0C;
                eat_character();
            } else if n == #char "n" {
                c = #char "\n";
                eat_character();
            } else if n == #char "r" {
                c = #char "\r";
                eat_character();
            } else if n == #char "t" {
                c = #char "\t";
                eat_character();
            } else if (n == #char "u") {
                eat_character();

                num_digits: s32 = 4;
                type := Hex_Digit_Type.UNICODE_16;

                value: u32;
                for i: 0..num_digits-1 {
                    digit, success := get_hex_digit(type);
                    if !success break;

                    value <<= 4;
                    value += digit;
                }

                LEN_MAX :: 4;
                buf: [LEN_MAX] u8;

                s: string;
                s.data = buf.data;

                character_utf32_to_utf8(value, *s);
                append(*builder, s);

                continue;  // NOTE: Do not fall through to the bottom.
            } else {
                // @Cleanup reporinterp.report(REPORT_WARNING, lexer, null, "Unknown escape sequence '\\%c' in string constant!", n);
                c = n;
                eat_character();
            }
        }

        // NOTE: In the case of Unicode characters, we continue
        // rather than falling through to the bottom, so don't depend
        // on that!
        append(*builder, cast(u8) c);

    }

    return builder_to_string(*builder);
}

