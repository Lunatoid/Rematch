/*

Features:

  abc                 matches a character literal
  .                   matches any character except newline (unless .DOT_ALL is specified)
  [abc], [a-c]        matches either a|b|c
  [^abc], [^a-c]      matches anything that is NOT a|b|c
  (abc)               matches the group (abc)
  (?:abc)             matches the group (abc), does not capture
  (?<Name>abc)        matches the group (abc), names the capture Name (<> is required)
  (?im-s:abc)         modifies flags for the capture group,
  (?=Foo)             lookahead, asserts that the following text follows the pattern
  (?!Foo)             negative lookahead, asserts that the following text does not follow the pattern
  (?<=Foo)            lookbehind, asserts that the previous text follows the pattern
  (?<!Foo)            negative lookbehind, asserts that the previous text does not follow the pattern
  \d                  shorthand for [0-9]
  \D                  shorthand for [^0-9]
  \w                  shorthand for [A-Za-z0-9_]
  \W                  shorthand for [^A-Za-z0-9_]
  \s                  shorthand for [\f\n\r\t\v\u0020\u00a0\u1680\u2000-\u200a\u2028\u2029\u202f\u205f\u3000\ufeff]
  \S                  shorthand for [^\f\n\r\t\v\u0020\u00a0\u1680\u2000-\u200a\u2028\u2029\u202f\u205f\u3000\ufeff]
  \t                  matches horizontal tab
  \r                  matches carriage return
  \n                  matches newline
  \v                  matches vertical tab
  \f                  matches formfeed
  [\b]                matches backspace
  \0                  matches null byte
  \cX                 matches a control character with caret notation
  \xHH                matches a character with a hex code
  \uHHHH \u{HHHHHH}   matches a unicode character with a hex code
  \p{UnicodeProperty} @TODO matches a unicode property
  \\                  matches \
  foo|bar             matches either foo or bar
  \N                  matches a backreference by index, N must be >=1
  \k<Name>            matches a backreference by name
  ^                   asserts the match only at the beginning of the input (or beginning of the line if .MULTILINE is specified)
  $                   asserts the match only at the end of the input (or end of the line if .MULTILINE is specified)
  \b                  word boundary, asserts that one side is \w while the other side is \W
  \B                  word boundary, asserts that either side is either \w or \W

Flags:
    GLOBAL           : matches all occurences instead of the first
    CASE_INSENSITIVE : ignores case
    MULTILINE        : Keeps searching after encountering \n
    DOT_ALL          : `.` matches everything, including newlines
    STICKY           : requires the next match to be exactly where the last match index left off
    FOLD_GROUPS      : in disjunctions, will fold mutually exclusive groups into overlapping indices.
                     : e.g. (foo)|(((bar))) -> foo = ["foo","foo"]             instead of ["foo","foo","","",""]
                                            -> bar = ["bar","bar","bar","bar"] instead of ["bar","","bar","bar","bar"]

*/

#import "Basic";
#import "Math";
String :: #import "String";
#import "Unicode";
#import "Hash_Table";

#load "compile.jai";
#load "match.jai";
#load "replace.jai";
#load "to_string.jai";
#load "allocator.jai";

Flags :: enum_flags {
    NONE :: 0;
    GLOBAL           :: (1 << 0);
    CASE_INSENSITIVE :: (1 << 1);
    MULTILINE        :: (1 << 2);
    DOT_ALL          :: (1 << 3);
    // UNICODE_SETS     :: (1 << 4);
    STICKY           :: (1 << 5);
    FOLD_GROUPS      :: (1 << 6);
}

Regex :: struct {
    total_steps := 0;
    last_index := 0;
    flags: Flags;
    root: Group;
    pattern: string;
    max_capture_index: int;
}

#scope_module

Symbol :: enum {
    // Characters:
    WILDCARD; // .

    LITERAL; // character literal

    // Logic:
    DISJUNCTION;      // |
    GROUP;            // ()
    BACKREFERENCE;    // \1

    // Character classes:
    CHAR_CLASS; // []

    ASSERTION; // \b \B (?=Foo)
}

Assertion_Type :: enum {
    NONE;
    WORD_BOUNDARY; // \b \B
    LOOKAHEAD;     // (?=Foo) (?!Foo)
    LOOKBEHIND;    // (?<=Foo) (?<!Foo)
    ANCHOR_BEGIN;  // ^
    ANCHOR_END;    // $
}

Step :: struct {
    symbol: Symbol;

    quantifier: Quantifier;

    union {
        literal: struct {
            count: int;
            chars: [128] u8; // Yes we can fit this many without enlarging the union
        };

        group: Group;

        char_class: struct {
            Char_Class :: struct {
                range_start: int;
                range_end: int;
            }

            negated: bool;
            chars: [..] Char_Class;
        };

        backreference: struct {
            by_index: bool;
            index: int;
            name: string;
        }

        disjunction: struct {
            left: Group;
            right: Group;
        }

        assertion: struct {
            type: Assertion_Type;

            // If applicable
            negated: bool;
            group: Group;
        };
    }
}

Quantifier :: struct {
    min_times := 1;
    max_times := 1;
    greedy := true;
}

Group_Flags :: enum_flags {
    CASE_INSENSITIVE :: (1 << 0);
    MULTILINE        :: (1 << 1);
    DOT_ALL          :: (1 << 2);
}

Group :: struct {
    flags: Group_Flags;

    is_non_capturing := false;
    index := -1;
    name: string;

    steps: [..] Step;
}

get_codepoint :: inline (p: *string) -> int, int {
    char, char_size := inline character_utf8_to_utf32(p.data, p.count);
    return char, char_size;
}

next_codepoint :: inline (p: *string, size := -1) {
    cp_size := size;
    if cp_size == -1 then _, cp_size = inline character_utf8_to_utf32(p.data, p.count);
    inline advance(p, cp_size);
}

