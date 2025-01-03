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
#import "Flat_Pool";

#load "compile.jai";
#load "match.jai";
#load "replace.jai";
#load "to_string.jai";

Regex_Flags :: enum_flags {
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
    flags: Regex_Flags;
    root: Regex_Group;
    pattern: string;
    max_capture_index: int;
    max_groups: int;
}

#scope_module

Regex_Symbol :: enum {
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

Regex_Assertion_Type :: enum {
    NONE;
    WORD_BOUNDARY; // \b \B
    LOOKAHEAD;     // (?=Foo) (?!Foo)
    LOOKBEHIND;    // (?<=Foo) (?<!Foo)
    ANCHOR_BEGIN;  // ^
    ANCHOR_END;    // $
}

Regex_Step :: struct {
    symbol: Regex_Symbol;

    literal: struct {
        char: int;
    };

    quantifier: Regex_Quantifier;

    group: Regex_Group;

    char_class: struct {
        Char_Class :: struct {
            range_start: int;
            range_end: int;
        }

        negated: bool;
        chars: [..] Char_Class;
    };

    backreference: struct {
        by_index := true;
        index: int;
        name: string;
    }

    disjunction: struct {
        left: Regex_Group;
        left.is_non_capturing = true;

        right: Regex_Group;
        right.is_non_capturing = true;
    }

    assertion: struct {
        type: Regex_Assertion_Type;

        // If applicable
        negated: bool;
        group: Regex_Group;
    };
}

Regex_Quantifier :: struct {
    min_times := 1;
    max_times := 1;
    greedy := true;
}

Regex_Group_Flags :: enum_flags {
    CASE_INSENSITIVE :: (1 << 0);
    MULTILINE        :: (1 << 1);
    DOT_ALL          :: (1 << 2);
}

Regex_Group :: struct {
    flags: Regex_Group_Flags;

    is_non_capturing := false;
    index := -1;
    name: string;

    steps: [..] Regex_Step;
}

get_codepoint :: (p: *string) -> int, int {
    char, char_size := character_utf8_to_utf32(p.data, p.count);
    return char, char_size;
}

next_codepoint :: (p: *string, size := -1) {
    cp_size := size;
    if cp_size == -1 then _, cp_size = character_utf8_to_utf32(p.data, p.count);
    advance(p, cp_size);
}



// @TODO: performance and memory improvements
// The following pattern takes a lot of time and memory:
//    ^(?=.*\\d)(?=.*[A-Z])(?=.*[a-z])(?=.*[^\\w\\d\\s:])([^\\s]){8,16}$
// with the following input text:
//    ^(?=.*\\d)(?=.*[A-Z])(?=.*[a-z])(?=.*[^\\w\\d\\s:])([^\\s]){8,16}$
//

