# Rematch

An ECMAScript compliant Regex written in Jai.
It has all the same features as the Regex from ECMAScript/JavaScript, except for the Unicode property matching.

## Getting started

To use this in your own project, copy the `Rematch` module to your `modules` folder.

## Features

### Syntax
|Syntax|Description|
|------|-----------|
|`abc`|Matches "abc" as a literal|
|`.`|Matches any character except newline (unless `.DOT_ALL` is specified)|
|`[abc]`|Matches either `a`, `b` or `c`|
|`[a-c]`|Matches character range from `a` to `c`|
|`[^abc]`|Matches any character that is not `a`, `b` or `c`|
|`[^a-c]`|Matches any character that is not in range from `a` to `c`|
|`(abc)`|Matches the group containing `abc`|
|`(?:abc)`|Matches the group containing `abc`, does not capture|
|`(?<Name>abc)`|Matches the group containing `abc`, names the capture `Name`|
|`(?im-s:abc)`|Modifies the flags for the capture group|
|`(?=Foo)`|Lookahead, asserts that the following text is equal to `Foo`|
|`(?!Foo)`|Negative lookahead, asserts that the following text is not equal to `Foo`|
|`(<?=Foo)`|Lookbehind, asserts that the previous text is equal to `Foo`|
|`(<?!Foo)`|Negative lookbehind, asserts that the previous text is not equal to `Foo`|
|`\d`|Shorthand for `[0-9]`|
|`\D`|Shorthand for `[^0-9]`|
|`\w`|Shorthand for `[a-zA-Z0-9_]`|
|`\W`|Shorthand for `[^a-zA-Z0-9_]`|
|`\s`|Shorthand for `[\f\n\r\t\v\u0020\u00a0\u1680\u2000-\u200a\u2028\u2029\u202f\u205f\u3000\ufeff]`|
|`\S`|Shorthand for `[^\f\n\r\t\v\u0020\u00a0\u1680\u2000-\u200a\u2028\u2029\u202f\u205f\u3000\ufeff]`|
|`\t`|Matches horizontal tab|
|`\r`|Matches carriage return|
|`\n`|Matches newline|
|`\v`|Matches vertical tab|
|`\f`|Matches formfeed|
|`[\b]`|Matches backspace|
|`\0`|Matches null byte|
|`\cX`|Matches a control character using caret notation|
|`\xHH`|Matches a character with hex notation|
|`\uHHHH`|Matches a unicode character with hex notation|
|`\u{HHHHHH}`|Matches a unicode character with hex notation, up to 6 hex digits|
|`\\`|Matches backslash|
|`foo\|bar`|Matches either `foo` or `bar`|
|`\N`|Matches a backreference by index, N must be >= 1|
|`\k<Name>`|Matches a backreference by name|
|`^`|Asserts that the match is only at the beginning of the input (or line if `.MULTILINE` is specified)|
|`$`|Asserts that the match is only at the end of the input (or line if `.MULTILINE` is specified)|
|`\b`|Word boundary, asserts that one side is `\w` while the other is `\W`|
|`\B`|Negated word boundary, asserts that both sides are either `\w` or `\W`|

### Flags
|Flag|Description|
|----|-----------|
|`GLOBAL`|Matches all occurences instead of only the first|
|`CASE_INSENSITIVE`|Ignores all ASCII case|
|`MULTILINE`|Keep searching after encountering a newline|
|`DOT_ALL`|`.` matches everything, including newlines|
|`STICKY`|Requires the next match to be exactly where the last match index left off|
|`FOLD_GROUPS`|Folds mutually exclusive groups into overlapping indices|

### Replace Syntax
|Syntax|Description|
|------|-----------|
|`$$`|Inserts a `$` literal|
|`$&`|Inserts the matched substring|
|`` $` ``|Inserts the pre-match substring|
|`$'`|Inserts the post-match substring|
|`$N`|Inserts the capture group `N`|
|`$<Name>`|Inserts the named capture group `Name`|

#### Fold Groups
Fold groups makes it so that when a disjunction is encountered,
both sides will have overlapping indices so that there are no gaps.

For instance, take the following pattern:
```regex
(foo)|(((bar)))
```
The following is the default matching behaviour:

Matching `foo`:
```json
["foo", "foo", "", "", ""]
```
Matching `bar`:
```json
["bar", "", "bar", "bar", "bar"]
```

The empty captures are the groups from the other branch. With `FOLD_GROUPS` these gaps don't occur.

Matching `foo`:
```json
["foo", "foo"]
```
Matching `bar`:
```json
["bar", "bar", "bar", "bar"]
```

## Examples

Matching an input string:
```go
regex, success := Rematch.compile("(\\w+)", .GLOBAL);
defer Rematch.deinit(*regex);

matches, match_result := Rematch.match(*regex, "Hello, these are words");
defer Rematch.free_matches(matches);

if match_result == .MATCH {
    for matches {
        log("%. %", it_index, it.captures);
    }
}
```

Setting match constraints:
```go
options := Rematch.Match_Options.{
    memory_limit = 16 * 1024 * 1024,
    time_limit_ms = 5000,
};

matches, match_result := Rematch.match(*regex, input_str, options);

if match_result == .OUT_OF_TIME || match_result == .OUT_OF_MEMORY {
    // ...
}
```

Converting compiled Regex back into a string:
```go
regex, success := Rematch.compile("(\\w+) {2,4}\\1", .GLOBAL);

pattern := Rematch.to_string(*regex);
defer free(pattern);

log("%", pattern); // "([a-zA-Z0-9_]+) {2,4}\1"
```
*Please note that `Rematch.to_string()` is not perfect, see below for remarks.*


Replacing a string using Regex:
```go
replaced, replace_count := Rematch.replace_all("These are words", "(\\w+)", "[$1]");
log("%", replaced); // "[These] [are] [words]"
```

### Testing

There are various tests included in JSON format. Run `jai build.jai` to build the test suite
and run it in `bin/rematch`.

You can generate tests using `node create-test.js 'pattern' 'input 1' 'input 2'`.
It will print JSON of the test, which can be added in the `tests/` directory.

## Remarks

### Known Issues

* As this is a backtracking engine, "catastrophic backtracking" can occur.
* `Rematch.to_string()` currently does not properly print cases where flags are toggled in capture groups, e.g. `(?i:(?-i:foo))`.
* `Rematch.compile()` is currently too lax. It allows for input like `foo^`, which should not be allowed.

### Remarks

I tried to add many tests to this engine, but I am sure that there are still edge cases. Please report them if you find them!

This engine is not super optimized. On my machine it can complete the test suite in around 20 ms.
Node/V8's Regex implementation can do it in about 11 ms.

This repo is mirrored from a private repository.

