/*

Features:

  $$      inserts a "$" literal
  $&      inserts the matched substring
  $`      inserts the pre-match substring
  $'      inserts the post-match substring
  $N      inserts the capture group N
  $<Name> inserts the named capture group Name (<> is required)

*/


//
// Simple Regex substitution. Supports $N to reference capture groups.
// e.g. ` \b(\w+)\b, [$1] ` will substitute all words and place brackets around them.
//
// Each procedure will return its input if it failed to match or compile the Regex.
// If it successfully matched and replaced, the resulting string is allocated using the context's
// allocator.
//

replace :: (text: string, find_pattern: string, replace_pattern: string, options := Match_Options.{}) -> string, bool {
    regex, success := compile(find_pattern, .MULTILINE);
    defer deinit(*regex);

    if !success then return text, false;

    new_text, occurrences := replace_all(text, *regex, replace_pattern, options);

    return new_text, occurrences > 0;
}

replace :: (text: string, find_regex: *Regex, replace_pattern: string, options := Match_Options.{}) -> string, bool {
    old_flags := find_regex.flags;
    defer find_regex.flags = old_flags;

    find_regex.flags &= ~.GLOBAL;
    find_regex.flags |= .MULTILINE;

    new_text, occurrences := replace_all(text, find_regex, replace_pattern, options);

    return new_text, occurrences > 0;
}

replace_all :: (text: string, find_pattern: string, replace_pattern: string, options := Match_Options.{}) -> string, int {
    regex, success := compile(find_pattern, .GLOBAL | .MULTILINE);
    defer deinit(*regex);

    if !success then return text, 0;

    new_text, occurrences := replace_all(text, *regex, replace_pattern, options);

    return new_text, occurrences;
}

replace_all :: (text: string, find_regex: *Regex, replace_pattern: string, options := Match_Options.{}) -> string, int {
    matches, match_result := match(find_regex, text);
    defer free_matches(matches);

    if match_result != .MATCH then return text, 0;

    builder: String_Builder;

    last_match_pos := 0;

    for match: matches {
        pre_match: string;
        pre_match.data = text.data + last_match_pos;
        pre_match.count = match.captures[0].data - text.data - last_match_pos;

        append(*builder, pre_match);

        replacement_text := replace_pattern;
        if match.captures.count > 1 {
            for #v2 < 1..match.captures.count-1 {
                full_pre_match := text;
                full_pre_match.count = match.captures[0].data - text.data;

                full_post_match: string;
                full_post_match.data = match.captures[0].data + match.captures[0].count;
                full_post_match.count = text.count - (full_post_match.data - text.data);

                // @TODO: probably really slow and stupid
                replacement_text = String.replace(replacement_text, "$$", "$",, temp);
                replacement_text = String.replace(replacement_text, "$&", match.captures[0],, temp);
                replacement_text = String.replace(replacement_text, "$`", full_pre_match,, temp);
                replacement_text = String.replace(replacement_text, "$'", full_post_match,, temp);
                replacement_text = String.replace(replacement_text, tprint("$%", it), match.captures[it],, temp);

                named_end := 0;
                while true {
                    named_start := String.find_index_from_left(replacement_text, "$<", named_end);

                    if named_start == -1 then break;

                    named_end = String.find_index_from_left(replacement_text, #char ">", named_start);

                    if named_end == -1 then break;

                    name: string;
                    name.data = replacement_text.data + named_start + 2;
                    name.count = named_end - named_start - 2;

                    named_capture, success := table_find(*match.named_captures, name);

                    if !success then continue;

                    replacement_text = String.replace(replacement_text, tprint("$<%>", name), named_capture,, temp);
                }
            }
        }

        append(*builder, replacement_text);

        last_match_pos += pre_match.count + match.captures[0].count;
    }

    post_match: string;
    post_match.data = text.data + last_match_pos;
    post_match.count = text.count - last_match_pos;

    append(*builder, post_match);

    return builder_to_string(*builder), matches.count;

}

#scope_file

