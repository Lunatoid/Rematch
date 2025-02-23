
Match_Range :: struct {
    start: int;
    count: int;
}

Named_Match_Range :: struct {
    #as using range: Match_Range;
    name: string;
}

operator != :: (a: Match_Range, b: Match_Range) -> bool {
    return a.start == b.start && a.count == b.count;
}

Match_Ranges :: struct {
    captures: [..] Match_Range;
    named_captures: [..] Named_Match_Range;
}

Named_Match :: struct {
    name: string;
    text: string;
}

Match_Strings :: struct {
    captures: [..] string;
    named_captures: [..] Named_Match;
}

Match_Options :: struct {
    memory_limit  := 0;
    time_limit_ms := 0;
}

range_to_string :: inline (range: Match_Range, text: string) -> string {
    str := text;
    str.data += range.start;
    str.count = range.count;
    return str;
}

ranges_to_strings :: inline (ranges: [] Match_Range, text: string) -> [..] string {
    str_matches: [..] string;
    for ranges {
        array_add(*str_matches, range_to_string(it, text));
    }
    return str_matches;
}

match_ranges_to_strings :: (matches: [..] Match_Ranges, text: string) -> [..] Match_Strings {
    string_matches: [..] Match_Strings;

    for match: matches {
        m: Match_Strings;
        m.captures = ranges_to_strings(match.captures, text);
        for match.named_captures {
            str := range_to_string(it.range, text);
            array_add(*m.named_captures, .{ name=it.name, text=str });
        }
        array_add(*string_matches, m);
    }

    return string_matches;
}

