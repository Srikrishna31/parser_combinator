mod core;

use core::*;
/// # The Xcruciating Markup Language
/// We're going to write a parser for a simplified version of XML. It looks like this:
///    <parent-element>
///        <single-element attribute="value">text</single-element>
///    </parent-element>
/// XML elements open with the symbol `<` and an identifier consisting of a letter followed by any
/// number of letters, numbers and `-`. This is followed by some whitespace, and an optional list
/// of attribute pairs: another identifier as defined previously, followed by a `=` and a double
/// quoted string. Finally, there is either a closing `/>` to signify a single element with no
/// children, or a `>` to signify there is a sequence of child elements following, and finally a
/// closing tag starting with `</`, followed by an identifier which must match the opening tag, and
/// a final `>`.

#[derive(Clone, Debug, PartialEq, Eq)]
struct Element {
    name: String,
    attributes: Vec<(String, String)>,
    children: Vec<Element>,
}

/// A simple parser which just looks at the first character in the string and decides whether or not
/// it's the letter a.
fn the_letter_a(input: &str) -> ParseResult<()> {
    match input.chars().next() {
        Some('a') => Ok((&input['a'.len_utf8()..], ())),
        _ => Err(input),
    }
}

/// # A Parser for something less specific
/// The rule for the element name identifier is - one alphabetical character, followed by zero or more
/// of either an alphabetical character, a number, or a hyphen.
fn match_identifier(input: &str) -> ParseResult<String> {
    let mut matched = String::new();
    let mut chars = input.chars();

    match chars.next() {
        Some(c) if c.is_alphabetic() => matched.push(c),
        _ => return Err(input),
    }

    chars
        .take_while(|c| c.is_alphanumeric() || *c == '-')
        .for_each(|c| matched.push(c));

    let next_index = matched.len();

    Ok((&input[next_index..], matched))
}

fn attribute_pair<'a>() -> impl Parser<'a, (String, String)> {
    pair(match_identifier, right(match_literal("="), quoted_string()))
}

fn attributes<'a>() -> impl Parser<'a, Vec<(String, String)>> {
    zero_or_more(right(space1(), attribute_pair()))
}

fn element_start<'a>() -> impl Parser<'a, (String, Vec<(String, String)>)> {
    right(match_literal("<"), pair(match_identifier, attributes()))
}

fn single_element<'a>() -> impl Parser<'a, Element> {
    left(element_start(), match_literal("/>")).map(|(name, attributes)| Element {
        name,
        attributes,
        children: vec![],
    })
}

fn open_element<'a>() -> impl Parser<'a, Element> {
    left(element_start(), match_literal(">")).map(|(name, attributes)| Element {
        name,
        attributes,
        children: vec![],
    })
}

fn element<'a>() -> impl Parser<'a, Element> {
    whitespace_wrap(either(single_element(), parent_element()))
    // whitespace_char()
    //     .zero_or_more()
    //     .either(single_element())
    //     .either(open_element())
}

fn close_element<'a>(expected_name: String) -> impl Parser<'a, String> {
    right(
        match_literal("</"),
        left(match_identifier, match_literal(">")),
    )
    .pred(move |name| name == &expected_name)
}

fn whitespace_wrap<'a, P, A>(parser: P) -> impl Parser<'a, A>
where
    P: Parser<'a, A>,
{
    right(space0(), left(parser, space0()))
}

fn parent_element<'a>() -> impl Parser<'a, Element> {
    open_element().and_then(|e1| {
        left(zero_or_more(element()), close_element(e1.name.clone())).map(move |children| {
            let mut e1 = e1.clone();
            e1.children = children;
            e1
        })
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn literal_parser() {
        let parse_joe = match_literal("Joe");
        assert_eq!(parse_joe.parse("Joe"), Ok(("", ())));
        assert_eq!(parse_joe.parse("Joe!"), Ok(("!", ())));
        assert_eq!(parse_joe.parse("Not Joe"), Err("Not Joe"));

        let parse_joe_1 = match_literal("Hello Joe!");
        assert_eq!(Ok(("", ())), parse_joe_1.parse("Hello Joe!"));
        assert_eq!(
            Ok((" Hello Robert!", ())),
            parse_joe_1.parse("Hello Joe! Hello Robert!")
        );
        assert_eq!(Err("Hello Mike!"), parse_joe_1.parse("Hello Mike!"));
    }

    #[test]
    fn identifier_parser() {
        assert_eq!(
            Ok(("", "i-am-an-identifier".to_string())),
            match_identifier("i-am-an-identifier")
        );
        assert_eq!(
            Ok((" entirely an identifier", "not".to_string())),
            match_identifier("not entirely an identifier")
        );
        assert_eq!(
            Err("!not at all an identifier"),
            match_identifier("!not at all an identifier")
        );
    }

    #[test]
    fn pair_combinator() {
        let tag_opener = pair(match_literal("<"), match_identifier);
        assert_eq!(
            Ok(("/>", ((), "my-first-element".to_string()))),
            tag_opener.parse("<my-first-element/>")
        );
        assert_eq!(Err("oops"), tag_opener.parse("oops"));
        assert_eq!(Err("!oops"), tag_opener.parse("!oops"));
    }

    #[test]
    fn right_combinator() {
        let tag_opener = right(match_literal("<"), match_identifier);

        assert_eq!(
            Ok(("/>", "my-first-element".to_string())),
            tag_opener.parse("<my-first-element/>")
        );
        assert_eq!(Err("oops"), tag_opener.parse("oops"));
        assert_eq!(Err("!oops"), tag_opener.parse("!oops"));
    }

    #[test]
    fn quoted_string_parser() {
        assert_eq!(
            Ok(("", "I am a quoted string".to_string())),
            quoted_string().parse("\"I am a quoted string\"")
        );
    }

    #[test]
    fn attribute_parser() {
        assert_eq!(
            Ok((
                "",
                vec![
                    ("one".to_string(), "1".to_string()),
                    ("two".to_string(), "2".to_string())
                ]
            )),
            attributes().parse(" one=\"1\" two=\"2\"")
        );
    }

    #[test]
    fn single_element_parser() {
        assert_eq!(
            Ok((
                "",
                Element {
                    name: "div".to_string(),
                    attributes: vec![("class".to_string(), "float".to_string())],
                    children: vec![]
                }
            )),
            single_element().parse("<div class=\"float\"/>")
        );
    }

    #[test]
    fn xml_parser() {
        let doc = r#"
            <top label="Top">
                <semi-bottom label="Bottom"/>
                <middle>
                    <bottom label="Another bottom"/>
                </middle>
            </top>"#;
        let parsed_doc = Element {
            name: "top".to_string(),
            attributes: vec![("label".to_string(), "Top".to_string())],
            children: vec![
                Element {
                    name: "semi-bottom".to_string(),
                    attributes: vec![("label".to_string(), "Bottom".to_string())],
                    children: vec![],
                },
                Element {
                    name: "middle".to_string(),
                    attributes: vec![],
                    children: vec![Element {
                        name: "bottom".to_string(),
                        attributes: vec![("label".to_string(), "Another bottom".to_string())],
                        children: vec![],
                    }],
                },
            ],
        };
        assert_eq!(Ok(("", parsed_doc)), element().parse(doc));
    }

    #[test]
    fn mismatched_closing_tag() {
        let doc = r#"
            <top>
                <bottom/>
            </middle>"#;
        assert_eq!(Err("</middle>"), element().parse(doc));
    }
}
