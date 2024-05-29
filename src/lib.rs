//#![feature(type_alias_impl_trait)]
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

/// Parsing is a process of deriving structure from a stream of data. A parser is something which
/// teases out that structure. In it's simplest form, a parser is a function which takes some input
/// and returns either the parsed output along with the remainder of the input, or an error.
/// Fn(Input) -> Result<(Output, Input), Error>
///
/// Unfortunately the below definition doesn't work as we get the error:
/// `impl Trait` in type aliases is unstable (see issue #63063)
/// type Parser<Element> = impl Fn(&str) -> Result<(&str, Element), &str>;
///
/// So, do all the below machinery to get around it.
/// Taken from: https://stackoverflow.com/questions/57937436/how-to-alias-an-impl-trait
/// This trait is local to this crate, so we can implement it on any type we want.
// trait Parser<Element>: Fn(&str) -> Result<(&str, Element), &str> {}
//
// // So let's go ahead and implement `Parser` on any type that implements the `Fn(&str) -> Result<(&str, Element), &str>`.
// impl <Element: Fn(&str) -> Result<(&str, Element), &str>> Parser<Element> for Element {}

/// A simple parser which just looks at the first character in the string and decides whether or not
/// it's the letter a.
fn the_letter_a(input: &str) -> Result<(&str, ()), &str> {
    match input.chars().next() {
        Some('a') => Ok((&input['a'.len_utf8()..], ())),
        _ => Err(input),
    }
}

/// # A Parser builder
/// A function that produces a parser for a static string of any length, not just a single character.
fn match_literal<'a>(expected: &'a str) -> impl Fn(&str) -> Result<(&str, ()), &str> + 'a {
    move |input| match input.get(0..expected.len()) {
        Some(next) if next == expected => Ok((&input[expected.len()..], ())),
        _ => Err(input),
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn literal_parser() {
        let parse_joe = super::match_literal("Joe");
        assert_eq!(parse_joe("Joe"), Ok(("", ())));
        assert_eq!(parse_joe("Joe!"), Ok(("!", ())));
        assert_eq!(parse_joe("Not Joe"), Err("Not Joe"));

        let parse_joe_1 = super::match_literal("Hello Joe!");
        assert_eq!(Ok(("", ())), parse_joe_1("Hello Joe!"));
        assert_eq!(Ok((" Hello Robert!", ())), parse_joe_1("Hello Joe! Hello Robert!"));
        assert_eq!(Err("Hello Mike!"), parse_joe_1("Hello Mike!"));
    }
}