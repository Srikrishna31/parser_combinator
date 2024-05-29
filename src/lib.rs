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
/// This building block helps us to parse `<`, `>`, `</`, `/>`, `=` etc.
fn match_literal<'a>(expected: &'a str) -> impl Fn(&str) -> Result<(&str, ()), &str> + 'a {
    move |input| match input.starts_with(expected) {
        true => Ok((&input[expected.len()..], ())),
        _ => Err(input),
    }
}

/// # A Parser for something less specific
/// The rule for the element name identifier is - one alphabetical character, followed by zero or more
/// of either an alphabetical character, a number, or a hyphen.
fn match_identifier(input: &str) -> Result<(&str, String), &str> {
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


/// # Combinators
/// The next step is to write another parser builder function, one that takes two parsers as input
/// and returns a new parser which parses both of them in order. In other words a `parser combinator`,
/// because it combines two parsers into one.
fn pair<P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Fn(&str) -> Result<(&str, (R1, R2)), &str>
where
    P1: Fn(&str) -> Result<(&str, R1), &str>,
    P2: Fn(&str) -> Result<(&str, R2), &str>,
{
    move |input| parser1(input).and_then(|(next_input, result1)| {
        parser2(next_input).map(|(last_input, result2)| (last_input, (result1, result2)))
    })
}


/// # Enter the functor
/// This combinator has one purpose: to change the type of the result.
/// This pattern is what's called a "functor" in Haskell and its mathematical sibling, category theory.
/// If you've got a thing with a type `A` in it, and you have a `map` function available that you can
/// pass a function from `A` to `B` into to turn it into the same kind of thing but with type `B`
/// instead, that's a functor.
fn map<P, F, A, B>(parser: P, map_fn: F) -> impl Fn(&str) -> Result<(&str, B), &str>
where
    P: Fn(&str) -> Result<(&str, A), &str>,
    F: Fn(A) -> B,
{
    move |input| parser(input).map(|(next_input, result)| (next_input, map_fn(result)))
}

#[cfg(test)]
mod tests {
    use super::{
        match_literal, match_identifier, pair,
    };

    #[test]
    fn literal_parser() {
        let parse_joe = match_literal("Joe");
        assert_eq!(parse_joe("Joe"), Ok(("", ())));
        assert_eq!(parse_joe("Joe!"), Ok(("!", ())));
        assert_eq!(parse_joe("Not Joe"), Err("Not Joe"));

        let parse_joe_1 = match_literal("Hello Joe!");
        assert_eq!(Ok(("", ())), parse_joe_1("Hello Joe!"));
        assert_eq!(Ok((" Hello Robert!", ())), parse_joe_1("Hello Joe! Hello Robert!"));
        assert_eq!(Err("Hello Mike!"), parse_joe_1("Hello Mike!"));
    }

    #[test]
    fn identifier_parser() {
        assert_eq!(Ok(("", "i-am-an-identifier".to_string())), match_identifier("i-am-an-identifier"));
        assert_eq!(Ok((" entirely an identifier", "not".to_string())), match_identifier("not entirely an identifier"));
        assert_eq!(Err("!not at all an identifier"), match_identifier("!not at all an identifier"));
    }

    #[test]
    fn pair_combinator() {
        let tag_opener = pair(match_literal("<"), match_identifier);
        assert_eq!(Ok(("/>", ((), "my-first-element".to_string()))), tag_opener("<my-first-element/>"));
        assert_eq!(Err("oops"), tag_opener("oops"));
        assert_eq!(Err("!oops"), tag_opener("!oops"));
    }
}