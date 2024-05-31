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

/// A generic type alias for a parser function return type.
type ParseResult<'a, Output> = Result<(&'a str, Output), &'a str>;

trait Parser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output>;
}

/// Implement the Parser trait for any function that matches the signature of a parser.
/// This way, not only can we pass around the same functions we've been passing around so far as
/// parsers fully implementing the `Parser` trait, we also open up the possibility to use other
/// kinds of types as parsers.
impl<'a, F, Output> Parser<'a, Output> for F
where
    F: Fn(&'a str) -> ParseResult<'a, Output>,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self(input)
    }
}

/// A simple parser which just looks at the first character in the string and decides whether or not
/// it's the letter a.
fn the_letter_a(input: &str) -> ParseResult<()> {
    match input.chars().next() {
        Some('a') => Ok((&input['a'.len_utf8()..], ())),
        _ => Err(input),
    }
}

/// # A Parser builder
/// A function that produces a parser for a static string of any length, not just a single character.
/// This building block helps us to parse `<`, `>`, `</`, `/>`, `=` etc.
fn match_literal<'a>(expected: &'a str) -> impl Parser<'a, ()> {
    move |input: &'a str| match input.starts_with(expected) {
        true => Ok((&input[expected.len()..], ())),
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

/// # Combinators
/// The next step is to write another parser builder function, one that takes two parsers as input
/// and returns a new parser which parses both of them in order. In other words a `parser combinator`,
/// because it combines two parsers into one.
fn pair<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, (R1, R2)>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    move |input| {
        parser1.parse(input).and_then(|(next_input, result1)| {
            parser2
                .parse(next_input)
                .map(|(last_input, result2)| (last_input, (result1, result2)))
        })
    }
}

/// # Enter the functor
/// This combinator has one purpose: to change the type of the result.
/// This pattern is what's called a "functor" in Haskell and its mathematical sibling, category theory.
/// If you've got a thing with a type `A` in it, and you have a `map` function available that you can
/// pass a function from `A` to `B` into to turn it into the same kind of thing but with type `B`
/// instead, that's a functor.
fn map<'a, P, F, A, B>(parser: P, map_fn: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A>,
    F: Fn(A) -> B,
{
    move |input| {
        parser
            .parse(input)
            .map(|(next_input, result)| (next_input, map_fn(result)))
    }
}

/// # Left and Right
/// We are going to have to deal with result types like: `((), String)`, a lot. Some of our parsers
/// only match patterns in the input without producing values, and so their outputs can be safely
/// ignored. To accommodate this pattern, we're going to use out `pair` combinator to write two
/// other combinators: `left` which discards the result of the first parser and only returns the
/// second, and it's opposite number, `right`, which discards the second result and only returns the
/// first.
fn left<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R1>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(left, _right)| left)
}

fn right<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R2>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(_left, right)| right)
}


/// # One or More
fn one_or_more<'a ,P, A>(parser: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    move |mut input| {
        // TODO: Figure out a way to use the zero_or_more combinator to simplify this
        let mut result = Vec::new();

        if let Ok((next_input, first_item)) = parser.parse(input) {
            input = next_input;
            result.push(first_item);
        } else {
            return Err(input);
        }

        // TODO: Use take_while to simplify this
        while let Ok((next_input, next_item)) = parser.parse(input) {
            input = next_input;
            result.push(next_item);
        }

        Ok((input, result))
        // let first_item = if let Ok((next_input, first_item)) = parser.parse(input) {
        //     input = next_input;
        //     first_item
        // } else {
        //     return Err(input);
        // };


       // let mut result =  zero_or_more(parser).parse(input);
       //
       //  result.map(|(next_input, mut items)| {
       //      items.insert(0, first_item);
       //      (next_input, items)
       //  })
    }
}

fn zero_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    move |mut input| {
        let mut result = Vec::new();

        // TODO: Use take_while to simplify this
        while let Ok((next_input, next_item)) = parser.parse(input) {
            input = next_input;
            result.push(next_item);
        }

        Ok((input, result))
    }
}

/// # Any character
/// This parser returns a single `char` as long as there is one left in the input.
fn any_char(input: &str) -> ParseResult<char> {
    match input.chars().next() {
        Some(c) => Ok((&input[c.len_utf8()..], c)),
        _ => Err(input),
    }
}

/// # Pred Combinator
/// This combinator is a bit different from the others. It takes a parser and a predicate function.
/// If the parser succeeds, the predicate is called with the result of the parse. If the predicate
/// returns true, the parse succeeds and the result is returned. If the predicate returns false, the
/// parse fails and the input is returned unchanged.
fn pred<'a, P, A, F>(parser: P, predicate: F) -> impl Parser<'a, A>
where
    P: Parser<'a, A>,
    F: Fn(&A) -> bool,
{
    move |input| {
        if let Ok((next_input, value)) = parser.parse(input) {
            if predicate(&value) {
                return Ok((next_input, value));
            }
        }

        Err(input)
    }
}

fn whitespace_char<'a>() -> impl Parser<'a, char> {
    pred(any_char, |c| c.is_whitespace())
}

/// # Space0 and Space1
/// space1 is a parser that matches one or more whitespace characters, and space0 is a parser that
/// matches zero or more whitespace characters.
fn space1<'a>() -> impl Parser<'a, Vec<char>> {
    one_or_more(whitespace_char())
}

fn space0<'a>() -> impl Parser<'a, Vec<char>> {
    zero_or_more(whitespace_char())
}

/// # Quoted String
/// This parser parses a quoted string, which is defined more precisely as follows:
/// * one quote
/// * zero or more characters that are not a quote
/// * followed by another quote
fn quoted_string<'a>() -> impl Parser<'a, String> {
    map(
        right(
            match_literal("\""),
            left(zero_or_more(pred(any_char, |c| *c != '"')),
                 match_literal("\"")),
        ),
        |chars| chars.into_iter().collect(),
    )
}

fn attribute_pair<'a>() -> impl Parser<'a, (String, String)> {
    pair(match_identifier, right(match_literal("="), quoted_string()))
}

fn attributes<'a>() -> impl Parser<'a, Vec<(String, String)>> {
    zero_or_more(right(space1(), attribute_pair()))
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
    fn one_or_more_combinator() {
        let parser = one_or_more(match_literal("ha"));
        assert_eq!(Ok(("", vec![(), (), ()])), parser.parse("hahaha"));
        assert_eq!(Err("ahah"), parser.parse("ahah"));
        assert_eq!(Err(""), parser.parse(""));
    }

    #[test]
    fn zero_or_more_combinator() {
        let parser = zero_or_more(match_literal("ha"));
        assert_eq!(Ok(("", vec![(), (), ()])), parser.parse("hahaha"));
        assert_eq!(Ok(("ahah", vec![])), parser.parse("ahah"));
        assert_eq!(Ok(("", vec![])), parser.parse(""));
    }

    #[test]
    fn predicate_combinator() {
        let parser = pred(any_char, |c| *c == 'o');
        assert_eq!(Ok(("mg", 'o')), parser.parse("omg"));
        assert_eq!(Err("lol"), parser.parse("lol"));
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
            Ok(("", vec![
                ("one".to_string(), "1".to_string()),
                ("two".to_string(), "2".to_string())
            ])),
            attributes().parse(" one=\"1\" two=\"2\"")
        );
    }
}
