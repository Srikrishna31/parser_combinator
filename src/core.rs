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
pub type ParseResult<'a, Output> = Result<(&'a str, Output), &'a str>;

pub trait Parser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output>;

    fn map<F, NewOutput>(self, map_fn: F) -> BoxedParser<'a, NewOutput>
    where
        Self: Sized + 'a,
        Output: 'a,
        NewOutput: 'a,
        F: Fn(Output) -> NewOutput + 'a,
    {
        BoxedParser::new(map(self, map_fn))
    }

    fn pred<F>(self, pred_fn: F) -> BoxedParser<'a, Output>
    where
        Self: Sized + 'a,
        Output: 'a,
        F: Fn(&Output) -> bool + 'a,
    {
        BoxedParser::new(pred(self, pred_fn))
    }

    fn zero_or_more(self) -> BoxedParser<'a, Vec<Output>>
    where
        Self: Sized + 'a,
        Output: 'a,
    {
        BoxedParser::new(zero_or_more(self))
    }

    fn one_or_more(self) -> BoxedParser<'a, Vec<Output>>
    where
        Self: Sized + 'a,
        Output: 'a,
    {
        BoxedParser::new(one_or_more(self))
    }

    fn either<P>(self, other: P) -> BoxedParser<'a, Output>
    where
        Self: Sized + 'a,
        Output: 'a,
        P: Parser<'a, Output> + 'a,
    {
        BoxedParser::new(either(self, other))
    }

    fn and_then<F, NextParser, NewOutput>(self, f: F) -> BoxedParser<'a, NewOutput>
    where
        Self: Sized + 'a,
        Output: 'a,
        NewOutput: 'a,
        NextParser: Parser<'a, NewOutput> + 'a,
        F: Fn(Output) -> NextParser + 'a,
    {
        BoxedParser::new(and_then(self, f))
    }
}

/// A simple parser which just looks at the first character in the string and decides whether or not
/// it's the letter a.
// fn the_letter_a(input: &str) -> ParseResult<()> {
//     match input.chars().next() {
//         Some('a') => Ok((&input['a'.len_utf8()..], ())),
//         _ => Err(input),
//     }
// }

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

/// # A Parser builder
/// A function that produces a parser for a static string of any length, not just a single character.
/// This building block helps us to parse `<`, `>`, `</`, `/>`, `=` etc.
pub fn match_literal<'a>(expected: &'a str) -> impl Parser<'a, ()> {
    move |input: &'a str| match input.starts_with(expected) {
        true => Ok((&input[expected.len()..], ())),
        _ => Err(input),
    }
}

/// # Combinators
/// The next step is to write another parser builder function, one that takes two parsers as input
/// and returns a new parser which parses both of them in order. In other words a `parser combinator`,
/// because it combines two parsers into one.
pub fn pair<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, (R1, R2)>
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
pub(crate) fn left<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R1>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(left, _right)| left)
}

pub(crate) fn right<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R2>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(_left, right)| right)
}

/// # One or More
fn one_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
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

pub fn zero_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
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
pub fn any_char(input: &str) -> ParseResult<char> {
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

pub fn whitespace_char<'a>() -> impl Parser<'a, char> {
    pred(any_char, |c| c.is_whitespace())
}

/// # Space0 and Space1
/// space1 is a parser that matches one or more whitespace characters, and space0 is a parser that
/// matches zero or more whitespace characters.
pub fn space1<'a>() -> impl Parser<'a, Vec<char>> {
    one_or_more(whitespace_char())
}

pub fn space0<'a>() -> impl Parser<'a, Vec<char>> {
    zero_or_more(whitespace_char())
}

/// # And-then
/// This takes a parser, and a function that takes the result of a parser and returns a new parser,
/// which we'll then run. It's a bit like `pair`, except instead of just collecting both results in
/// a tuple, we thread them through a function.
pub fn and_then<'a, P, F, A, B, NextP>(parser: P, f: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A>,
    NextP: Parser<'a, B>,
    F: Fn(A) -> NextP,
{
    move |input| {
        parser
            .parse(input)
            .and_then(|(next_input, result)| f(result).parse(next_input))
    }
}

/// # Either
/// This parser combinator tries two parsers in order: if the first parser succeeds, we're done, we
/// return its result and that's it. If it fails, instead of returning an error, we try the second
/// parser on the same input. If that succeeds, great, and if it doesn't, we return the error too,
/// as that means both our parsers have failed, and that's an overall failure.
pub fn either<'a, P1, P2, A>(parser1: P1, parser2: P2) -> impl Parser<'a, A>
where
    P1: Parser<'a, A>,
    P2: Parser<'a, A>,
{
    move |input| parser1.parse(input).or_else(|_| parser2.parse(input))
}

/// # Quoted String
/// This parser parses a quoted string, which is defined more precisely as follows:
/// * one quote
/// * zero or more characters that are not a quote
/// * followed by another quote
pub fn quoted_string<'a>() -> impl Parser<'a, String> {
    right(
        match_literal("\""),
        left(
            zero_or_more(any_char.pred(|c| *c != '"')),
            match_literal("\""),
        ),
    )
    .map(|chars| chars.iter().collect())
}

pub struct BoxedParser<'a, Output> {
    parser: Box<dyn Parser<'a, Output> + 'a>,
}

impl<'a, Output> BoxedParser<'a, Output> {
    pub fn new<P>(parser: P) -> Self
    where
        P: Parser<'a, Output> + 'a,
    {
        BoxedParser {
            parser: Box::new(parser),
        }
    }
}

impl<'a, Output> Parser<'a, Output> for BoxedParser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self.parser.parse(input)
    }
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
}
