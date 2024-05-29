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