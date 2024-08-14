3bi)

re ::= union
union ::= union `|` intersect | intersect
intersect ::= intersect & concat | concat
concat ::= concat not | not
not ::= ~ not | star
star ::= star* | star+ | star? | atom
atom ::= ! | # | . | c

3bii)

Because union is an or operator, the intersect never has to be true. Thus, we would use union to check if we are in a true union which would cause us to infinitely keep checking if union is right.

3biii)

re ::= union
union ::= intersect {`|` intersect}
intersect ::= concat {`&` concat}
concat ::= not {not}
not ::= {~} star
star ::= atom | atom {*} | atom {+} | atom {?}
atom ::= ! | # | . | c

3biv)

re ::= union
union ::= intersect unions
unions ::= € | `|` intersect unions
intersect ::= concat intersects
intersects ::= € | `&` concat intersects
concat ::= not concats
concats ::= € | not concats
not ::= nots star
nots ::= € | `~` nots
star ::= atom stars
stars ::= € | stars {*} | stars {+} | stars {?}
atom ::= ! | # | . | c | `(`re`)`
