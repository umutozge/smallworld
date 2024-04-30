SmallWorld sample lexicon
-------------------------

def proper-names () {
s/(s[bar=?x]\np[bar=?x]); \p.p lex';
john
mary
}


def sg-intransitives () {
s\np[sg];
\x.lex' x;

talks walks
}

def pl-intransitives () {
s\np[pl];
\x.lex' x;

talk
walk
}

def transitive () {
s\np/np; \x\y.lex'x y; likes
}


def sg-nouns () {
n[sg];
\x.lex' x;
spy
}

def pl-nouns () {
n[pl];
\x.lex' x;
spies
}

def definite () {
(s/(s\np[agr=?x]))/n[agr=?x];
\p\q. q (lex' p);

the
}

def existential () {
(s/(s\np))/n;
\p\qEx. p x & q x;

some
}

def universal () {
(s/(s\np[sg]))/n[sg];
\p\qAx. p x > q x;

every
}

def negative-q () {
(s/(s\np[sg]))/n[sg] ;

\p\q~Ex. p x & q x;

no
}
