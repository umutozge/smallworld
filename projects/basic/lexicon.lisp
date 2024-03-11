SmallWorld sample lexicon
-------------------------

def proper-names {
s/(s\np[sg]);
\p.p lex';

john
mary
}

def sg-intransitives {
s\np[sg];
\x.lex' x;

talks
}

def pl-intransitives {
s\np[pl];
\x.lex' x;

talk
}

def s-gnouns {
n[sg];
\x.lex' x;

spy
}

def plnouns {

n[pl];
\x.lex' x; 

spies 
}
         
def definite {
(s/(s\np))/n; 
\p\q. q (lex' p);

the
}

def existential {
(s/(s\np[sg]))/n[sg];
\p\qEx. p x & q x;

some 
}

def universal {
(s/(s\np[sg]))/n[sg];
\p\qAx. p x > q x;

every
}

def negativeq {
(s/(s\np[sg]))/n[sg] ; 

\p\q~Ex. p x & q x; 

no 
}
