SmallWorld sample lexicon
-------------------------

### Nominal

def proper-noun (pn) {s/(s\np); \p.p lex'; john mary
}
def sg-pn (num) {(s/(s\np[sg]))\*(s/(s\np)); \x.x; sg
}
def plu-pn (num) {(s/(s\np[pl]))\*(s/(s\np)); \x.x; pl
}
def noun (n) {n; \x.lex' x; spy cat city fox panic try watch 
}
def number (num) {n[sg]\*n; \x.x; sg 
}
def number (num) { n[pl]\*n; \x.x; pl 
}
def definite (d) { (s/(s\np))/n; \p\q. q (lex' p); the
}
def existential (d) { (s/(s\np[sg]))/n[sg]; \p\qEx. p x & q x; some 
}
def universal (d) { (s/(s\np[sg]))/n[sg]; \p\qAx. p x > q x; every
}
def neg-quan (d) { (s/(s\np[sg]))/n[sg] ; \p\q~Ex. p x & q x; no 
}
def nominative (case) {(s/(s\np[agr=?x]))\*(s/(s\np[agr=?x])); \x.x; nom
}
def accusative (case) {((s\np)\((s\np)/np))\*(s/(s\np)); \p\q\y.p (\x.q x y); acc
}

### Verbal

def intransitive (v) { s\np; \x.lex' x; talk  walk  beg   
}
def transitive (v) { s\np/np; \x\y.lex' x y; like make panic try watch 
}
