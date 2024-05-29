SmallWorld sample lexicon with morphology
-----------------------------------------

Verbs:
def intransitive (v) {s[-]\np; \x.lex' x; talk  walk panic}
def transitive (v) {(s[-]\np)/np; \x\y.lex' x y; like make watch}

Verbal agreement:
def phi (agr) {(s[+]\np[pl])\(s[-]\np); \x.x; 0}
def phi (agr) {(s[+]\np[sg])\(s[-]\np); \x.x; 1}

Tense:
def past (tense) {s[+]\np\(s\np); \x.x; past }
def present (tense) {s[+]\np[agr=?x]\(s\np[agr=?x]); \x.x; pres }

Nouns:
def proper-noun (pn) {np[sg]; lex'; john mary}
def noun (n) {n; \x.lex' x; spy cat city fox panic watch}

Nominal agreement:
def number (num) {n[pl]\n; \x.x; pl}
def number (num) {n[sg]\n; \x.x; sg}
def number (num) {np[pl]\np; \x.x; pl}
def number (num) {np[sg]\np; \x.x; sg}

Case:
def case (case) {s/(s[+]\np[agr=?x])\np[agr=?x]; \x\p. p x ; nom}
def case (case) {(s[fin=?x]\np)\(s[fin=?x]\np/np)\np; \x\v\y. v x y ; acc}

Determiners:
def definite (d) {np[agr=?x]/n[agr=?x]; \x.lex' x ; the}
