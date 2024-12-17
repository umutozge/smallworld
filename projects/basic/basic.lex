SmallWorld sample lexicon
-------------------------

def proper-names () {
np[sg]; lex'; john mary
}
def sg-intransitives () {
s\np[sg]; \x.lex'x;  talks walks
}
def pl-intransitives () {
s\np[pl]; \x.lex'x; talk walk
}
def sg-transitives () {
s\np[sg]/np; \x.lex'x; loves 
}
def pl-transitives () {
s\np[pl]/np; \x\y.lex'x y; love 
}
def sg-nouns () {
n[sg]; \x.lex'x; spy dog
}
def pl-nouns () {
n[pl]; \x.lex'x; spies dogs
}
def definite () {
np[agr=?x]/n[agr=?x]; \p.lex'p; the
}
