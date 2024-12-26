SmallWorld sample lexicon
-------------------------

def proper-name (pn) {
np; lex'; john mary
}
def iverb (v) {
s\np; lex';  sleep walk
}
def tverb (v) {
s\np/np; lex'; love chase
}
def noun (n) {
n[sg]; lex'; spy dog
}
def noun (n) {
n[pl]; lex' ; spies dogs
}
def determiner () {
np[agr=?x]/n[agr=?x]; lex'; the a
}
def singular (agr) {
s[+]\np[sg]\(s[-]\np); \x.x; sg
}
def plural (agr) {
s[+]\np[pl]\(s[-]\np); \x.x; pl 
}
def nominative (case) {
s/(s\np)\np; \x\q. q x ; nom 
}
def accusative (case) {
(s\np[agr=?x])\(s\np[agr=?x]/np)\np; \x\p. p x; acc
}
