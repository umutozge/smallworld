;;; SmallWorld sample lexicon

; proper names
s/(s\np[sg])  : \p.p lex' < john mary
; intransitive verbs
s\np[sg]      : \x.lex' x < talks
s\np[pl]      : \x.lex' x < talk
; nouns
n[sg]         : \x.lex' x < spy
n[pl]         : \x.lex' x < spies
; determiners
(s/(s\np))/n : \p\q. q (lex' p) < the
(s/(s\np[sg]))/n[sg] : \p\q. q (lex' p) < a

