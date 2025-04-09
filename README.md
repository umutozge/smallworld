### SmallWorld

A linguists' parser based on Combinatory Categorial Grammar


#### Features

* Linguistically transparent derivations with shift-reduce parsing
* Rich feature structures for commensurability with Minimalism
* Separate definitions for feature theory and lexicon
* `xfst` integration through `flookup` for morphological analysis
* Terminal pretty-printing and TeX support



<details>
<summary>Installation</summary>



#### Prerequisites

First install the following to your system.

- [`git`](https://github.com/git-guides/install-git)
- [`SBCL`](https://www.sbcl.org/getting.html)
- [`rlwrap`](https://github.com/hanslub42/rlwrap)
- [`foma`](https://fomafst.github.io/)



Type,

```bash
sbcl --version
```

on the command-line to see if you have a working `SBCL`.


Install `quicklisp` as described [here](https://www.quicklisp.org/beta/), make sure to complete the following steps.
After downloading `quicklisp`:

```lisp
(quicklisp-quickstart:install)
```

then 

```lisp
(ql:add-to-init-file)
```

#### Fetch the program to your system


Clone the `SmallWorld` repo by going to your choice of installation directory and doing:

```bash
git clone git@github.com:umutozge/smallworld.git
```

#### Create the `smallworld` executable

The `git clone` command above will create a directory named `smallworld`. Change to the program directory by,

```bash
cd smallworld/
```

and do,

```bash
sbcl --script install.lisp
```

This operation, if successful, will create an executable file `smallworld` in your local bin directory. If you don't have one, the installer will create it. Note this directory, because you need to [add](https://medium.com/@jamexkarix583/add-bin-folder-to-the-path-772de253f579) it to your `PATH`.

#### Run the program

Now, you can run `smallworld` from anywhere on your system. You need to specify the project directory as a command line argument:


```bash
rlwrap smallworld -p <project-yaml-file> 
```

or, if you are already in the project directory, just:

```bash
rlwrap smallworld
```

If you do not have `rlwrap` -- which is highly recommended -- replace `rlwrap smallworld`
with `smallworld` in your commands.

#### How to update

To update `SmallWorld`, do,

```bash
git pull origin main
```

when in somewhere in the `smallworld` folder. Your local project files will NOT
be overwritten or get lost. Re-run the install script for changes to take
effect. Incidentaly, it is a good practice to keep your projects NOT in the
`smallworld` directory, since at some point you might have to delete your
`smallworld` directory and re-clone it.

</details>


<details>
<summary>Usage</summary>

#### Projects

A project consists of the file `<project-name>.yaml`.


```yaml
---
 feature-dictionary:
  - (agr pl sg)
  - (cat v n)
  - (bar 0 1 2)

 category-bundles:
  - (s (cat v)) 
  - (np (cat n) (bar 2))
  - (n (cat n) (bar 0))

 lexicon:
  - {pos: n, phon: [john], syn: "np[sg]", sem: lex'}

  - pos: n
    syn: np[sg]
    sem: lex' 
    phon:
      - john
      - mary

  - pos: n
    syn: n[sg]
    sem: lex' 
    phon: [dog,cat,spy]

  - pos: n
    syn: n[pl]
    sem: lex' 
    phon: [dogs,cats,spies]
```



Whenever you load a project, there will appear a file named `_lexicon.lisp` in
your project folder. This file will be useful to inspect the details of your
lexicon for debugging purposes.


#### Attribute-value matrices

These are basic, intuitive data records. Here is an example:

```lisp
((title sir)
 (name alex)
 (surname ferguson))
```

In every ordered pair, the first component is the ''attribute'' and the second is the ''value'' of that attribute. Any collection of such pairs is an ''attribute-value matrix'' (or ''AVM'' for short).

The real interest of attribute-value structures lies in their recursive structure; an attribute has another attribute-value structure as its value. E.g.:

```lisp
((title sir)
 (name alex)
 (surname ferguson)
 (pysique ((height 186cm)
           (weight 87kg)
           (color caucasian))))
```


###### Internal representation of categories

`SmallWorld` translates each category it finds in your `.lex` file to its internal representation, which is written to the file `_lexicon.lisp` each time you load a lexicon.


Here is an example lexical entry.

```yaml
  - pos: v
    syn: s\np[agr=sg]
    sem: lex' 
    phon: [talks,walks]
```

This entry defines the lexical category of 2 words. The `lex'` in the semantic
interpretation gets replaced by the listed tokens during the translation into
internal representation.

The internal representation of a lexical category is an AVM with three main
features: `PHON`, `SYN` and `SEM`.

```lisp
((PHON TALKS)
 (SYN
  ((IN ((CAT N) (AGR SG) (BAR 2)))
   (SLASH (DIR BACKWARD) (MODE DOT))
   (OUT ((CAT V) (AGR ?_) (BAR 1)))))
 (SEM (LAM X (TALKS X))))
```

`PHON` feature has the phonetic representation of the lexical item itself.

`SEM` is either an atom like `JOHN` or a lambda term. Inspecting the example lexicon will clarify how to write lambda terms.

`SYN` is a complex feature which has another AVM as its value. For functional categories like `TALKS` above, the value of the `SYN` feature is an AVM with three features: `IN` for the input category, `SLASH` for the directionality, and `OUT` for the output category.

</details>
