### SmallWorld

A linguists' parser based on Combinatory Categorial Grammar


#### Features

* Linguistically transparent derivations with shift-reduce parsing
* Rich feature structures for commensurability with Minimalism
* Separate definitions for feature theory and lexicon
* Terminal pretty-printing and TeX support



<details>
<summary>

#### Installation
</summary>



##### Prerequisites

First install the following to your system.

- [`git`](https://github.com/git-guides/install-git)
- [`SBCL`](https://www.sbcl.org/getting.html)
- [`rlwrap`](https://github.com/hanslub42/rlwrap) (optional)


Type,

```bash
sbcl --version
```

on the command-line and see if you have a working `SBCL`.


Install [`quicklisp`](https://www.quicklisp.org/beta/) as described in the link, make sure to complete all the steps. 


##### Fetch the program to your system


Clone the `SmallWorld` repo by going to your choice of installation directory and doing:

```bash
git clone git@github.com:umutozge/smallworld.git
```


##### Create the `smallworld` executable

The `git clone` command above will create a directory named `smallworld`. Change to the program directory by,

```bash
cd smallworld/
```

and do,

```bash
sbcl --script install.lisp
```

This operation, if successful, will create an executable file `smallworld` in your local bin directory. If you don't have one, the installer will create it. Note this directory, because you need to [add](https://medium.com/@jamexkarix583/add-bin-folder-to-the-path-772de253f579) it to your `PATH`. 


##### Run the program

Now, you can run `smallworld` from anywhere on your system. You need to specify the project directory as a command line argument:


```bash
smallworld <project-directory>
```

If you installed `rlwrap` -- which is highly recommended -- you can run the command as:

```bash
rlwrap smallworld <project-directory>
```

##### How to update


To update `SmallWorld`, do,

```bash
git pull origin master
```
when in somewhere in the `smallworld` folder. Your local project files will NOT be overwritten or get lost. Re-run the install script.

</details>


<details>
<summary>

#### Guide
</summary>

##### Projects

Projects are found under the folder `prj`. A project consists of `theory.lisp` and `lexicon.lisp` files. The `lexicon.lisp` file is where you enter your lexicon; inspecting the file `prj/basic/lexicon.lisp` should be enough to understand its syntax. Understanding the function of `theory.lisp` requires having grasped some other concepts.

###### Attribute-value matrices 

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


###### The theory file

The function of the `theory.lisp` is to define the basic (or atomic) categories of your grammar/lexicon, i.e. "non-slashed" categories.
First you define a basic category template, which will be the most general structure of an atomic category.

```lisp
(base-cat-template ((cat ?_) (agr ?_) (bar ?_)))
```

This says that each basic category has a `cat`, `agr` and `bar` attribute (linguists prefer ''feature'' over ''attribute''). The basic template leaves the values of these features underspecified. In the notation of `SmallWorld`, symbols starting with a `?` are variables.

The second component of a theory is the feature dictionary

```lisp
(feature-dictionary (agr pl sg)
                    (cat v n a p m)
                    (bar 0 1 2))
```

This data structure declares that the `agr` feature can have `pl` and `sg` as values, and likewise for other features.

The third part of a theory file is the specification of category bundle symbols. What we write as, for instance `S`, in CCG categories get translated into an AVM according to these specifications:

```lisp
(category-bundle-symbols (s (cat v) (bar 1))
                         (np (cat n) (bar 2))
                         (adj (cat a) (bar 0))
                         (ap (cat a) (bar 2))
                         (n (cat n))
						 (m (cat m) (bar 2)))
```

In declaring category bundle symbols you only give the feature-value pairs that you want to be overridden on the base category template.

###### Internal representation of categories

`SmallWorld` translates each category it finds in `lexicon.lisp` to its internal representation, which is written to the file `_lexicon.lisp` each time you load a lexicon.


Here is an example lexical entry.

```
s\np[sg] : (lam x ($ x)) < sleeps walks works talks
```
This entry defines the lexical category of 4 words. The `$` in the semantic interpretation gets replaced by the word during the translation into internal representation.

The internal representation of a lexical category is an AVM with three main features: `PHON`, `SYN` and `SEM`. 

```lisp
((PHON SLEEPS)
 (SYN
  ((IN ((CAT N) (AGR SG) (BAR 2)))
   (DIR BACKWARD)
   (OUT ((CAT V) (AGR ?_) (BAR 1)))))
 (SEM (LAM X (SLEEPS X))))
```

`PHON` feature has the phonetic representation of the lexical item itself.

`SEM` is either an atom like `JOHN` or a lambda term. Inspecting the example lexicon will clarify how to write lambda terms.

`SYN` is a complex feature which has another AVM as its value. For functional categories like `SLEEPS` above, the value of the `SYN` feature is an AVM with three features: `IN` for the input category, `DIR` for the slash, and `OUT` for the output category.

Studying the example `lexicon.lisp`, `theory.lisp` and the `_lexicon.lisp` generated on the basis of the former two will clarify how the system works.
</details>
