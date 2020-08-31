fry
===

Fry, a declarative object-oriented programming language. Implemented as a proof of concept for a talk I gave a long time ago to support a far-fetched thesis that if you want to do principled OO programming, you'll end up functional anyway.

Homepage (tutorials and examples): [http://maciejpirog.github.io/fry](http://maciejpirog.github.io/fry)

About Fry
---------

Fry is an experimental language that combines the best features of object-oriented (anybody?) and declarative paradigms. The slogans are: "**panobjectivity**" - everything is an object - which gives you the lightweightness feeling of Smalltalk (and their descendants such as Ruby), and "**less is more**" - no mutable data structures! - which gives you the true declarative lightweightness of pure functional programming (as in Haskell).

The theoretical foundation of Fry is Martín Abadi and Luca Cardelli's calculus of primitive objects augmented with base types, side-effects, and inheritance. Fry comes equipped with a full formal operational semantics.

Note that Fry is in its infancy. A lot of changes to the language and surroundings are bound to happen, some of them revolutionary, all of them unpredictable.

About the repo
--------------

The Fry project consists of the following parts, put in appropriate directories:

* **emacs-mode** - emacs syntax highliting mode for Fry
* **interpreter** - interactive interpreter written in Haskell

Todo list and issues
--------------------

* **TODO**: Base types (for now, there are only ints with basic arithemetic and strings with no operations on them)
* Performance is killed by the HOAS representation of programs and the fact that identifiers and labels are kept in the dictionaries as original strings. The reason for this is that want the interpreter to be as simple as possible, since the semantics of the language might change slightly in the future.


