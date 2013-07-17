hot
===

Implementation of higher-order model checking tools.

Requirements
============

- Ocaml >= 4.00.1
- Batteries >= 2
- Autoconf >= 2.69
- Make

With OPAM:
`opam install ounit batteries`

Compilation
===========


Just run:
`./bootstrap && ./configure && make <target> && make install`

Replace `<target>` with
 - all (building stuff necessary for the library.)
 - everything (building stuff for the library, running tests and creating docs.)
