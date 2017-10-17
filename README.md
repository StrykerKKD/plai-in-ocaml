# Programming Languages: Application and Interpretation with Ocaml
This project implements the projects from [PLAI](http://cs.brown.edu/courses/cs173/2012/book/) in ocaml.

## How to use:
You should use the project with the latest version of the ocaml compiler.
You can optain the latest version of ocaml with the use of the [opam switch command](https://opam.ocaml.org/doc/Usage.html#opam-switch).

Also this projects depends on core and ppx_deriving:
```sh
opam install core ppx_deriving
```
Clone the repo:
```sh
git clone https://github.com/StrykerKKD/plai-in-ocaml.git
cd plai-in-ocaml
```

To run a project from the repository you need to build it with jbuilder:
```sh
jbuilder build projectname.exe
./_build/default/projectname.exe

#For example(building the parser project):
jbuilder build parser.exe
./_build/default/parser.exe
(MultC (PlusC (NumC 1 ) (NumC 2 ) ) (PlusC (NumC 3 ) (NumC 4 ) ) )
```

## Editor support
Jbuilder automatically builds a .merlin file, so you only need to have merlin and an editor that supports it.

To install merlin: `opam install merlin` .
I recommend using visual studio code with the Ocaml extension for this project.