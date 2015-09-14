# Ansuz

**Ansuz** is a combinatorial parsing library for the scheme programming language.
It is talored on [Gambit](http://gambitscheme.org/wiki/index.php/Main_Page)

## Prerequisites

download and install
[Gambit](http://gambitscheme.org/wiki/index.php/Main_Page)

## Download

    git clone git@github.com:francesco-bracchi/ansuz-gambit.git

## Build

    cd ansuz-gambit
    make

## Install

    sudo make install

### Examples

    make calc

runs an interactive matematical expression evaluator

    make json

parses one million times the string `"{\"x\" : true, \"y\" : false }"`

## Language

**TBD**

see [ansuz-clojure](https://github.com/francesco-bracchi/ansuz-clojure)
i.e. the same library for clojure, the naming convention is slightly
different, in scheme is `(define-parser (name ...) ...)` while in clojure
is `(defparser name [...] ...)`

