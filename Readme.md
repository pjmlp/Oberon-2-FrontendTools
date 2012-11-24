# Introduction

These grammar and lexer files were first created back in 1997, when I was fully into learning about compiler development.

In those days [Oberon-2](http://en.wikipedia.org/wiki/Oberon-2_%28programming_language%29) was quite popular in European universities, for having an [operating system](http://en.wikipedia.org/wiki/Oberon_%28operating_system%29) fully done in a GC enabled language, hence my interest in the language.

I am now making the lexer and grammar files available to a broader audience.

# Overview

## Lexer
The lexer helper functions make use of C++ language features, as the original goal was to integrate the generated lexer into a C++ application.

Even though Flex generates C code, there was already the goal to make use of the features that make C++ a better C.

## Grammar

The grammar is Yacc based and was written to target Bison as Yacc processor.

Due to some naming conflict with the flex version of the time, the all keyword tokens are suffixed with __TK_ to keep consistency.

While writting the grammar, I had 2 shift/reduce conflicts that were caused due to the fact that the original specification was in EBNF for a recursive descent parser so the following rules weren't a problem:

    Qualident  ::= [ident "."] ident.
    Designator ::= Qualident {"." ident | "[" ExprList "]" |  "(" Qualident ")" | "^"}.

 In a BNF grammar if the parser sees A.B it doesn't know if  it represents a Qualident or a Designator so I decided to
 create a new token `MOD_ID`, that the lexer should return in the case of detecting an id that represents a module name.

But the main reason is that to solve that conflict is necessary to to do some semantic checking and that is one thing that yacc (or his clones) can't do.

# Caveats

Althought these lexer and grammar worked for the purpose I had on those days, I cannot garantee they are fully compliant with the language specification.
