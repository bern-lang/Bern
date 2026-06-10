# Bern

<img align="center" src="img/logo.png" alt="Bern Logo" width="400"/>
<p align="center">An interpreted, dynamically typed programming language</p>

<p align="center">
  <a href="https://bern-lang.github.io/Bern">
    <img alt="Version" src="https://img.shields.io/badge/Version-2.0-blueviolet"></a>
  <a href="#technology">
    <img alt="Language" src="https://img.shields.io/badge/Built%20With-Haskell-purple"></a>
</p>

## Bern 2.0 | Major Release

Bern **2.0** is a major release. It turns the interpreter into a configurable language and adds a lot of new capability. Highlights:

* **New defaults:** auto-currying, lazy ranges, exhaustive-match checking, short-circuit `&&`/`||`, and negative indexing. Opt into errors-as-values (language errors become inspectable values instead of crashing) with the `{--! errors-as-values !--}` pragma; `error()`/`is_error()` user errors are always values.
* **Pragmas:** file-level directives like `{--! strict-types !--}` or `{--! immutable !--}` to opt into stricter or looser behaviour.
* **Language:** written-word operators, the `)|` pipe, `when` guards, `returns`/`such-that`/`lambda` syntax, and `loop ... in`.
* **Library & tooling:** JSON (`json_parse`/`json_stringify`), `keys()`, `error()`/`is_error()`, a much-improved REPL, and `bern build` for standalone executables.

> These features require Bern 2.0. See the full [Changelog](https://bern-lang.github.io/Bern/changelog.html) for details.

## About
Bern is a small, interpreted, dynamically typed programming language focused on clarity and expressiveness.

It blends ideas from functional programming (inspired by Haskell) with a lightweight, imperative syntax influenced by Lua and Odin.

Bern is designed so that:

* Everything is an expression
* Literals and expressions are evaluated and printed automatically
* Syntax stays minimal and readable
* Control flow is explicit and uniform

The goal is to make experimentation, scripting, and learning feel natural without sacrificing structure.

## VSCode Extension
A VSCode extension is available with syntax highlighting, snippets, and REPL integration. Search for "Bern Language" in the Extensions Marketplace, or install from the [VSCode Marketplace](https://marketplace.visualstudio.com/items?itemName=mirvoxtm.bern-syntax).

You can also find the source code for the extension in the `extension` folder of this repository.

## Contributing
Bern is open source, community driven and welcomes contributions! **Got a library you wanna implement? A framework you want out of the box? A bug you want to fix? A feature you want to add? Please, please, please contribute!**

You can open up a PR or an issue, and I'll be happy to review it and merge it if it's good. :D

## Language Overview

### Literals and Evaluation

In Bern, literals and expressions are evaluated immediately:

```
"Hello, World!"
23
```

There is no need for an explicit print function - evaluation implies output.

---

### Variables

Variables are defined with a simple assignment syntax:

```
integer = 2
double = 3.14
helloworld = "Hello, World!"
```

Variables can be used directly in expressions:

```
"Result from variable \"helloworld\": " + helloworld
```

---

### Conditionals

Bern provides clear `if / else if / else` control flow:

```
age = 18

if age >= 18 then
    "You are an adult."
else
    "You are a minor."
end
```

Multiple conditions can be chained naturally:

```
score = 85

if score >= 90 then
    "You got an A."
else if score >= 80 then
    "You got a B."
else if score >= 70 then
    "You got a C."
else
    "You need to improve."
end
```

---

### Loops

Bern uses a single keyword, `loop`, with different forms depending on context.

> `for` is accepted everywhere `loop` is, as a backwards-compatible alias. `loop` is the preferred spelling.

#### Repeat Loop

```
loop 3 do
    "Hello, Loops!"
end
```


#### Conditional Loop

```
counter = 2

loop counter > 0 do
    "Hello, Loops!"
    counter = counter - 1
end
```

#### Infinite Loop

```
loop true do
    "Hello!"
end
```

---

### Loop-In Iteration

Iterating over a string:

```
text = "Bern"

loop char : text do
    "Current char is: " + char
end
```


With automatic indexing:

```
text = "Bern"

loop char, index : text do
    "Current char is: " + char
    "Current index is: " + index
end
```

---

## Functions and Pattern Matching

Bern supports concise function definitions, lambdas, and multi-clause pattern matching.

### Simple Functions

Define a function with `def name(params) -> expr`:

```
def add(x, y) -> x + y
add(2, 3)
```

### Lambdas

Lambdas use the same arrow syntax and can be passed around:

```
inc = \x -> x + 1
pairSwap = \a, b -> [b, a]
inc(10)
pairSwap(1, 2)
```

### Multi-Clause Pattern Matching

Define multiple clauses; the first matching pattern runs:

```
def sign(0) -> "zero"
def sign(n) -> "positive"

sign(0)
sign(42)
```

Wildcards and literals are supported too:

```
def greet("Alice") -> "Hi, Alice!"
def greet(_) -> "Hello, someone else!"

greet("Alice")
greet("Bob")
```

### Block-Body Functions

Use `do ... end` with `return` when you need statements:

```
def sumList(xs) do
    total = 0
    loop n : xs do
        total = total + n
    end
    return total
end

sumList([1, 2, 3, 4])
```

## FizzBuzz in Bern

A complete FizzBuzz example demonstrating variables, conditionals, and loops:

```
numbers = [1..100]
loop n : numbers do 
    if n % 15 == 0 then 
        "FizzBuzz"
    else if n % 3 == 0 then
        "Fizz"
    else if n % 5 == 0 then
        "Buzz"
    else
        n
    end
end
```

Each branch evaluates to a value, which is automatically printed.

## Pattern-Matching Guards

Clauses can carry a `when` guard; a clause only matches when its patterns match **and** the guard is true. Guards work on function clauses, `case` branches, and lambdas. Negative numbers can be matched as literals, and a variable repeated in a pattern must bind equal values.

```
def classify(n) when n > 0 -> "positive"
def classify(0)            -> "zero"
def classify(n)            -> "negative"

def sign(-1) -> "minus one"
def sign(_)  -> "other"

def same(x, x) -> "equal"
def same(_, _) -> "different"
```

## The Pipe Operator

The `)|` operator threads a value into a function as its **first** argument, so transformations read left to right. `x )| f` is `f(x)`, and `x )| f(a, b)` is `f(x, a, b)`.

```
[1, 2, 3, 4] )| filter(\x -> x % 2 == 0) )| map(\x -> x * 10)
-- [20, 40]
```

## Written-Word Operators

Every symbolic operator also has a written-word form, so code can read like prose. Symbols and words are interchangeable.

| Symbol | Word | Symbol | Word |
|---|---|---|---|
| `+` | `plus` | `==` | `equals` (or `is`) |
| `-` | `minus` | `!=` | `not-equals` |
| `*` | `times` | `&&` | `and` |
| `/` | `divided-by` | `\|\|` | `or` |
| `%` | `modulo` | `!` | `not` |
| `>` | `is-greater` | `<>` | `concat` |
| `<` | `is-less` | `<\|` | `union` |
| `>=` | `is-greater-or-equal` | `\|>` | `intersect` |
| `<=` | `is-less-or-equal` | `</>` | `difference` |
| `::` | `typeof` | `)\|` | `and-do` |
| `:>` | `length` | `=` | `be` (assignment) |

Loops also accept `in` (for `:`), and function and case bodies accept `returns` (for `->` / `=`).

```
nums be [1, 2, 3, 4, 5]
3 is-greater 2 and 5 is-less-or-equal 5
[1, 2] concat [3, 4]
```

## Tooling

* `bern`: interactive REPL
* `bern build app.brn`: bundle a script and its imports into a standalone `.exe` (`-o` sets the output name)

## Technology

* Interpreter written in Haskell
* Dynamically typed
* Expression-oriented evaluation model


## Status

Bern is released and under active development.
Syntax and semantics may still evolve as the language grows.

Contributions, feedback, and experimentation are welcome.


## AI Disclosure
I feel like I should add this for disclosure. Bern was not created with the help of AI. The Parser, Interpreter, Libraries and Documentation were all written by me.

However, It would be impossible to develop more advanced features completely alone.

***I strongly feel that people should NOT use AI for writing code - specially when adding stuff to Bern. If you are a developer that is (aside from being way smarter than me) interested in contributing to Bern, and would want to replace all AI-generated or AI-assisted code with manually written code, please, please do so.***

Here is a list of features that were developed with the help of AI and that I would like to be rewritten by a human:

- *Foreign Function Interface (FFI)* 
I feel like this is the biggest offender, as it was the thing that opened the gates for me even touching AI in this project. I haven't used it prior to this.

Even with the help of AI, this was still a very difficult feature to implement. I could not find any decent material regarding implementation of FFIs in Haskell built languages, so I had to appeal. 

I feel like the current implementation is messy.

If you know about Haskell and have experience with FFI implementations, please, please rewrite the FFI implementation in Bern. I would be very grateful.

- *Written-Word Operators*
Not that it makes much of a difference, but I did use AI to automatically add my written-word operators to the Parser (names only), which was a very tedious task.

- *Commenting*
I'm a very messy programmer, and I don't like commenting my code. So I used AI to add comments to my code, which was a very helpful way to make the code more readable and maintainable.

I did give instructions for it to comment in a specific way, but I feel like the comments could be improved by a human. If you have experience with commenting code, please, please rewrite the comments in the code. I would be very grateful.

- *JSON Library*
The implementation of a JSON library was 'impossible' according to a colleague of mine. I took upon myself to try and implement it, but stumbled upon many difficulties in regards to actual Bern incompatibilities with the implementation of a JSON library.

I had to appeal to AI to get it done, rewriting some of the Interpreter's code and guiding it through the Bernparsec library.

Please rewrite the JSON library in Bern if you have experience with JSON libraries, Bern and Haskell. I would be very grateful.

- *Test Cases*
Some of the test cases for new features were generated with the help of AI. I feel like they could be improved by a human, and that they could be more comprehensive.

Please note - as a clarification - that the library for testing was written by me, so that is not AI-generated code, but the test cases themselves were generated with the help of AI.

