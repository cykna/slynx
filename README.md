# Slynx, a data oriented UI language.

The Slynx language is a project intended to be a programming language that focuses on being data oriented and able to create UI's to different targets with the same code.

## Why Slynx?
#### Short Answer
The idea is for Slynx to be a UI language that focuses on being fast, readable, able to be used not for creating UI's that are agnostic of runtime and so, compiled down to different targets with the same code base. 

#### Long answer
The language tries to solve the problem of code not being able to be used across platforms and the difficulty of reading them to understand what's being shown on the screen. The main idea behind Slynx, which is what made me start it, is how hard it's to open a JSX file and get what's going on. The main idea is to make the language simple to be read, so if you look at the language and get confused and thinking "what am i looking at?" we failed as a language.

Other problem that is tried to be solved on the language is to make it fast, the idea is for the language to be data-oriented, so instead of the default being an AOS, the language treats things like SOA, and only AOS when explicitly said to. Another feature is to make it able to have good serialization of data, how? I'm still thinking.

These problems are intended to be solved at syntax level, so you'd write code normally that would be optimized as much as the frontend can to run as fast as a data-oriented handwritten code (on the long run, a compile time interpreter is something that would be interesting to be made, and actually, is intended to be made).

The language would be able to compile to different targets by exposing the IR and documenting it, and then, make it able to the community to generate specialized compilers, so, just execute the specialized compiler, and done, your Slynx code is compiling to that specific target. The idea is to make things as simple as that.
Since the backend and the frontend are separated, the same errors will cross different platforms, unless the compiler behind it modifies how the frontend should check things, which is idealized to be an option.

## Inspirations
#### Ui

The language is heavily inspired by QML and Slint like languages for the code that is written for coding UI.

#### General 
For general code, it's also inspired by languages like Rust, Swift and JS on features that can be used, such as async, generators, pattern matching, enums, etc. 

## Main Problem
Slynx is a project that will require a lot of work to start working, the Frontend of the language is idealized to be a library that you can use anywhere, get the generated IR and interpret/compile/interpret.
So the main problem is the frontend of the language requires a lot of steps to start working.
Doing this alone will be painful and then, this project is open to contributions.

## A basic example
```slynx
//styles are not made targeting specifically CSS
//It can be a target, but it's not the main one
style AppText(size: int, color:int) {
  styleprop font_family = "Sans Serif";
  styleprop font_size = size;
  styleprop text_color = color;
}

style Rounded(pixels:int){
  styleprop border_radius = pixels;
}

style RoundedCounter(color:int, round:int) inherits Rounded(round) {
  styleprop color = color;
}

static style GeneralText = AppText(16,0xffffff);

component Counter {
  prop value = 0;
  pub(parent) style: Style?;
  Div {
    style: style matches Some(s) ? s : RoundedCounter(0xff0000, 24),
    P {
      text: value,
      style: GeneralText
    }
    Button {
      text: "Increase to be $(value+1)",
      style: GeneralText,
      on_click: event -> value += 1; //automatically increases and updates where 'prop' is required
    }
  }
}

component AlotOfCounters<N:const int > 0>{
  for i in 0..N do if i % 2 {
    Counter{}
  };
  
}

func main():Component -> AlotOfCounters<20>;
``` 

## Compilation
Assert cargo is installed. Clone this codebase and run
```cargo build --release```
and to run it simply execute
```./target/release/Slynx --target filename```
or if you don't want doing so, simply clone this repo and run
```just run filename```
which will compile the code and then compile it down to js


## Roadmap

#### Legend:
[x] Done

[~] In progress / partial

[ ] Planned

#### Current State (TL;DR)

- Language: experimental
- Actual Focus: frontend + type system
- Initial Target: Web.
- Short-term focus: prove the language can be compiled to any codebased

#### Language Design
 - [~] Core vision and goals
 - [~] UI-oriented component model
 - [~] Function definitions(Only focusing Components now)
 - [~] Numeric and string types. 
 - [ ] Boolean type
 - [~] Object model (in progress)
 - [ ] Structs
 - [ ] Enums
 - [ ] Serialization/Deserialization
 - [ ] Control flow model (if / match / loops – formalized)
 - [ ] Error model

#### Lexer
 - [~] Lexer
 - [x] Identifiers
 - [~] Keywords, need to implement a lot yet
 - [~] Numbers, missing hex and binary
 - [x] Strings
 - [ ] Booleans
 - [ ] Comments
 - [ ] Unicode / edge cases

##### Parser
- [~] Parser (partial)
- [~] Expressions
- [~] Function definitions, missing statments
- [~] Component definitions, missing conditions and loop for children
- [~] Object definitions
- [ ] Generic parameters
- [ ] Pattern matching
- [ ] Error recovery
#### Type System
- [~] Hindley–Milner type inference
- [x] Numbers
- [x] Strings
- [~] Functions (UI support only)
- [ ]Booleans
- [x] Objects
- [x] Components as types
- [ ] Generics
- [ ] Monomorphization
- [x] Clear type error reporting
#### Intermediate Representation (IR)
- [ ] IR design and constraints
- [ ] SSA-based IR
- [ ] Data-oriented layout (SoA by default)
- [ ] Control flow representation
- [ ] Transformation passes
- [ ] Serialization-friendly format

#### Code Generation
JavaScript (initial target, and maybe, the only one)
- [~] JS backend design
- [~] IR → JS lowering
- [ ] Runtime support (reactivity, events)
- [ ] Minimal DOM / UI binding
- [ ] Example app running end-to-end

#### Frontend as a Library

- [ ] Public API for the frontend
- [ ] AST / IR exposed
- [ ] Embeddable in other tools
- [ ] Documentation for consumers
- [ ] Tooling & Documentation
- [ ] Initial README

#### Language reference
- [ ] IR documentation
- [ ] CONTRIBUTING.md
- [ ] More examples
