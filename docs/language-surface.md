# Current Language Surface

This document describes the language surface that is currently supported on the
`main` branch of `Slynx-Language/slynx`.

It is intentionally scoped to what the repository can already lex, parse, and
type-check today. It should be safe to reuse this document on the landing page
or docs site without marketing unfinished features as already available.

## Status

This is a grounded overview of the current language surface.

It does **not** try to document:

- slot syntax as implemented code;
- reactive graph lowering as implemented code;
- generics as a finished language feature;
- a stable public CLI workflow;
- a stable backend/output contract beyond the current `.sir` output.

For design-only topics, see:

- [docs/component-slots.md](component-slots.md)
- [middleend/docs/reactive-graph-generation.md](../middleend/docs/reactive-graph-generation.md)
- [middleend/docs/linerize-the-graph.md](../middleend/docs/linerize-the-graph.md)

## Top-Level Declarations

The current parser accepts three top-level declaration kinds:

- `object`
- `component`
- `func`

Example:

```slynx
object Person {
    name: str,
    age: int,
}

component Profile {
    Text {
        text: "profile"
    }
}

func main(): Component {
    Profile {}
}
```

## Functions

Functions use the `func` keyword.

They currently support:

- typed arguments;
- an explicit return type;
- a block body;
- or a short arrow body.

### Block Body

```slynx
func add(a: int, b: int): int {
    let total = a + b;
    total
}
```

### Arrow Body

```slynx
func add(a: int, b: int): int -> a + b;
```

### Notes

- the return type is required in the current syntax;
- the last expression in a block can act as the implicit return value;
- a non-`void` function should still end in a return-producing expression.

## Statements

Inside function bodies, the current frontend supports:

- immutable `let`;
- mutable `let mut`;
- assignment;
- expression statements.

Example:

```slynx
func main(): int {
    let value = 1;
    let mut total: int = 2;
    total = value + total;
    total
}
```

### Assignment

Assignments currently work with:

- identifiers;
- field access expressions such as `person.age`.

Example:

```slynx
func main(): int {
    let mut count = 0;
    count = count + 1;
    count
}
```

## Expressions

The current surface includes:

- integer literals;
- float literals;
- string literals;
- boolean literals;
- identifiers;
- function calls;
- object expressions;
- component expressions;
- field access;
- binary expressions;
- `if` expressions.

### Literals

```slynx
let i = 10;
let f = 1.5;
let s = "hello";
let ok = true;
```

### Function Calls

Function calls use parentheses and now accept zero or more arguments.

```slynx
func ping(): int -> 0;

func main(): int {
    ping();
    0
}
```

### Binary Expressions

The current parser supports arithmetic, comparison, and logical operators.

Examples already covered by the codebase include:

- `+`
- `-`
- `*`
- `/`
- `==`
- `>`
- `>=`
- `<`
- `<=`
- `&&`
- `||`

Example:

```slynx
func bigger(a: int, b: int): bool -> a > b && a >= b;
```

### If Expressions

`if` currently behaves as an expression.

Example:

```slynx
func main(): int {
    let x = if 1 > 9 {
        1
    } else {
        3
    };

    x
}
```

## Objects

Objects are declared with named fields.

Object expressions currently use parentheses with named arguments:

```slynx
object Person {
    name: str,
    age: int,
}

func main(): int {
    let mut person = Person(name: "John", age: 10);
    person.age = 55;
    person.age
}
```

### Notes

- object fields are currently separated by commas in declarations;
- object construction uses named fields;
- field access uses dot syntax.

## Components

Components are declared with the `component` keyword and use brace-based
construction.

Example:

```slynx
component Header {
    Div {
        Text {
            text: "Header"
        }
    }
}

func main(): Component {
    Header {}
}
```

### Component Members

The current parser already understands:

- child components inside a component body;
- `prop` declarations;
- `pub prop` declarations;
- default prop values.

Example:

```slynx
component Counter {
    pub prop count = 0;

    Text {
        text: count
    }
}
```

### Special Case: `children`

The parser has a dedicated `prop children;` path that currently maps to a
vector-of-components style shape internally.

That is parser/frontend behavior today, but the full slot/child model is still
being designed separately.

## Comments

The lexer currently supports common line comments:

```slynx
// this is a comment
func main(): int -> 0;
```

## Current Non-Goals Of This Document

These topics are still being discussed or implemented and should not be read as
finished language surface:

- `while` as a fully merged feature on `main`;
- slot syntax as implemented code;
- final child representation in the IR;
- reactive graph lowering;
- generic-specialized component lowering;
- finalized backend semantics.

## Practical Reading

If you want to see real examples that match the current repository syntax,
start here:

- [slynx/component.slynx](../slynx/component.slynx)
- [slynx/variables.slynx](../slynx/variables.slynx)
- [slynx/functioncall.slynx](../slynx/functioncall.slynx)
- [slynx/objects.slynx](../slynx/objects.slynx)
- [slynx/If.slynx](../slynx/If.slynx)
