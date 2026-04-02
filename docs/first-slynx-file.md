# First Slynx File

This short guide walks through a small `.slynx` file using the syntax that is
currently supported on the `main` branch.

The goal is to give new contributors one compact example they can read before
diving into the parser, checker, or IR code.

## A Small Example

```slynx
object Person {
    name: str,
    age: int,
}

func make_person(name: str, age: int): Person {
    Person(name: name, age: age)
}

component ProfileCard {
    pub prop title = "Profile";

    Text {
        text: title
    }
}

func main(): Component {
    let person = make_person("Maria", 22);

    if person.age > 18 {
        ProfileCard {
            title: person.name
        }
    } else {
        ProfileCard {
            title: "Minor"
        }
    }
}
```

## What This Example Shows

### 1. Object Declaration

```slynx
object Person {
    name: str,
    age: int,
}
```

This defines a structured value with named fields.

### 2. Function Returning An Object

```slynx
func make_person(name: str, age: int): Person {
    Person(name: name, age: age)
}
```

Points to notice:

- function arguments are typed;
- the return type is explicit;
- object construction uses named fields in parentheses;
- the last expression acts as the returned value.

### 3. Component Declaration

```slynx
component ProfileCard {
    pub prop title = "Profile";

    Text {
        text: title
    }
}
```

This shows the current component shape:

- `component` introduces a component declaration;
- `pub prop` declares a public prop;
- a prop can have a default value;
- nested component expressions use braces.

### 4. Main Returning A Component

```slynx
func main(): Component {
    let person = make_person("Maria", 22);

    if person.age > 18 {
        ProfileCard {
            title: person.name
        }
    } else {
        ProfileCard {
            title: "Minor"
        }
    }
}
```

This combines:

- `let` bindings;
- function calls;
- field access with dot syntax;
- `if` as an expression;
- component construction.

## Current Rules Worth Remembering

- function calls use parentheses: `sum(1, 2)`
- zero-argument calls are valid: `ping()`
- objects use named fields in parentheses: `Person(name: "A", age: 1)`
- components use braces: `ProfileCard {}`
- field access uses dot syntax: `person.age`
- non-final statements in a block still need `;`

## What This Guide Does Not Promise

This guide is intentionally limited to the syntax that is already grounded on
`main`.

It does **not** document as finished:

- slots;
- reactive graphs;
- finalized child lowering in the IR;
- a stable CLI workflow;
- a production-ready backend.

## Where To Go Next

- [docs/language-surface.md](language-surface.md)
- [README.md](../README.md)
- [middleend/README.md](../middleend/README.md)
