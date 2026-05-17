# Stylesheet Lowering Guide

This document describes how the `stylesheet` construct in Slynx surface syntax is lowered into the SlynxIR.

## Overview

A `stylesheet` in Slynx mixes three distinct concepts, each of which maps to a different IR construct:

1. **Styling** — the style properties themselves → lowered to a **struct**
2. **Turing completeness** — expressions, `let` bindings, parameters → lowered to a **function**
3. **Reuse / inheritance** — the `uses` keyword → lowered to a **struct merge**

## Lowering Example

Given the following Slynx source:

```slynx
stylesheet Rounded(size: px) {
  styles {
    borderRadius: size
  }
}
stylesheet RoundedRed() uses Rounded(5px) {
  styles {
    backgroundColor: red
  }
}

component C {
  Div {
    style: Rounded(12px),
  }
}
```

The IR generator produces:

```slynxir
struct RoundedStyle {i32};
struct RoundedRed{i32,i32}

%RoundedRed __init_RoundedRed(){
$entry:
  %v = %RoundedRed{5px, 0xff0000};
  ret %v;
}

void RoundedRed(PrimitiveComponent, RoundedRed) {
$entry:
  roundness = getfield p1, 0;
  @sapply BorderRadius, p0, roundness;
  bg = getfield p1, 1;
  @sapply BackgroundColor, p0, bg;
}

void Rounded(PrimitiveComponent, RoundedStyle) {
$entry:
  roundness = getfield p1, 0;
  @sapply BorderRadius, p0, roundness;
  ret;
}

component %C() {
  #t0 = specialized Div;
  @initcall CInit, #t0;
  @initcall Rounded, #t0, __init_Rounded();
}
```

A stylesheet becomes **three** IR constructs:

1. A **struct type** (`RoundedStyle`) whose fields correspond to properties in `STYLES_TABLE.md` order.
2. A **constructor function** (`__init_Rounded`) that evaluates the property expressions and builds the struct literal.
3. An **apply function** (`Rounded`) that takes a component and the style struct, extracts each field, and emits an `@sapply` instruction for every property.

The component that uses the style gets **two** UI instructions:

- `@initcall` to its own init function (sets up children / text)
- `@initcall` to the style's apply function, with the constructor call result as the second operand

## Lowering of `uses` (Inheritance)

The lowering of `uses` merges style properties from parent stylesheets into a single struct. Properties are resolved left-to-right so that higher-precedence styles override lower-precedence ones.

```slynx
stylesheet A() {
  styles {
    borderRadius: 7px
  }
}

stylesheet B() uses A() {
  styles {
    backgroundColor: Blue
  }
}
```

`B`'s struct contains all properties from `A` merged with `B`'s own properties, with `B`'s values taking precedence when a property is defined in both.

Internally, the merge happens at the HIR level during `lower_stylesheet` in `helper/styles.rs`:

- Parent properties are collected first (in `uses` order).
- Own properties override parents with the same `StyleProperty` code.
- Fields are sorted by `StyleProperty` code (the numeric order from `STYLES_TABLE.md`).

## Precedence Rules

The `uses` clause follows left-to-right precedence:

```slynx
stylesheet C() uses A(), B() { ... }
// precedence: A < B < C
```

The apply function emits one `@sapply` per property. Properties with lower precedence (earlier in `uses`) are emitted first, and higher-precedence values override them because they appear later in the IR.

## Field Ordering Convention

Style struct fields follow a fixed positional order defined by `StyleProperty` codes (see `STYLES_TABLE.md`). The backend identifies each property by its position index, not by name.
