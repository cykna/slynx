# Ownership, Move Semantics and Single Writer

Slynx has no garbage collector and no borrow checker. The memory model is intentionally simpler than Rust’s, while still being safe enough to prevent the most common memory bugs without runtime overhead.

The two core rules are:

1. **Move semantics** — values are moved by default, not copied.
2. **Single writer** — at any point in time, only one thing can mutate a value.

### Move Semantics

When you assign a value to another variable or pass it to a function, the value is moved. The original binding becomes invalid after the move.

```slynx
let a = SomeStruct { x: 5 };
let b = a;     // a is moved into b
a.x;           // error: a has been moved
```

This ensures there is at most one owner of any value at any time. When the owner goes out of scope, the value is dropped automatically.

If you need to keep the original value, you must copy it explicitly:

```slynx
let a = SomeStruct { x: 5 };
let b = copy a;

a.x; // ok
b.x; // ok
```

The compiler never copies silently. You always know when a copy happens.

### Single Writer

At any moment, only one binding can mutate a value. This is enforced through move semantics rather than a full borrow checker.

Since a value has exactly one owner, and mutation requires ownership, there is naturally only one writer at a time.

```slynx
let mut a = SomeStruct { x: 5 };
a.x = 10;        // ok
```

You can pass mutable values to functions by moving them:

```slynx
func increment(s: mut SomeStruct): SomeStruct {
    s.x += 1;
    s
}

let mut a = SomeStruct { x: 5 };
let a = increment(a);   // moved in and back out
```

### References (`&` and `&mut`)

Slynx supports `&T` and `&mut T`. However, these references **do not** come with borrow checker guarantees. A `&mut SomeStruct` does not guarantee that the reference is still valid or that no other mutation has happened since it was created. Since the `&mut SomeStruct` is valid, the
no other writer/reader to that reference is possible until it's released.

This is an intentional design choice to keep the language simpler. The safety of references is expected to be handled through **typestates / automata**, which is the main mechanism the language will use to prevent use-after-free, invalid mutation, and similar bugs.

For example, a reference in a wrong state (e.g. trying to use a `&mut File<Closed>`) will be rejected at compile time by the automaton rules.

### Why not a full borrow checker?

Rust’s borrow checker is very powerful but comes with a steep learning curve and forces the programmer to constantly fight lifetimes and borrowing rules. Slynx aims for a much gentler experience while still avoiding garbage collection.

Move semantics + Single Writer + Typestate automata cover the majority of real-world use cases with far less complexity. Cases that still need stronger guarantees are handled explicitly through lanes or typestates.

### Why no garbage collector?

A GC introduces unpredictable pauses and hides allocation costs. For a desktop-first language (including UI), predictable performance and deterministic destruction are much more important.
