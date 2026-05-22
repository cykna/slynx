# IR Generation

**Status:** Draft  
**Author:** cycro (cykna)  
**Date:** 2025-05-21

The IR generation at the moment looks terrible, the main issue is that it was idealized to be DOD and with cheap handles, and it got to the point that it got. The main issue is that I(cycro, in github, cykna) am not very used to write things in a DOD way, but i felt it was necessary if I wanted to make the IR manipulation fast enought. 
The main problem on the IR right now is the leak of safety on doing operations, so, this makes it easy to write something incorrectly, and impossible to write something that is not highly tied to the HIR.

## Maybe a solution

Based on the statement above, my idea is to turn the IR into a big struct that exposes builder functions, so it makes things easier to:

1. Generate it in a more safe way
2. Make the IR generation not tied to HIR

Every process made after the IR generation, such as optimization and dead code analysis, constant folding, etc, is made after the conclusion of the IR, and so it SHOULD NOT affect anything. 
This'd make things easier to frontends that are not slynx but, for some reason opt to compile down to the slynx ir, such as, for example, the IR itself.
So what now is I dont even know how, would become such as:
```rs
for field in struct_fields {
  match field.ty {
    ... get ir type
  }
}
let mut s = ir.create_struct();
for field in ir_field_types {
  s.insert_field(field);
}
let s_id = s.generate(); //this returns now the ID to that struct

let int = ir.i32_type();
let s_again = ir.get_struct(s_id);
s_again.insert_field(int);
```

An abi to C is planned but maybe not now. The main goal is to make the IR generation of anything related to components, work properly. Maybe with bugs, but working at least. So I might first refactor to work, and if needed, refactor again to make it repr C compatible
