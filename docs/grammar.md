# Grammar of the Slynx language

## This document specifies the entire grammar of the Slynx language version 0.0.1 using BNF as the meta-syntax.


### utils

```bnf
<id> ::= <letter_or_underscore> | <letter_or_underscore> <id>
<letter_or_underscore> ::= "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z" | "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" | "_"
<digits> ::= <number> | <number> <digits>
<number> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
<empty> ::= 
<opMultiplicative> ::= "*" | "/"
<opAdditive> ::= "-" | "+"
```

### Expresion

```bnf

  <op> ::= "==" | "<" | ">" | "<=" | ">="
  <multiplicative> ::= <primary> <multiplicative_tail>
  <multiplicative_tail> ::= <opMultiplicative> <primary> <multiplicative_tail>
                            | <empty>
  <additive> ::= <multiplicative> <additive_tail>
  
  <additive_tail> ::= <opAdditive> <multiplicative> <additive_tail>
                  | <empty>
  <comparison> ::= <additive> <comparison_tail>
                  
  <comparison_tail> ::= <op> <additive> <comparison_tail>
                                      | <empty>
  <logical> ::= <comparison> <logical_tail>
  <logical_tail> ::= <logical_op> <comparison> <logical_tail>
                     | <empty>
  <logical_op> ::= "&&" | "||"
  <expr> ::= <if> |  <logical>
```


### Primitive types
```bnf
<float> ::= <digits> "."<digits>
<bool> ::= "true" | "false"
<int> ::= <digits>
<type_opt> ::= ":" <type> | <empty>
<type> ::= <float> | <int> | <bool>
```

### Objects
```bnf
  <field>  ::= <id> ":" <type>
  <field_list> ::= <field> | <field> "," <field_list>
  <object> ::= <id> "{" <field_list> "}" | <id> "{" <empty> "}"
```

### Functions

```bnf
  <func> ::= "func" <id> "(" <args> ")" ":" <type> <func_body> 
  <func_body> ::= "->" <expresion> ";" | "{" <statement_list> "}"
  
  <args> ::= <arg> | <arg> "," <args> | <empty>
  <arg> ::= <id> ":" <type>

```
### variables

```bnf
<var> ::= "let" <mut_opt> <id> <type_opt> "=" <expression>

<mut_opt> ::= "mut" | <empty>

```
