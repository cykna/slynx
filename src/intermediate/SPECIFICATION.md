# Slynx IR

Slynx IR specifies what the backend compiling it should do instead of how, even though the language is a bit opiniously about how to do so.
First of all the IR follows SSA and is extremely typed.
The IR has got the concept of 'contexts' that are anything able to be runned, this means a struct isn't a context, because it by itself cannot execute code, but a method or a function are, as well as components that can have code
to be run, such as the reactivity their reactivity model.

## Syntax

### Structs
The IR has implementation of operations primitives. The syntax for the deffinition of a struct can be the following:

`struct %name {int, float};`

Which represents the first created struct that in slynx code would be:

```
object S {
  property_1: int,
  property_2: float
}
```

Tuples use the same method, so a tuple in slynx denotated by
```slynx

object T(int,float);

```

is defined by the same way as `S`, the only thing is how their fields are accessed on the code. (Tuples must be implemented in slynx)

#### Functions
Functions are defined on the IR level as the following:
```
int #add(int, int) {
  result = addint p0, p1;
  ret result;
}
```
Where p0 is the first parameter and p1 the second one. The order of the names then is pN where N is the (N+1)th parameter of the function.
Something in slynx such as
```slynx
struct Currency {
  money: int
}

func currency_of(money: int): Currency {
  Currency(money: money)
}
```
in the IR can be represented by

```

struct %Currency {int}

%Currency currency_of(int) {
  result = %Currency{0};
  storefield result, 0, p0;
  ret result;
}

```

Which represents that it creates a temporary variable named `result` being the currency zeroed. storefield stores the value of `p0` on the first field of `result`

#### Components

Since components in Slynx are a bit more complex than a simple struct, as they can execute some sort of code during runtime, they can be declared as contexts as well. This said, the implementation of the reactivity may be specific from one compiler to another
even though slynx in its PoC is idealized(might change since its not complete yet) to be OOP like.
A component like the following:

```slynx

func f(n: int): int {
  n * 2
}

component Counter {
  pub prop count = 0;
  Text {
    text: count
  }
  Text {
    text: f(count)
  }
}

func main():Component {
  Counter {}
  Counter {count: 12}
}
```

Can be compiled to an IR that looks like the following:

```

int f(int) {
  result = muli32 p0, 2;
  ret result;
}

component %Counter(int) {
  %count: int = p0;
  #t0: specialized Text;
  #t1: specialized Text;
  @bind %count -> field #t0, 0;
  @bind %count |> f -> field #t1, 0;
}

AnyComponent main() {
  Counter0 = %Counter(0); //0 = default value
  Counter1 = %Counter(12);
  ret Counter1
}

```
The idea is that instead of the value being optional on the IR as it's on the Slynx code, is to when the value is ommited, we instead of passing null, pass the explictly the default value.
For some button that updates the state, suppose the following code:

```slynx

func f(n: int): int {
  n * 2
}

component Counter {
  pub prop count = 0;
  Text {
    text: count
  }
  Text {
    text: f(count)
  }
  Button {
    on_click: _ -> count += 1
  }
}

func main():Component {
  Counter {}
  Counter {count: 12}
}

```
which will generate:

```

special component %Text(bytes) {
  %text: bytes = p0;
}

int f(int) {
  result = muli32 p0, 2;
  ret result;
}

void Counter_count_update(%Counter) {
  count = prop_get p0, 0;
  inc = addi32 count, 1;
  prop_set p0, 0, inc;
  @emit %count;
}

component %Counter(int) {
  %count: int = p0;

  #t0: specialized Text;
  #t1: specialized Text;
  #b1: specialized Button;

  #b1.click -> Counter_count_update(self)

  @bind %count -> field #t0, 0;
  @bind %count |> f -> field #t1, 0;
  
}

AnyComponent main() {
  Counter0 = %Counter(0); //0 = default value
  Counter1 = %Counter(12);
}

```

Differently of default values, special values are primitives that are expected to exist on the runtime we are compiling to.
