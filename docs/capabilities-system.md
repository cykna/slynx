# Capabilities

In slynx a feature that is being idealized is to make functions to have a system of capabilities, so each capability tells what kind of functions can be called. For example, if a function needs reading/writing files
it needs IO, so this function MUST be defined as IO, for example:

```slx
@capabilities(io(read))
func readfile(path: str): Maybe<str, IoError> {
  std::fs::readFile(path)
}
```

so any function that calls 'readfile' requires 'io(read)' as well. Its mainly it.
A function might be able to add capabilities even though they are not builtin, and no function call internally does so. for example, `std::fs::readFile` is idealized to call an OS syscall, and it won't come with the capabilities
system on it's own. So the function readFile create the requirement of a using capability, and now everything that needs it also needs this capability.

## Why This?

The main goal for this is to avoid of some package that does some io but doesn't say so. Some package that reads a file when it shouldn't be a requirement. So this make them more explicit. If a function is intended to
handle caching for example, why would it make requests? This kind of thing is idealized to be not necessarily removed, but easier to figure out.

## Idealization on Finishing

The idea is to differentiate between libraries and binaries, or programs in general. A binary is idealized to able to simply ignore the usage of capabilities and simply call a function T with capabilities S, without it's function defining it. This
should be made via flags.
But libraries instead, they MUST be OBLIGATED to have their system 100% explicit and the language SHOULD be able to give a compile time error to any library that simply does not follow it

## Runtime
At runtime the capability systems simply do not exist. They're fully comptime checks to know if a function A is allowed to call a function B, and so, if B is malicious, it's easier to know
