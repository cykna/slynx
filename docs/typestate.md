# Type States

On the type system of the language, one of my goals is to implement automatos. This means that a type T might be able to contain some state that represents some information about it during type checking. For example,
lets say a socket connection. It's able to have the Socket type, but closed, and yet call a method to send content.
The automatos on the type system would be to tell that 'this socket is closed', and then give or remove the capability of doing something. In this case, to send content when closed. In terms of code, I think in it being such as:

```
state SocketState {
  ///Is opened, able to send contents but not sure if the other side is receiving
  Opened,
  ///Opened a connection and the other side is listening properly
  Connected,
  ///Closes everything and cannot send data
  @final Closed,
}

pub struct Socket<@S: SocketState> {
  somefield: SocketRaw,

  pub func open(target: Ip): This<@S=Opened> {
    This {
      somefield:SocketRaw::new(target)
    }
  }
  pub async func stablishConnection(this: &mut This): MaybeError<void, ConnectionError> where
    Return::Ok -> set @S = Connected,
  {
    let data = this.somefield.recv();
    somelogic(data)?;
  }

  pub func sendData(this: &This, data: [&]u8):MaybeError<usize, ConnectionError> where 
    requires @S tobe Connected,
  {
    self.somefield.send(data)
  }
  
  pub func close(this: &mut This): void set @S = Closed{
    this.somefield.close();
  }
}
```

The main idea is that, since the lang is exaustive, the type checker analyzes which function was called and sets the state to be the one the function signature says so. It then requires that the Socket is Connected to be able
to send some data.

The `@final Closed` means that the type can only leave scope when its automato is at this final state, thus, Closed. It's incorrect to then, make the Socket leave the scope Opened or Connected.
There might be more than 1 `@final` state.

The idea is still immature but it's idealized to be somewhat like this
