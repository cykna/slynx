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

The idea is still immature but it's idealized to be somewhat like this.
An example of codes that are respectively correct and incorrect are:
```slx
let socket = Socket.open();
switch await socket.stablishConnection() {
  Ok(_) => {
    //here the socket is Connected for the type checker
  },
  Err(_) => breakControlFlow(), //here its not connected.
}
socket.sendData(b"Hello World") //since its exaustive, this expects the socket to be connected. A breakControlFlow() is just an example method to tell that the control flow was broken at that point and wont continue further.
//so it garantuees that it will be connected.
```

```slx
let socket = Socket.open();
socket.sendData(b"Hello World"); //errors cause the socket requires to be connected, but its only opened
if await socket.stablishConnection() matches Ok(_) {
  //here its Connected
  socket.sendData(b"Hello World2"); //this is permitted since the state is Connected
}
socket.sendData(b"Hello World3"); //this is not because if something has the chance to happen, but is not garanteed, then we assume that it didn't happen
```

## Internal Representation
The internal representation will NOT be affected by the type states, since they're just a capabilities systems to say what can and cannot be executed during comptime. Based on this, the Socket is just an wrapper over `SocketRaw`
and so it can simply be removed and its function work directly inside `SocketRaw` the thing is that it's a lowering phase, where it's garanteed that it is working properly, thus no monomorphization is going to be made.

Based on the statement above, what can really be made is that maybe only the 'stablishConnection' function would really exist on the IR after optimization phases such as inlining and dead code analysis, and all the other
functions would simply get erased, and since it works on a single field, maybe the struct Socket itself don't exist, but the function works at the SocketRaw directly instead
