# Lanes

Lanes are not a thing that I personally recognize in most of the languages. Since the language is idealized to have a good support to DOD, the idea of lanes is to make it easy to write SoA instead of AoS but making
it less painfull.
The main idea behind lanes is that they are such as an interface to an array/vector of some type T. For example:

```
lane UserPost {
  userId: id,
  contentMessage: string,
  likes: u64
}
let v = []UserPosts; //vetor of UserPost. See the docs related to vetors and arrays to understand the syntax
let post = v[4];
post.userId;
```

it can be represented instead such as
```
struct UserPostVec {
  userId: []id,
  contentMessage: []string,
  likes: []u64
}

let v = UserPostVec {userId: [],contentMessage: [], likes: []};
let post = 4;
v.userId[post]
```
The main idea around the lanes is to be a syntax sugar that acts like what i've said earlier, an array/vector of some type T. So the idea is that a `let p = laneval[idx]` actually never ever takes borrowing of that content
instead, be a handle for it. So it is very easy to read/write since you never acquire an aliasing pointer to it for too long(Rust's &mut), but instead, acquire it only when you do some write, and after that write, it's free
to use again.

## Some difficulties
When working with a lane, the handle that it generates might not be necessarily an index, in case, a direct one. This will have to be explicitly said because if for example the lane might delete stuff on the mid, and not only
on the end of the vector(supposing a vector), copying the content will suck and the indices will break, so we can use a slotmap to handle so. So some syntax such as

```
@IndexLogic(slotmap)
lane UserPost {}
````
will be required. If not providing the index logic as a slot map, the indices might be able to break and cause bugs. This is intentional by the way.
Another thing is that handles, as said earlier do not acquire aliasing pointers, so you are(in theory) able to have N mutating pointers at the same time, which goes against the idea of single writer the language idealizes,
but because it simply isn't in fact a writer, but rather simply an ID. that's literally what it is, an ID which you can use.

Since these IDs can be passed and cheap, it is NOT the language responsability to prevent that one uses the ID of one UserPost lane into another UserPost. It's be design highly tied to the lane that generated it. A way to prevent so
is to make the usage of the handle in other function whose signatures have mutability for some UserPost lane, since the compiler cannot prove that the given lane is the lane that generated that handle.
This is just an idea that is possible, but not an idea that I personally pretend adding. If the language gets some community and it asks for this, then alright. 
