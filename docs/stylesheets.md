# Stylesheets

Stylesheets are functions that generate some styles. Differently of CSS which is mainly static, and not
able to make reuse of code, thus, classes, ids, etc. Styles are intended to be run at runtime and generate it there.
The main reason for so it's because there is no way to generate styles ahead of time. I've personally worked with skia, opengl, and wgpu(even though not so advanced) and the idea of precompiled styles seems to be
some kind of joke for me, because it simply does not work like that. You simply MUST pass runtime data to the gpu so it can properly render the style. Based on it, the most logical thing for me was to make styles
work at runtime.

## Possible problems
For sure there are problems with this approach, for example, the incapacity of using css. This is, by now, a thing but idealized to be solved later with macros.

Another problem is that now there's runtime to generate styles. Which may not be so familiar to a lot of JS devs(which are being targetted by now) because of the usage of tailwind.
Depending on what kind of logic you are doing on the stylesheet, it yes might be bad for performance, but I really don't think this is a language responsability.

## Benefits
The benefits of styles being defined at runtime is that they now can be dynamic and reative, such as anything that'd interact with the language. So it becomes easier to generate an style that depends on some
property for example, and so change that style accordingly.

Another benefit that i've been thinking on is the usage of some kind of inheritance of styles, thus, make an style A apply style B.

There's also the benefit of the styles being type safe, so you cannot simply pass a color to a padding.

## How it works
In slynx you'd probably write some shit like the following:
```slx
stylesheet Bg(color: int) uses Fg(color | 0x00ff00) {
	let c = (color & 0xff) << 8;
	styles {
		backgroundColor: color,
		foregroundColor: c,
	}
}
stylesheet Fg(color: int) {
	styles {
		foregroundColor: color
	}
}

component Pedrinho {
	Div {
		style: Bg(12),
		Text {
			text: "Pedrinho leitazedo"
		}
	}
}

component Jorgin {
    pub prop color:int;
	Text {
	    style: Fg(color),
		text: "Jorginho neguinho"
	}
	Pedrinho {}
}

func main(): Component {
	let a = 5;
	let b = 12;
	let c = a + b;
	Jorgin {
        color: 0
	}
}
```
which is the code of example 'examples/styles/property_as_param.slx'.
What this does is to create 2 main styles, Fg and Bg. Fg simply sets the foreground color to be the given param, so Fg(0xff0000), according to RGB, sets the foreground color to be 'red' in css. The Bg in case, has the clause
'uses' which says that the Bg style applies the 'Fg' style with that params. Thus 'uses Fg(color | 0x00ff00)' is saying that Bg will apply FIRST, Fg, passing to it, as parameter, color | 0x00ff00, which means, the given color
with Green at max. So when passing for example: Bg(0xff0000), it applies Fg(0xffff00), which is rgb(255,255,0). The main thing is that inside bg, it also writes on 'foregroundColor', thus the 'foregroundColor' inside the 'bg' scope
will overwrite the 'foregroundColor' that was previously applied by 'Fg'. It is yes redundant, but that is the logic and 100% intentional. This evicts the problem of how to apply the style if theres already a style that
uses that property, so, we override.

The main intention for this was to make it easier to simply create an style library. You simply make something like:
```slx
stylesheet SomeStyle(arg1: T, arg2: V, arg3: K) {
  ...
}
```

and the one using your library simply does
```slx
stylesheet AnotherStyle() uses SomeStyle(...) {}
```
and boom it works.

### Multiple usages
In the styles examples you may see only examples containing one 1 usage, but in fact it does support more than 1, which follows the given:
```slx
stylesheet D() uses A(5), B(12), C {
  
}
```
What it does is to apply A(5), then B(12) then C(), and after them all, the styles inside D. Its literally something like in js:
function D(){
  const out = {};
  A(out, 5);
  B(out,12);
  C(out);
  //shit of D
  return out;
}
The lowering phase is not exactly this, but its more over like this the idea. 
Observation: Only 'C' by its own is not accepted, but the idea is that i change the parsing and make it accept and become the same as `C()`

## And CSS?
Im starting to think in a way to support macros in a powerful way and not so painful such as rust. So the idea is to make the macro good enough to the point where you can copy paste css and turn it into stylesheet.

### And Tailwind?
No. I got no idea of how to start to support that. Seriously. Just no.

## Limitations
By now it just support 2 styles, which are backgroundColor and foregroundColor. The idea is to start supporting more styles when the std starts to be initialized. So we can put semantics into it.

Since the language is not an UI dsl, but rather a language intended to make both backend and frontend of some application(initially, desktop), I think that inserting types such as 'pixel', 'color', 'rem', 'gradient'
and this kind of stuff is NOT good for the language on the long run, for obvious reasons: i dont think no one will use 'pixel' and 'color' on the backend. So its better we remove the boilerplate and dificulties it'd
generate to be implemented, and make it part of std, so the focus of the compiler is improving what has to be done. Even though there might be a way to make the stylesheet expect these types that come from the std.

A list of what is going to be implemented on the future can be found at crates/ir/STYLES_TABLE.md, and in general docs related to the IR

## Future Implementations
To make the stylesheets more 'safe' they will later require to be pure. So no dependency on things that are outer of the stylesheet scope are allowed. Thus it will be a MUST that the stylesheet rely purely on the
paremeter it is given.

### States
Currently the states on the stylesheets are sintactically complete(in theory), and semantically with some feets on the path, the thing is that their lowering isn't yet implemented.
The main idea behind them is the following:
```
stylesheet S(){
  styles {
    backgroundColor: red,
    hover(0.2s) {
      backgroundColor: blue,
      click.disabled {
        backgroundColor: grey
      }
    }
  }
}
```

The idea is that 'hover(0.2s)' means that, when hovering the component using the style, it transitions the old state, thus, backgroundColor: red, to backgroundColor: blue, during 0.2 seconds. A second parameter can be given
and it's the curve of the transition.

When doing so, new states as possible, thus 'click.disabled'. Due to problems with not knowing how to diferentiate state `click:disabled(0.2s) {}` from `click: disabled(0.2s)` `property: expression`, i've chosen to use dots instead.
So it means that on hover, if click or disabled, turns the color to be grey. The thing is that it's got no transition.
And thats it. 
