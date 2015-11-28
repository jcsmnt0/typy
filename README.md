# typy
is a static typechecker for Python 3. It works via symbolic execution of Python bytecode, which isn't terribly practical but has provided me with a good excuse to get familiar with Python bytecode. It might someday be [PEP 0484](https://www.python.org/dev/peps/pep-0484/ "PEP 0484") compliant but at the moment it's distinctly not. If you want a practical/complete/robust Python typechecker, you should check out [mypy](http://mypy-lang.org/ "mypy") (no affiliation).

# How to use
`from typy.typecheck import typecheck`, then decorate your classes or functions with the `@typecheck()` decorator. Optionally, `from typy.classes import *` to get access to some base type definitions. Typechecking happens at declaration time, so the easiest way to run the checker is just to run your decorated .py file.

Check tests.py for examples, bearing in mind that it uses `@testTypecheck` where client code should instead use `@typecheck`.

# Typechecking functions
Functions can be decorated with argument and return types with Python 3's annotation syntax.

```
@typecheck()
def intToString(x: int) -> str:
    return str(x)
```

# Typechecking classes
All the methods of a decorated class get typechecked. Since the identifier referring to a class isn't available within the class's own definition, there's a special `Self` identifier to refer to the class currently being defined.

```
@typecheck()
class FakeSingleton:
    def __new__(self: Self) -> Self:
        return self
    
    def __eq__(self: Self, other: Self) -> bool:
        return True
```

When a class's constructor is called, typy looks to the `__new__` magic method to figure out its type. `__init__` isn't supported yet but is probably next on my todo list.

```
@typecheck()
def makeFakeSingleton() -> FakeSingleton:
    return FakeSingleton()
```

# Function types
`typy.classes.Function` creates function types. It takes two arguments: a list of argument types, and a return type. It doesn't currently support keyword arguments or variadic functions.

```
@typecheck()
def compareInts(cmp: Function([int, int], bool), x: int, y: int) -> bool:
    return cmp(x, y)
```

# Generic types
`typy.classes.TypeVar` creates a type variable given a string to use as a name; the type variable can then be used in type annotations as a universally quantified generic type.

```
a = TypeVar('a')

@typecheck
def id(x: a) -> a:
    return x

def testId() -> str:
    return id("test")
```

`typy.classes.TypeVars` is a convenience method for creating multiple generic type variables at once.

```
a, b, c = TypeVar('a', 'b', 'c')

@typecheck
def s(f: Function([a, b], c), g: Function([a], b), x: a) -> c:
    return f(x, g(x))
```

# Miscellanea
I find it kind of awkward to overload e.g. `int` to represent a constructor/conversion function in some contexts and a type in others; if you happen to agree, you'll be happy to know that `typy.classes` provides capital-I `Integer` and a few friends as aliases for Python's built-in type objects so you can use `int` for converting things to integers and `Integer` as a type. You're not required to use the aliases, though.

The `@typecheck` decorator takes some optional keyword arguments:
* `identifierTypes` is a dict mapping identifier names (strings) to types, used to give types to variables that you expect to be in scope when your function is called. It's empty by default.
* `overrides` is a dict mapping values to types, used to override the types of things that are already defined. It's populated with typings for some standard library functions by default.
* `errFile` is the file to print type errors to, and defaults to `sys.stderr`.
* `logFile` is the file to print logging information to (for my own debugging purposes, but you're welcome to use it if you want). It defaults to `None`, which suppresses logging.

# Caveat
typy can't presently read Python stub files, so typings for the standard library are hand-generated and woefully incomplete. The whole project is very much a work in progress and most likely can't yet typecheck any serious programs; in particular, generic containers aren't fully supported yet, so you're on your own for lists/tuples/dicts/etc.
