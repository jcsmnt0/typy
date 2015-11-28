# typy
is a static typechecker for Python 3's type annotations. It works via symbolic execution of Python bytecode, which isn't terribly practical but has provided me with a good excuse to get familiar with Python bytecode. It might someday be PEP 0484 compatible. If you want a practical/complete/robust Python typechecker, you should check out [mypy](http://mypy-lang.org/ "mypy"), which typy is heavily inspired by.

# How to use
`from typy.typecheck import typecheck`, then decorate your classes or functions with the `@typecheck()` decorator. Optionally, `from typy.classes import *` to get access to some base type definitions. Typechecking happens at declaration time, so the easiest way to run the checker is just to run your decorated .py file.

Check tests.py for example uses, bearing in mind that it uses `@testTypecheck` where client code should instead use `@typecheck`.
