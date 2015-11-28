from sys import stdout
from dis import get_instructions
from pprint import pprint

from typy.typecheck import *
from typy.classes import *

def testTypecheck(scopeTypes = {}, *expectedErrors):
    def typechecker(f):
        errors = []
        typecheck(scopeTypes, errFile = errors)(f)

        assert errors == list(expectedErrors)

        return f

    return typechecker

class TestCallable(object):
    def __call__(self):
        pass

def test_CallableMembershipChecks():
    assert not isinstance(True, Callable)
    assert isinstance(TestCallable(), Callable)
    assert isinstance(TestCallable().__call__, Callable)
    assert isinstance(TestCallable, Callable)
    assert isinstance(TestCallable.__call__, Callable)
    assert isinstance(lambda x, y: x, Callable)
    assert isinstance(test_CallableMembershipChecks, Callable)

    assert issubclass(TestCallable, Callable)

def test_UnionMembershipChecks():
    assert issubclass(Integer, Number)
    assert issubclass(Complex, Number)

    assert isinstance(3, Number)
    assert isinstance(True, Number)

def test_FunctionMembershipChecks():
    class Base(object): pass
    class Derived(Base): pass

    def f(x: Base, y: Base) -> Derived: pass

    assert isinstance(f, Function([Base, Base], Derived))
    assert isinstance(f, Function([Base, Derived], Derived))
    assert isinstance(f, Function([Derived, Derived], Base))

    assert issubclass(Function([Base], Derived), Function([Derived], Derived))
    assert issubclass(Function([Base], Derived), Function([Base], Base))

    assert isinstance(lambda x: x, Function([Object], Object))
    assert isinstance(lambda x: x, Function([Integer], Object))

def test_inferFunctionTypes():
    def f(x: Integer, y) -> Number: pass
    def g(x, y: Boolean): pass

    assert inferredFunctionType(f) == Function([Integer, Object], Number)
    assert inferredFunctionType(g) == Function([Object, Boolean])

def test_assignments():
    @testTypecheck({'a': Integer, 'b': Number})
    def noErrors(x: String, y):
        a = 3
        b = 4.0
        x = "hello"
        y = (1, "two", 3.9)
        y = a

    @testTypecheck({}, AssignmentTypeMismatch(refName = 'x', refType = Boolean, valType = Integer))
    def badArgumentAssignment(x: Boolean):
        x = 3

    @testTypecheck({}, AssignmentTypeMismatch(refName = 'x', refType = Float, valType = String))
    def badArgumentReassignment(x: Float):
        x = 3.0
        x = "error"

    @testTypecheck({'a': Boolean}, AssignmentTypeMismatch(refName = 'a', refType = Boolean, valType = Integer))
    def badScopeVariableAssignment():
        a = 3

    @testTypecheck({'a': String}, AssignmentTypeMismatch(refName = 'a', refType = String, valType = Complex))
    def badScopeVariableReassignment():
        a = "test"
        a = 3 + 2j

def test_casts():
    @testTypecheck()
    def f(x: Integer, y: Boolean) -> String:
        x = cast(Integer)("3")
        y = cast(Boolean)(x)
        return cast(String)(y)

def test_ifStatements():
    @testTypecheck()
    def branchOnValue(x: Boolean) -> Integer:
        if x:
            return 1
        else:
            return 0

    @testTypecheck()
    def branchOnComparison(x: Integer) -> Boolean:
        if x > 0:
            return True
        else:
            return False

    # todo: only the first error in the function is reported; it should be the first error in each branch
    @testTypecheck({}, ReturnTypeMismatch(valType = Integer, returnType = String))
    def errorInIfBranch(x: Boolean) -> String:
        if x:
            return 3
        else:
            return "test"

    @testTypecheck({}, ReturnTypeMismatch(valType = Integer, returnType = String))
    def errorInElifBranch(x: Boolean, y: Boolean) -> String:
        if x:
            return "test"
        elif y:
            return 8
        else:
            return "test"

    @testTypecheck({}, ReturnTypeMismatch(valType = Integer, returnType = String))
    def errorInElseBranch(x: Boolean, y: Boolean) -> String:
        if x:
            return "test"
        elif y:
            return "test"
        else:
            return 9

def test_functionCalls():
    def test(x: Integer) -> String:
        return x

    # typechecked functions can call ones that haven't been checked as long as the signature of the callee checks out
    @testTypecheck()
    def goodCall(x: Integer) -> String:
        return test(x)
    
    @testTypecheck({}, ArgumentTypeMismatch(argType = String, paramType = Integer))
    def badArgument(x: Integer) -> String:
        return test(str(x))

    @testTypecheck({}, ReturnTypeMismatch(valType = String, returnType = Boolean))
    def badReturn(x: Integer) -> Boolean:
        return test(x)

def test_generics():
    a, b, c = TypeVars('a', 'b', 'c')

    @testTypecheck()
    def apply(f: Function([a], b), x: a) -> b:
        return f(x)

    @testTypecheck()
    def id(x: a) -> a:
        return x

    @testTypecheck()
    def goodApplyId() -> Integer:
        return apply(id, 3)

    @testTypecheck({}, ReturnTypeMismatch(valType = String, returnType = Integer))
    def badApplyId() -> Integer:
        return apply(id, "test")

    @testTypecheck()
    def goodApply(f: Function([b], a), x: b) -> a:
        return f(x)

    @testTypecheck()
    def intToString(x: Integer) -> String:
        return cast(String)(str(x))

    @testTypecheck()
    def s(f: Function([a, b], c), g: Function([a], b), x: a) -> c:
        return f(x, g(x))

    @testTypecheck()
    def foo(x: String, y: Boolean) -> Number:
        return int(x) + int(y)
    
    @testTypecheck()
    def bar(x: String) -> Boolean:
        return bool(x)

    @testTypecheck()
    def goodUse() -> Number:
        return s(foo, bar, "b")

    @testTypecheck(
        {},
        ArgumentTypeMismatch(Function([String], Boolean), Function([a, b], c)),
        ArgumentTypeMismatch(Function([String, Boolean], Number), Function([a], b)))
    def badUse() -> String:
        return s(bar, foo, 3)

def test_classes():
    @testTypecheck()
    class Foo:
        def __new__(self: Self) -> Self:
            return self

        def __add__(self: Self, other: Integer) -> Boolean:
            return cast(Boolean)(3 + other)

    @testTypecheck({}, ReturnTypeMismatch(Number, Boolean))
    class BadFoo:
        def __new__(self: Self) -> Self:
            return self

        def __add__(self: Self, other: Integer) -> Boolean:
            return 3 + other

    @testTypecheck()
    def goodAdd() -> Boolean:
        return Foo() + 3

    @testTypecheck({}, ArgumentTypeMismatch(paramType = Integer, argType = String))
    def badAdd() -> Boolean:
        return Foo() + "nope"

    @testTypecheck()
    class Bar:
        def __new__(self: Self, one: Foo, two: BadFoo) -> Self:
            return self

    @testTypecheck()
    def goodConstructorCall() -> Bar:
        return Bar(Foo(), BadFoo())

    @testTypecheck({}, ArgumentTypeMismatch(Foo, BadFoo))
    def badConstructorCall() -> Bar:
        return Bar(Foo(), Foo())

    @testTypecheck()
    class GoodRecursiveCall:
        def goodCall(self: Self, x: String): goodCall(self, x)

    @testTypecheck({}, ArgumentTypeMismatch(Integer, String))
    class BadRecursiveCall:
        def badCall(self: Self, x: String): badCall(self, 3)

    @testTypecheck()
    def goodInterfaceCall(x: Interface(f = Function([Boolean, String], Integer))) -> Integer:
        return x.f(True, "eurT")

    @testTypecheck({}, ArgumentTypeMismatch(Boolean, String))
    def badInterfaceCall(x: Interface(f = Function([Boolean, String], Integer))) -> Integer:
        return x.f(True, False)

if __name__ == "__main__":
    # cheap hack to call all top-level functions whose names start with "test"
    for k, v in list(globals().items()):
        if k.startswith("test"):
            v()
