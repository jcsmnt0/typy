import inspect
import types
from collections import namedtuple
from functools import reduce
from random import choice

def firstNonNone(*xs):
    return next((x for x in xs if x is not None), None)

# just for consistency, and to distinguish between using these as types vs. as constructors/conversion functions
Object = object
Boolean = bool
Integer = int
Float = float
Complex = complex
String = str
Object = object
List = list
Dict = dict
Void = type(None)

class UnknownValue:
    def __init__(self, t):
        self._type = t

class TypeVarContainerMeta(type): pass

def isGenericContainer(type):
    return isinstance(type, TypeVarContainerMeta)

class FunctionMeta(TypeVarContainerMeta):
    def __init__(cls, name, bases, fields):
        cls.types = cls.paramTypes + [cls.returnType]

    def __instancecheck__(cls, f):
        type = inferredType(f)
        return bool(type) and issubclass(type, cls)

    def __subclasscheck__(cls, other):
        if not isinstance(other, FunctionMeta):
            return False

        if len(cls.paramTypes) != len(other.paramTypes):
            return False

        for clsType, otherType in zip(cls.paramTypes, other.paramTypes):
            isSubclass = issubclass(clsType, otherType)
            isGenericSubclass = isinstance(clsType, TypeVarMeta) and issubclass(otherType, clsType.constraint)

            if not (isSubclass or isGenericSubclass):
                return False

        # return types are contravariant
        if not (isinstance(other.returnType, TypeVarMeta) or issubclass(other.returnType, cls.returnType)):
            return False

        return True

    def __eq__(cls, other):
        return (isinstance(other, FunctionMeta) and
            len(cls.paramTypes) == len(other.paramTypes) and
            all(x == y for x, y in zip(cls.paramTypes, other.paramTypes))
            and cls.returnType == other.returnType)

    def __str__(cls):
        return '{0} -> {1}'.format(
            str(cls.paramTypes[0] if len(cls.paramTypes) == 1 else cls.paramTypes),
            str(cls.returnType))

    def __repr__(cls):
        return str(cls)

    def __hash__(cls):
        return hash(tuple(cls.types))

def Function(paramTys = [], returnTy = Object):
    class Function(object, metaclass = FunctionMeta):
        paramTypes = paramTys
        returnType = returnTy

    return Function

def stripSelfParameter(functionType):
    return Function(functionType.paramTypes[1:], functionType.returnType)

def isPositional(arg):
    return arg._kind in [inspect.Parameter.POSITIONAL_OR_KEYWORD, inspect.Parameter.POSITIONAL_ONLY]

def inferredFunctionType(f):
    if isinstance(f, type):
        f = f.__new__ # todo: __new__ vs. __init__

    if isinstance(f, Callable):
        sig = inspect.signature(f)

        argTypes = []
        for t in (p.annotation for p in sig.parameters.values() if isPositional(p)):
            argTypes.append(t if t != inspect._empty else Object)

        returnType = sig.return_annotation

        if returnType != inspect._empty:
            return Function(argTypes, returnType)
        else:
            return Function(argTypes)

    return None

def inferredClassType(obj):
    return type if isinstance(obj, type) else None

def inferredType(obj, overrides = {}):
    return firstNonNone(
        overrides.get(obj),
        getattr(obj, '_type', None),
        inferredClassType(obj),
        inferredFunctionType(obj),
        type(obj))

def typeInferrer(overrides = {}):
    def infer(obj): return inferredType(obj, overrides)
    return infer

class Self(type):
    pass

class Infer(type):
    pass

class UnionMeta(type):
    def __instancecheck__(cls, obj):
        return issubclass(type(obj), cls)

    def __subclasscheck__(cls, other):
        if isinstance(other, UnionMeta):
            return all(issubclass(x, cls) for x in other.types)
        else:
            return any(issubclass(other, t) for t in cls.types)

    def __str__(cls):
        return 'Union(' + ', '.join(map(str, cls.types)) + ')'
    
    # only for the purposes of typechecking!!
    def __getattribute__(cls, attr):
        if attr in ('types', '__class__'):
            return type.__getattribute__(cls, attr)
        else:
            ts = [getattr(t, attr) for t in cls.types]

            # make sure all of the unioned types have this attribute with the same type
            if len({inferredType(t) for t in ts}) != 1:
                raise AttributeError(cls, attr)

            # return a random one
            # todo: return a closure that dispatches on type instead
            return choice(ts)

def Union(*ts):
    class Union(object, metaclass = UnionMeta):
        types = ts

    return Union

Number = Union(Integer, Float, Complex)

Callable = Union(
    type,
    types.MethodType,
    types.LambdaType,
    types.FunctionType,
    types.BuiltinFunctionType,
    types.BuiltinMethodType,
    type(id.__call__), # method-wrapper
    type(int.__init__)) # wrapper_descriptor

class IntersectionMeta(type):
    def __instancecheck__(cls, obj):
        return issubclass(type(obj), cls)

    def __subclasscheck__(cls, other):
        # todo: overwrite type to have its __subclasscheck__ account for Union and Intersection,
        # then these ifs don't have to exist anymore
        if isinstance(other, IntersectionMeta):
            return all(any(issubclass(u, t) for t in cls.types) for u in other.types)
        else:
            return any(issubclass(other, t) for t in cls.types)

    def __str__(cls):
        return 'Intersection(' + ', '.join(map(str, cls.types)) + ')'

    # only for the purposes of typechecking!!
    def __getattribute__(cls, attr):
        if attr == 'types':
            return type.__getattribute__(cls, attr)
        else:
            for t in cls.types:
                if hasattr(t, attr):
                    return getattr(t, attr)

            raise AttributeError(cls, attr)

def Intersection(*ts):
    class Intersection(object, metaclass = IntersectionMeta):
        types = ts

    return Intersection

class TupleMeta(type):
    def __instancecheck__(cls, xs):
        return (isinstance(xs, tuple) and
            len(cls.types) == len(xs) and
            all(map(isinstance, xs, cls.types)))

    def __subclasscheck__(cls, other):
        return (isinstance(other, TupleMeta) and
            len(cls.types) == len(other.types) and
            all(map(issubclass, other.types, cls.types)))

    def __str__(cls):
        return '(' + ', '.join(map(str, cls.types)) + ')'

def Tuple(*ts):
    class Tuple(object, metaclass = TupleMeta):
        types = ts

    return Tuple

class ListMeta(type):
    def __instancecheck__(cls, xs):
        return isinstance(xs, list) and all(isinstance(x, cls.type) for x in xs)

    def __subclasscheck__(cls, other):
        return isinstance(other, ListMeta) and issubclass(other.type, cls.type)

    def __str__(cls):
        return 'List(' + str(cls.type) + ')'

def TypedList(t):
    class List(object, metaclass = ListMeta):
        type = t

    return List

class InterfaceMeta(type):
    def __instancecheck__(cls, obj):
        return issubclass(type(obj), cls)

    def __subclasscheck__(cls, other):
        if isinstance(other, InterfaceMeta):
            return all(issubclass(other.members[name], cls.members[name]) for name in cls.members.keys())
        else:
            return all(issubclass(type(dict(inspect.getmembers(other)).get(name, None)), t)
                for name, t
                in cls.members.items())

    def __getattribute__(cls, attr):
        if attr in type.__getattribute__(cls, 'members').keys():
            return UnknownValue(cls.members[attr])
        else:
            return type.__getattribute__(cls, attr)

def Interface(**ms):
    class Interface(object, metaclass = InterfaceMeta):
        members = ms

    return Interface

Callable = Interface(__call__ = Callable)

class TypeVarMeta(type):
    def __str__(cls):
        return cls.name

    def __repr__(cls):
        return "TypeVar(" + cls.name + ")"

    def __subclasscheck__(cls, other):
        return not cls.constraint or issubclass(other, cls.constraint)

    def __getattribute__(cls, attr):
        # there's probably a better way to do this?
        if attr in ['name', 'constraint']:
            return type.__getattribute__(cls, attr)
        else:
            return getattr(cls.constraint, attr)

def isGeneric(ty, scopeGenerics = ()):
    return isinstance(ty, TypeVarMeta) and type not in scopeGenerics

def isConcrete(ty, scopeGenerics = ()):
    return not isGeneric(ty, scopeGenerics)

def TypeVar(varName, varConstraint = Object):
    class TypeVar(object, metaclass = TypeVarMeta):
        constraint = varConstraint
        name = varName

    return TypeVar

def TypeVars(*names):
    return map(TypeVar, names)
