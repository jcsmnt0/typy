import dis
import re
import sys
from pprint import pprint
from functools import reduce, partial
from collections import namedtuple, defaultdict, deque

from typy.classes import *

def extend(*xs):
    x = xs[0].copy()
    for y in xs[1:]:
        x.update(y)
    return x

def push(stack, *xs):
    for x in xs:
        stack.append(x)

def popN(stack, count):
    return [stack.pop() for _ in range(count)]

Context = namedtuple('Context', ['scopeVals', 'scopeTypes', 'signature'])
Symbol = namedtuple('Symbol', ['type', 'value'])

def hasValue(symbol):
    if symbol.value is None:
        return symbol.type is type(None)
    else:
        return type(symbol.value) is not UnknownValue

class TypecheckerError(object):
    def __eq__(self, other):
        return str(self) == str(other) # this is pretty hacky

    def __repr__(self):
        args = {
            arg: repr(getattr(self, arg))
            for arg
            in dir(self)
            if not arg.startswith("__")}

        return self.__class__.__name__ + repr(args)

class AssignmentTypeMismatch(TypecheckerError):
    def __init__(self, refName, refType, valType):
        self.refName = refName
        self.refType = refType
        self.valType = valType

    def __str__(self):
        return 'mismatch! tried to store {0} in ({1}: {2})'.format(
            self.valType, self.refName, self.refType)

class ReturnTypeMismatch(TypecheckerError):
    def __init__(self, valType, returnType):
        self.valType = valType
        self.returnType = returnType

    def __str__(self):
        return 'mismatch! tried to return {0}, but return type should be {1}'.format(
            self.valType, self.returnType)

class ArgumentTypeMismatch(TypecheckerError):
    def __init__(self, argType, paramType):
        self.argType = argType
        self.paramType = paramType
        
    def __str__(self):
        return 'mismatch! tried to pass {0} as {1}'.format(self.argType, self.paramType)

class BadCall(TypecheckerError):
    def __init__(self, funcType):
        self.funcType = funcType

    def __str__(self):
        return 'tried to call object of type {0} as a function'.format(self.funcType)

class ArgumentListLengthMismatch(TypecheckerError):
    def __init__(self, args, paramTypes, expectedLength, actualLength):
        self.args = args
        self.paramTypes = paramTypes
        self.expectedLength = expectedLength
        self.actualLength = actualLength

    def __str__(self):
        return 'tried to call function with {0} arguments {1}, but it expects {2} {3}'.format(
            self.actualLength,
            self.args,
            self.expectedLength,
            self.paramTypes)

class UndefinedReference(TypecheckerError):
    def __init__(self, name):
        self.name = name
        
    def __str__(self):
        return "{0} isn't defined, and a type wasn't declared for it".format(self.name)

class UndefinedMethod(TypecheckerError):
    def __init__(self, type, methodName):
        self.type = type
        self.methodName = methodName

    def __str__(self):
        return "{0} doesn't have method {1}".format(self.type, self.methodName)

class UndefinedAttribute(TypecheckerError):
    def __init__(self, obj, type, attr):
        self.obj = obj
        self.type = type
        self.attr = attr

    def __str__(self):
        return "{0} (of type {1}) doesn't have attribute {2}".format(repr(self.obj), self.type, self.attr)

class GenericTypeConflict(TypecheckerError):
    def __init__(self, genericType, concreteTypes):
        self.genericType = genericType
        self.concreteTypes = concreteTypes

    def __str__(self):
        return "generic type {0} is instantiated to conflicting concrete types: {1}".format(
                self.genericType,
                ", ".join(self.concreteTypes))

class UninstantiatedReturnType(TypecheckerError):
    def __init__(self, genericType):
        self.genericType = genericType

    def __str__(self):
        return "generic return type {0} can't be inferred from the arguments".format(self.genericType)

def fail(context, symbols, inst, infer):
    raise NotImplementedError(inst.opname)

instructionHandlers = defaultdict(lambda: fail)

def instructionHandler(name):
    def f(handler):
        instructionHandlers[name] = handler
        return handler

    return f

@instructionHandler('NOP')
@instructionHandler('JUMP_FORWARD')
def nop(context, symbols, inst, infer):
    pass

@instructionHandler('POP_TOP')
def popTop(context, symbols, inst, infer):
    symbols.pop()

@instructionHandler('ROT_TWO')
def rotTwo(context, symbols, inst, infer):
    x, y = popN(symbols, 2)
    push(symbols, x, y)

@instructionHandler('ROT_THREE')
def rotThree(context, symbols, inst, infer):
    x, y, z = popN(symbols, 3)
    push(symbols, x, z, y)

@instructionHandler('DUP_TOP')
def dupTop(context, symbols, inst, infer):
    x = symbols.pop()
    push(symbols, x, x)

@instructionHandler('DUP_TOP_TWO')
def dupTopTwo(context, symbols, inst, infer):
    x, y = popN(symbols, 2)
    push(symbols, x, y, x, y)

# todo: freshen names also
def freshen(ty, scopeGenerics):
    return TypeVar(ty.name, ty.constraint) if isGeneric(ty, scopeGenerics) else ty

# checks that each generic type in the graph points to at most one concrete type,
# and builds a dictionary mapping generic types to concrete types
def checkTypeEqualityGraph(typeEqualities, scopeGenerics):
    errors = []
    concreteTypeMap = {}

    for genericType, equalTypes in typeEqualities.items():
        queue = deque(t for t in equalTypes if isGeneric(t, scopeGenerics))
        seen = []
        concreteTypes = set(t for t in equalTypes if isConcrete(t, scopeGenerics))

        while queue:
            ty = queue.popleft()
            seen.append(ty)

            while ty in queue:
                queue.remove(ty)

            for equalType in typeEqualities[ty]:
                if isGeneric(equalType, scopeGenerics):
                    if equalType not in seen:
                        queue.append(equalType)
                else:
                    concreteTypes.add(equalType)
        
        if len(concreteTypes) > 1:
            errors.append(
                # todo: this is too general to be really useful as an error;
                # include argument names and stuff
                GenericTypeConflict(
                    genericType = genericType,
                    concreteTypes = concreteTypes))

        if len(concreteTypes) > 0:
            # this might cause confusing errors if the graph isn't consistent,
            # but at least in that case there will already be other errors that
            # point to the real problem
            concreteTypeMap[genericType] = concreteTypes.pop()

    return (concreteTypeMap, errors)

@instructionHandler('CALL_FUNCTION')
def callFunction(context, symbols, inst, infer, keywordArgCount = None, positionalArgCount = None):
    keywordArgCount = keywordArgCount or (inst.arg >> 8 if inst.arg else 0)
    positionalArgCount = positionalArgCount or (inst.arg & 0xff if inst.arg else 0)

    keywordArgList = popN(symbols, keywordArgCount * 2)
    keywordArgs = tuple(zip(keywordArgList[::2], keywordArgList[1::2]))

    positionalArgs = tuple(reversed(popN(symbols, positionalArgCount)))

    func = symbols.pop()

    signature = func.type

    # constructors
    if issubclass(signature, type):
        # todo: __init__
        if func.value:
            signature = stripSelfParameter(infer(func.value.__new__))
            signature.returnType = func.value
            func = Symbol(value = func.value.__new__, type = signature)
        else:
            # classes created impurely don't get good typechecking (todo?)
            push(symbols, Symbol(value = None, type = Object))
            return

    if not isinstance(signature, FunctionMeta):
        return [BadCall(signature)]

    positionalParams = signature.paramTypes
    scopeGenerics = filter(isGeneric, positionalParams)

    positionalArgs = tuple(freshen(ty, scopeGenerics) for ty in positionalArgs)
    positionalArgCount = len(positionalArgs)
    positionalParamCount = len(positionalParams)

    # todo: kwargs

    if positionalArgCount != positionalParamCount:
        return [ArgumentListLengthMismatch(
            args = positionalArgs,
            paramTypes = positionalParams,
            expectedLength = positionalParamCount,
            actualLength = positionalArgCount)]

    genericContext = set()
    for paramType in signature.paramTypes:
        if isGeneric(paramType, scopeGenerics):
            genericContext.add(paramType)
        elif isGenericContainer(paramType):
            genericContext.update(ty for ty in paramType.types if isGeneric(ty, scopeGenerics))

    # positionalArgs = [Symbol(value = sym.value, type = freshen(sym.type)) for sym in positionalArgs]

    errors = []
    typeEqualities = defaultdict(lambda: [])
    
    def addTypeEquality(paramType, argType):
        if isGeneric(paramType, scopeGenerics):
            typeEqualities[paramType].append(argType)

        if isGeneric(argType, scopeGenerics):
            typeEqualities[argType].append(paramType)

    for argSymbol, paramType in zip(positionalArgs, signature.paramTypes):
        argType = argSymbol.type

        # generics form a graph connected by equalities
        # build up a list of equalities, then check that they're consistent
        # consistent means: each generic points transitively to at most one concrete value

        addTypeEquality(paramType, argType)

        if (isinstance(paramType, TypeVarContainerMeta) and isinstance(argType, TypeVarContainerMeta)):
            if issubclass(argType, paramType):
                for paramGeneric, argGeneric in zip(paramType.types, argType.types):
                    addTypeEquality(paramGeneric, argGeneric)
            else:
                errors.append(ArgumentTypeMismatch(paramType = paramType, argType = argType))
        elif not issubclass(argType, paramType): 
            errors.append(ArgumentTypeMismatch(paramType = paramType, argType = argType))

    genericMap, genericTypeErrors = checkTypeEqualityGraph(dict(typeEqualities), scopeGenerics)
    errors += genericTypeErrors

    if errors:
        return errors

    if hasValue(func) and getattr(func.value, '_pure', False):
        returnValue = func.value(*(arg.value for arg in positionalArgs))
        returnType = infer(returnValue) if signature.returnType is Infer else signature.returnType
    else:
        returnValue = None
        returnType = signature.returnType

    if isGeneric(returnType, scopeGenerics):
        if returnType in genericMap:
            returnType = genericMap[returnType]

    push(symbols, Symbol(value = returnValue, type = returnType))

@instructionHandler('LOAD_CONST')
def loadConst(context, symbols, inst, infer):
    x = inst.argval
    push(symbols, Symbol(value = x, type = infer(x)))

@instructionHandler('LOAD_GLOBAL')
@instructionHandler('LOAD_FAST')
@instructionHandler('LOAD_DEREF')
def loadFromScope(context, symbols, inst, infer):
    name = inst.argval

    if name in context.scopeTypes:
        push(symbols, Symbol(value = None, type = context.scopeTypes[name]))
    elif name in context.scopeVals:
        value = context.scopeVals[name]
        push(symbols, Symbol(value = value, type = infer(value)))
    else:
        return [UndefinedReference(name)]

@instructionHandler('LOAD_ATTR')
def loadAttr(context, symbols, inst, infer):
    attrName = inst.argval
    obj = symbols.pop()

    if hasValue(obj) and hasattr(obj, attrName):
        # this should handle looking through the attribute dicts of obj's class and superclasses automatically
        value = getattr(obj.value, attrName)
        type = infer(value)
    elif hasattr(obj.type, attrName):
        value = getattr(obj.type, attrName)
        type = infer(value)
    elif attrName in context.scopeVals:
        value = context.scopeVals[attrName]
        type = infer(value)
    elif attrName in context.scopeTypes:
        type = context.scopeTypes[attrName]
        value = UnknownValue(type)
    else:
        return [UndefinedAttribute(obj, infer(obj), attrName)]

    push(symbols, Symbol(value = value, type = type))

# todo: x + y returns type(y).__radd__(y, x) if not hasattr(type(x), '__add__') or if it returns NotImplemented
def operator(methodName, arity, defaultType):
    def f(context, symbols, inst, infer):
        xs = tuple(reversed(popN(symbols, arity)))
        x = xs[0]

        if hasattr(x, methodName):
            method = getattr(x.type, methodName)
            methodType = infer(method) if hasattr(method, '__annotations__') else defaultType
            push(symbols, Symbol(value = method, type = methodType), *xs)
            return callFunction(context, symbols, inst, infer, positionalArgCount = arity)
        else:
            return [UndefinedMethod(methodName = methodName, type = x.type)]

    return f

instructionHandlers = extend(instructionHandlers, {
    'UNARY_POSITIVE': operator('__pos__', 1, Function([Object], Number)),
    'UNARY_NEGATIVE': operator('__neg__', 1, Function([Object], Number)),
    'UNARY_NOT': operator('__not__', 1, Function([Object], Boolean)),
    'UNARY_INVERT': operator('__invert__', 1, Function([Object], Number)),
    'GET_ITER': operator('__iter__', 1, Function([Object], Object)), # todo
    'BINARY_POWER': operator('__pow__', 2, Function([Object, Object], Number)),
    'BINARY_MULTIPLY': operator('__mul__', 2, Function([Object, Object], Number)),
    'BINARY_FLOOR_DIVIDE': operator('__floordiv__', 2, Function([Object, Object], Number)),
    'BINARY_TRUE_DIVIDE': operator('__div__', 2, Function([Object, Object], Number)),
    'BINARY_MODULO': operator('__mod__', 2, Function([Object, Object], Number)),
    'BINARY_ADD': operator('__add__', 2, Function([Object, Object], Number)),
    'BINARY_SUBTRACT': operator('__sub__', 2, Function([Object, Object], Number)),
    'BINARY_SUBSCR': operator('__getitem__', 2, Function([Object, Object], Object)),
    'BINARY_LSHIFT': operator('__lshift__', 2, Function([Object, Object], Number)),
    'BINARY_RSHIFT': operator('__rshift__', 2, Function([Object, Object], Number)),
    'BINARY_AND': operator('__and__', 2, Function([Object, Object], Boolean)),
    'BINARY_XOR': operator('__xor__', 2, Function([Object, Object], Boolean)),
    'BINARY_OR': operator('__or__', 2, Function([Object, Object], Boolean))})

@instructionHandler('STORE_FAST')
def storeFast(context, symbols, inst, infer):
    x = symbols.pop()

    refName = inst.argval
    refType = context.scopeTypes[refName]

    if not issubclass(x.type, refType):
        return [AssignmentTypeMismatch(refName=refName, refType=refType, valType=x.type)]

@instructionHandler('RETURN_VALUE')
def returnValue(context, symbols, inst, infer):
    x = symbols.pop()
    returnType = context.signature.returnType

    if not issubclass(x.type, returnType):
        return [ReturnTypeMismatch(valType=x.type, returnType=returnType)]

@instructionHandler('BUILD_TUPLE')
def buildTuple(context, symbols, inst, infer):
    xs = popN(symbols, inst.argval)
    push(symbols, Symbol(
        value = tuple(x.value for x in xs) if all(map(hasValue, xs)) else None,
        type = Tuple(x.type for x in xs)))

@instructionHandler('POP_JUMP_IF_FALSE')
@instructionHandler('POP_JUMP_IF_TRUE')
@instructionHandler('JUMP_IF_TRUE_OR_POP')
@instructionHandler('JUMP_IF_FALSE_OR_POP')
def popJump(context, symbols, inst, infer):
    symbols.pop()

comparisonMethodNameLookup = {
    '==': '__eq__',
    '!=': '__ne__',
    '<': '__lt__',
    '>': '__gt__',
    '<=': '__le__',
    '>=': '__ge__'}

@instructionHandler('COMPARE_OP')
def compareOp(context, symbols, inst, infer):
    op = operator(comparisonMethodNameLookup[inst.argval], 2, Function([Object, Object], Boolean))
    return op(context, symbols, inst, infer)

jumpInstructions = [
    'POP_JUMP_IF_FALSE',
    'POP_JUMP_IF_TRUE',
    'JUMP_IF_TRUE_OR_POP',
    'JUMP_IF_FALSE_OR_POP',
    'CONTINUE_LOOP']

def executionPaths(insts, startIndex = 0, seenIndices = frozenset(), offsetLookup = None):
    if startIndex in seenIndices:
        return []

    currentPath = []
    offsetLookup = offsetLookup or {inst.offset: index for index, inst in enumerate(insts)}

    # there should be a drop function for enumerators in the standard library probably
    for index, inst in list(enumerate(insts))[startIndex:]:
        seenIndices = seenIndices.union({index})
        currentPath.append(inst)

        if inst.opname in jumpInstructions:
            jumpIndex = offsetLookup[inst.argval]
            jumpPaths = executionPaths(insts, jumpIndex, seenIndices, offsetLookup)
            skipPaths = executionPaths(insts, index + 1, seenIndices, offsetLookup)

            conditionalPop = inst.opname in ['JUMP_IF_TRUE_OR_POP', 'JUMP_IF_FALSE_OR_POP']
            currentJumpPath = currentPath[:-1] if conditionalPop else currentPath
            completeJumpPaths = [currentJumpPath + path for path in jumpPaths]
            completeSkipPaths = [currentPath + path for path in skipPaths]

            return completeSkipPaths + completeJumpPaths
        elif inst.opname in ['JUMP_ABSOLUTE', 'JUMP_FORWARD']:
            jumpPaths = executionPaths(insts, offsetLookup[inst.argval], seenIndices, offsetLookup)
            return [currentPath + path for path in jumpPaths]
        elif inst.opname == 'RETURN_VALUE':
            return [currentPath]

    raise "a concern: there should have been a jump or a return at some point"

def typecheckClass(cls, scopeTypes = {}, infer = typeInferrer(), logFile = None):
    errors = []

    for key, value in cls.__dict__.items():
        if hasattr(value, '__annotations__'):
            for attrName, attr in value.__annotations__.items():
                if attr is Self:
                    value.__annotations__[attrName] = cls

            errors = errors + typecheckFunction(value, scopeTypes, infer, logFile)

    return errors

def typecheckFunction(func, scopeTypes = {}, infer = typeInferrer(), logFile = None):
    symbols = []

    # __builtins__ is sometimes a module and sometimes a dict?
    builtins = __builtins__ if type(__builtins__) == dict else __builtins__.__dict__

    if func.__closure__:
        closure = {val.__name__: val for val in (cell.cell_contents for cell in func.__closure__)}
    else:
        closure = {}

    context = Context(
        scopeVals = extend(
            func.__globals__,
            builtins,
            closure,
            {func.__name__: func}),
        # todo: scopeTypes should also include types from scopeVals that have _type
        # todo: actually should just be scopeSymbols instead of scopeVals and scopeTypes
        scopeTypes = extend(
            defaultdict(lambda: Object),
            scopeTypes,
            func.__annotations__,
            {func.__name__: infer(func)}),
        signature = infer(func))

    for path in executionPaths(list(dis.get_instructions(func))):
        for inst in path:
            handler = instructionHandlers[inst.opname]

            if logFile:
                pprint(symbols, stream=logFile)
                print(file=logFile) # newline
                print(inst, file=logFile)

            errors = handler(context, symbols, inst, infer)

            if errors:
                if logFile:
                    print(errors, file=logFile)

                return errors

    return []

def typecheck(types = {}, infer = typeInferrer(), errFile = sys.stderr, logFile = None):
    def check(x):
        if isinstance(x, type):
            errors = typecheckClass(
                x,
                scopeTypes = types,
                infer = infer,
                logFile = logFile)
        else:
            errors = typecheckFunction(
                x,
                scopeTypes = types,
                infer = infer,
                logFile = logFile)

        errs = errFile # scope maintenance
        if errors:
            if type(errs) == list: # for testing
                errs += errors
            else:
                print('\n'.join(map(str, errors)), file = errs)

        return x

    return check

def pure(f):
    f._pure = True
    return f

def assertType(type):
    def asserter(x):
        x._type = type
        return x

    return asserter

@pure
def cast(t: type) -> Infer:
    @pure
    def caster(obj) -> t:
        return obj

    return caster
