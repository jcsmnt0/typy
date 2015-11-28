from typy.classes import *

overrides = {
    int.__new__: Function([Integer, Object], Integer),
    str.__new__: Function([String, Object], String),
    bool.__new__: Function([Boolean, Object], Boolean),
}
