"""
Module that contains the Value definition and associated type constructs.
"""

from enum import Enum
from intbase import InterpreterBase, ErrorType

class Type(Enum):
    """Enum for all possible Brewin types."""

    INT = 1
    BOOL = 2
    STRING = 3
    CLASS = 4
    NOTHING = 5
    VOID = 6


# Represents a value, which has a type and its value
class Value:
    """A representation for a value that contains a type tag."""

    def __init__(self, value_type, value=None, class_type=None):
        self.__type = value_type
        self.__value = value
        self.__class_type = class_type

    def type(self):
        return self.__type

    def value(self):
        return self.__value
    
    def class_type(self):
        return self.__class_type

    def set(self, other):
        self.__type = other.type()
        self.__value = other.value()
        self.__class_type = other.class_type()

# pylint: disable=too-many-return-statements
def create_value(interpreter, val, type):
    """
    Create a Value object of specified type
    """
    if val == InterpreterBase.TRUE_DEF:
        if type != InterpreterBase.BOOL_DEF:
            interpreter.error(
                ErrorType.NAME_ERROR,
                f"Value {val} is not of type {type}"
            )
        return Value(Type.BOOL, True)
    if val == InterpreterBase.FALSE_DEF:
        if type != InterpreterBase.BOOL_DEF:
            interpreter.error(
                ErrorType.NAME_ERROR,
                f"Value {val} is not of type {type}"
            )
        return Value(Type.BOOL, False)
    if val[0] == '"':
        if type != InterpreterBase.STRING_DEF:
            interpreter.error(
                ErrorType.NAME_ERROR,
                f"Value {val} is not of type {type}"
            )
        return Value(Type.STRING, val.strip('"'))
    if val.lstrip('-').isnumeric():
        if type != InterpreterBase.INT_DEF:
            interpreter.error(
                ErrorType.NAME_ERROR,
                f"Value {val} is not of type {type}"
            )
        return Value(Type.INT, int(val))
    if val == InterpreterBase.NULL_DEF:
        if type not in interpreter.get_class_set():
            interpreter.error(
                ErrorType.NAME_ERROR,
                f"Class {type} does not exist"
            )
        return Value(Type.CLASS, None, type)
    if val == InterpreterBase.NOTHING_DEF:
        return Value(Type.NOTHING, None)
    return None

def create_infered_value(val):
    """
    Create a Value object from a value.
    """
    if val == InterpreterBase.TRUE_DEF:
        return Value(Type.BOOL, True)
    if val == InterpreterBase.FALSE_DEF:
        return Value(Type.BOOL, False)
    if val[0] == '"':
        return Value(Type.STRING, val.strip('"'))
    if val.lstrip('-').isnumeric():
        return Value(Type.INT, int(val))
    if val == InterpreterBase.NULL_DEF:
        return Value(Type.CLASS, None)
    if val == InterpreterBase.NOTHING_DEF:
        return Value(Type.NOTHING, None)
    return None
