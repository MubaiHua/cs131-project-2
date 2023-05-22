# pylint: disable=too-few-public-methods

"""
Module with classes for class, field, and method definitions.

In P1, we don't allow overloading within a class;
two methods cannot have the same name with different parameters.
"""

from intbase import InterpreterBase, ErrorType
from type_value import Type


class MethodDef:
    """
    Wrapper struct for the definition of a member method.
    """

    def __init__(self, type, name, params, code, class_type=None):
        self.method_type = type
        self.method_class_type = class_type
        self.method_name = name
        self.formal_params = params
        self.code = code


class FieldDef:
    """
    Wrapper struct for the definition of a member field.
    """

    def __init__(self, field_def):
        self.field_type = field_def[1]
        self.field_name = field_def[2]
        self.default_field_value = field_def[3]


class ParamDef:
    """
    Wrapper struct for the definition of a param in a method.
    """

    def __init__(self, param_type, param_name, param_class_type=None):
        self.param_type = param_type
        self.param_name = param_name
        self.param_class_type = param_class_type


class ClassDef:
    """
    Holds definition for a class:
        - list of fields (and default values)
        - list of methods

    class definition: [class classname [field1 field2 ... method1 method2 ...]]
    """

    def __init__(self, class_name, inherit_class, class_def, interpreter):
        self.interpreter = interpreter
        self.name = class_name
        self.inherit_class = inherit_class
        self.get_all_field_types()
        self.get_all_method_types()
        self.__create_field_list(class_def)
        self.__create_method_list(class_def)

    def get_fields(self):
        """
        Get a list of FieldDefs for *all* fields in the class.
        """
        return self.fields

    def get_methods(self):
        """
        Get a list of MethodDefs for *all* fields in the class.
        """
        return self.methods

    def __create_field_list(self, class_body):
        self.fields = []
        fields_defined_so_far = set()
        for member in class_body:
            if member[0] == InterpreterBase.FIELD_DEF:
                type_def = member[1]

                if type_def not in self.all_type_sets:
                    self.interpreter.error(
                        ErrorType.TYPE_ERROR,
                        f"{type_def} is not a primitive type or class type",
                        member[0].line_num,
                    )

                if member[2] in fields_defined_so_far:  # redefinition
                    self.interpreter.error(
                        ErrorType.NAME_ERROR,
                        "duplicate field " + member[2],
                        member[0].line_num,
                    )
                self.fields.append(FieldDef(member))
                fields_defined_so_far.add(member[2])

    def __create_method_list(self, class_body):
        self.methods = []
        methods_defined_so_far = set()
        for member in class_body:
            if member[0] == InterpreterBase.METHOD_DEF:
                method_type = member[1]
                if method_type not in self.all_method_type_sets:
                    self.interpreter.error(
                        ErrorType.TYPE_ERROR,
                        f"{method_type} is not a primitive type or class type",
                        member[0].line_num,
                    )
                if member[2] in methods_defined_so_far:  # redefinition
                    self.interpreter.error(
                        ErrorType.NAME_ERROR,
                        "duplicate method " + member[2],
                        member[0].line_num,
                    )

                # process method params
                params = member[3]
                checked_params = []
                params_set = set()
                for param in params:
                    if len(param) != 2:
                        self.interpreter.error(
                            ErrorType.NAME_ERROR,
                            "Invalid parameter definition " + member[2],
                            member[0].line_num,
                        )

                    type = param[0]
                    name = param[1]
                    if name in params_set:
                        self.interpreter.error(
                            ErrorType.NAME_ERROR,
                            f"duplicate formal param name {name}",
                            member[0].line_num,
                        )

                    if type not in self.all_type_sets:
                        self.interpreter.error(
                            ErrorType.TYPE_ERROR,
                            f"{method_type} is not a primitive type or class type",
                            member[0].line_num,
                        )

                    checked_params.append(self.init_param(type, name))
                    params_set.add(name)

                method_type = self.get_method_type(member[1])
                method_class_type = member[1] if method_type == Type.CLASS else None
                if len(member) <=4:
                    code = None
                else:
                    code = member[4]
                self.methods.append(
                    MethodDef(
                        method_type,
                        member[2],
                        checked_params,
                        code,
                        method_class_type,
                    )
                )
                methods_defined_so_far.add(member[2])

    def get_all_field_types(self):
        self.all_type_sets = set()
        for type in [
            InterpreterBase.INT_DEF,
            InterpreterBase.BOOL_DEF,
            InterpreterBase.STRING_DEF,
        ]:
            self.all_type_sets.add(type)

        for class_type in self.interpreter.get_class_set():
            self.all_type_sets.add(class_type)

    def get_all_method_types(self):
        self.all_method_type_sets = set()
        for type in [
            InterpreterBase.INT_DEF,
            InterpreterBase.BOOL_DEF,
            InterpreterBase.STRING_DEF,
            InterpreterBase.VOID_DEF,
        ]:
            self.all_method_type_sets.add(type)

        for class_type in self.interpreter.get_class_set():
            self.all_method_type_sets.add(class_type)

    def init_param(self, type, name):
        if type == InterpreterBase.INT_DEF:
            return ParamDef(Type.INT, name)
        if type == InterpreterBase.BOOL_DEF:
            return ParamDef(Type.BOOL, name)
        if type == InterpreterBase.STRING_DEF:
            return ParamDef(Type.STRING, name)
        if type in self.interpreter.get_class_set():
            return ParamDef(Type.CLASS, name, type)
        self.interpreter.error(
            ErrorType.TYPE_ERROR,
            f"{type} is not a primitive type or class type",
        )

    def get_method_type(self, type):
        if type == InterpreterBase.INT_DEF:
            return Type.INT
        if type == InterpreterBase.BOOL_DEF:
            return Type.BOOL
        if type == InterpreterBase.STRING_DEF:
            return Type.STRING
        if type == InterpreterBase.VOID_DEF:
            return Type.VOID
        if type in self.interpreter.get_class_set():
            return Type.CLASS
        self.interpreter.error(
            ErrorType.TYPE_ERROR,
            f"{type} is not a primitive type or class type",
        )
