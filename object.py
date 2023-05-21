"""
Module handling the operations of an object. This contains the meat
of the code to execute various instructions.
"""

from environment import EnvironmentManager
from intbase import InterpreterBase, ErrorType
from type_value import create_value, create_infered_value
from type_value import Type, Value


class ObjectDef:
    STATUS_PROCEED = 0
    STATUS_RETURN = 1
    STATUS_NAME_ERROR = 2
    STATUS_TYPE_ERROR = 3

    def __init__(self, interpreter, class_def, trace_output):
        self.interpreter = interpreter  # objref to interpreter object. used to report errors, get input, produce output
        self.class_def = class_def  # take class body from 3rd+ list elements, e.g., ["class",classname", [classbody]]
        self.trace_output = trace_output
        self.__map_fields_to_values()
        self.__map_method_names_to_method_definitions()
        self.__map_inherit_fields_to_values()
        self.__map_inherit_method_names_to_method_definitions()
        self.__create_map_of_operations_to_lambdas()  # sets up maps to facilitate binary and unary operations, e.g., (+ 5 6)

    def call_method(
        self, method_name, actual_params, line_num_of_caller, inherit_level=-1
    ):
        """
        actual_params is a list of Value objects (all parameters are passed by value).

        The caller passes in the line number so we can properly generate an error message.
        The error is then generated at the source (i.e., where the call is initiated).
        """
        inherit_deepth = len(self.inherit_methods)
        actual_params_len = len(actual_params)
        list_of_actual_params_type = []
        method_info = None

        for param in actual_params:
            if param.type() == Type.CLASS:
                if param.value() != None:
                    list_of_actual_params_type.append(
                        (param.type(), param.class_type())
                    )
            else:
                list_of_actual_params_type.append(param.type())

        if inherit_level == -1:  # no super used
            if method_name in self.methods:
                if len(self.methods[method_name].formal_params) == actual_params_len:
                    flag = True
                    for i, formal in enumerate(self.methods[method_name].formal_params):
                        if isinstance(list_of_actual_params_type[i], tuple):
                            if formal.param_type != Type.CLASS:  # compare class type
                                flag = False
                                break
                            if (
                                formal.param_class_type
                                != list_of_actual_params_type[i][1]
                            ):
                                if not self.interpreter.is_parent_class(
                                    list_of_actual_params_type[i][1],
                                    formal.param_class_type,
                                ):
                                    flag = False
                                    break
                        else:
                            if list_of_actual_params_type[i] != formal.param_type:
                                flag = False
                                break
                    if flag:
                        method_info = self.methods[method_name]

        if method_info == None:
            if inherit_level == -1:
                inherit_level = 0
            while inherit_level < inherit_deepth:
                if method_name not in self.inherit_methods[inherit_level]:
                    inherit_level += 1
                    continue
                else:
                    if (
                        len(
                            self.inherit_methods[inherit_level][
                                method_name
                            ].formal_params
                        )
                        == actual_params_len
                    ):
                        flag = True
                        for i, formal in enumerate(
                            self.inherit_methods[inherit_level][
                                method_name
                            ].formal_params
                        ):
                            if isinstance(list_of_actual_params_type[i], tuple):
                                if (
                                    formal.param_type != Type.CLASS
                                ):  # compare class type
                                    flag = False
                                    break
                                if (
                                    formal.param_class_type
                                    != list_of_actual_params_type[i][1]
                                ):
                                    if not self.interpreter.is_parent_class(
                                        list_of_actual_params_type[i][1],
                                        formal.param_class_type,
                                    ):
                                        flag = False
                                        break
                            else:
                                if list_of_actual_params_type[i] != formal.param_type:
                                    flag = False
                                    break
                        if flag:
                            method_info = self.inherit_methods[inherit_level][method_name]
                            break
                    else:
                        inherit_level += 1

        if method_info == None:  # if no matching method found
            self.interpreter.error(
                ErrorType.NAME_ERROR,
                f"Unknown method {method_name}",
                line_num_of_caller,
            )

        env = (
            EnvironmentManager()
        )  # maintains lexical environment for function; just params for now

        for formal, actual in zip(method_info.formal_params, actual_params):
            if formal.param_type != actual.type():
                self.interpreter.error(
                    ErrorType.NAME_ERROR,
                    f"Parameter {formal.param_name} does not have the same type as the assigned value {actual.value()}",
                    line_num_of_caller,
                )

            if formal.param_type == Type.CLASS:
                if actual.value() != None:  # if the value is not null
                    if formal.param_class_type != actual.class_type():
                        self.interpreter.error(
                            ErrorType.NAME_ERROR,
                            f"Value {actual.value()} does not match the {formal.param_name} type",
                            line_num_of_caller,
                        )

            env.set(
                formal.param_name, actual, formal.param_type, formal.param_class_type
            )
        # since each method has a single top-level statement, execute it.
        status, return_value = self.__execute_statement(
            env, method_info.code, method_name, inherit_level
        )
        # if the method explicitly used the (return expression) statement to return a value, then return that
        # value back to the caller
        if status == ObjectDef.STATUS_RETURN:
            return return_value
        # Handle defualt return value
        method_type = method_info.method_type
        return self.__get_defualt_return_type(method_type)

    def __get_defualt_return_type(self, method_type):
        if method_type == Type.BOOL:
            return Value(Type.BOOL, False)
        if method_type == Type.INT:
            return Value(Type.INT, 0)
        if method_type == Type.STRING:
            return Value(Type.STRING, "")
        if method_type == Type.CLASS:
            return Value(Type.CLASS, None, None)  # null value

    def __execute_statement(self, env, code, method_name, inherit_level):
        """
        returns (status_code, return_value) where:
        - status_code indicates if the next statement includes a return
            - if so, the current method should terminate
            - otherwise, the next statement in the method should run normally
        - return_value is a Value containing the returned value from the function
        """
        if self.trace_output:
            print(f"{code[0].line_num}: {code}")
        tok = code[0]
        if tok == InterpreterBase.BEGIN_DEF:
            return self.__execute_begin(env, code, method_name, inherit_level)
        if tok == InterpreterBase.SET_DEF:
            return self.__execute_set(env, code, inherit_level)
        if tok == InterpreterBase.IF_DEF:
            return self.__execute_if(env, code, method_name, inherit_level)
        if tok == InterpreterBase.CALL_DEF:
            return self.__execute_call(env, code, inherit_level)
        if tok == InterpreterBase.WHILE_DEF:
            return self.__execute_while(env, code, method_name, inherit_level)
        if tok == InterpreterBase.RETURN_DEF:
            return self.__execute_return(env, code, method_name, inherit_level)
        if tok == InterpreterBase.INPUT_STRING_DEF:
            return self.__execute_input(env, code, True, inherit_level)
        if tok == InterpreterBase.INPUT_INT_DEF:
            return self.__execute_input(env, code, False, inherit_level)
        if tok == InterpreterBase.PRINT_DEF:
            return self.__execute_print(env, code, inherit_level)
        if tok == InterpreterBase.LET_DEF:
            return self.__execute_let(env, code, method_name, inherit_level)

        self.interpreter.error(
            ErrorType.SYNTAX_ERROR, "unknown statement " + tok, tok.line_num
        )

    def __execute_let(self, outside_env, code, method_name, inherit_level):
        def get_local_variable_type(local_variable_type):
            if local_variable_type == InterpreterBase.INT_DEF:
                return Type.INT
            if local_variable_type == InterpreterBase.BOOL_DEF:
                return Type.BOOL
            if local_variable_type == InterpreterBase.STRING_DEF:
                return Type.STRING
            if local_variable_type in self.interpreter.get_class_set():
                return Type.CLASS
            self.interpreter.error(
                ErrorType.NAME_ERROR,
                f"{local_variable_type} is invalid",
                code[0].line_num,
            )

        if (
            len(code) < 2
        ):  # let statement must always have at least one statement to execute
            self.interpreter.error(
                ErrorType.SYNTAX_ERROR,
                f"let statements must always have at least one statement to execute",
                code[0].line_num,
            )
        local_variables = code[1]
        added_local_variables_set = set()
        env = EnvironmentManager()

        # copy existing env
        env.copy(outside_env)

        for local_variable in local_variables:
            if len(local_variable) != 3:  # check syntax
                self.interpreter.error(
                    ErrorType.SYNTAX_ERROR,
                    f"syntax error in let statement",
                    code[0].line_num,
                )

            local_variable_type = get_local_variable_type(local_variable[0])
            local_variable_name = local_variable[1]

            if local_variable_name in added_local_variables_set:
                self.interpreter.error(
                    ErrorType.NAME_ERROR,
                    f"Can't define two local variables with the same name",
                    code[0].line_num,
                )

            local_variable_value = create_infered_value(local_variable[2])
            local_variable_class_type = (
                local_variable[0] if local_variable_type == Type.CLASS else None
            )
            if (
                local_variable_value is None
                or local_variable_value.type() != local_variable_type
            ):
                self.interpreter.error(
                    ErrorType.TYPE_ERROR,
                    f"{local_variable_name} does not have the same type as {local_variable[0]}",
                    code[0].line_num,
                )

            env.set(
                local_variable_name,
                local_variable_value,
                local_variable_type,
                local_variable_class_type,
            )
            added_local_variables_set.add(local_variable_name)

        for statement in code[2:]:
            status, return_value = self.__execute_statement(
                env, statement, method_name, inherit_level
            )
            if status == ObjectDef.STATUS_RETURN:
                return (
                    status,
                    return_value,
                )  # could be a valid return of a value or an error
        # if we run thru the entire block without a return, then just return proceed
        # we don't want the calling block to exit with a return
        return ObjectDef.STATUS_PROCEED, None

    # (begin (statement1) (statement2) ... (statementn))
    def __execute_begin(self, env, code, method_name, inherit_level):
        for statement in code[1:]:
            status, return_value = self.__execute_statement(
                env, statement, method_name, inherit_level
            )
            if status == ObjectDef.STATUS_RETURN:
                return (
                    status,
                    return_value,
                )  # could be a valid return of a value or an error
        # if we run thru the entire block without a return, then just return proceed
        # we don't want the calling block to exit with a return
        return ObjectDef.STATUS_PROCEED, None

    # (call object_ref/me methodname param1 param2 param3)
    # where params are expressions, and expresion could be a value, or a (+ ...)
    # statement version of a method call; there's also an expression version of a method call below
    def __execute_call(self, env, code, inherit_level):
        return ObjectDef.STATUS_PROCEED, self.__execute_call_aux(
            env, code, code[0].line_num, inherit_level
        )

    # (set varname expression), where expresion could be a value, or a (+ ...)
    def __execute_set(self, env, code, inherit_level):
        val = self.__evaluate_expression(env, code[2], code[0].line_num, inherit_level)
        self.__set_variable_aux(env, code[1], val, code[0].line_num, inherit_level)
        return ObjectDef.STATUS_PROCEED, None

    # (return expression) where expresion could be a value, or a (+ ...)
    def __execute_return(self, env, code, method_name, inherit_level):
        method_info = self.methods[method_name]
        method_type = method_info.method_type
        if len(code) == 1:
            # [return] with no return expression
            if method_type != Type.VOID:
                return ObjectDef.STATUS_RETURN, self.__get_defualt_return_type(
                    method_type
                )

            value = create_value(
                self.interpreter,
                InterpreterBase.NOTHING_DEF,
                InterpreterBase.NOTHING_DEF,
            )
            return ObjectDef.STATUS_RETURN, value

        if method_type == Type.VOID:
            self.interpreter.error(
                ErrorType.TYPE_ERROR,
                f"{method_name} of type void should not have a return value",
                code[0].line_num,
            )

        return_value = self.__evaluate_expression(
            env, code[1], code[0].line_num, inherit_level
        )
        if return_value.type() != method_type:
            self.interpreter.error(
                ErrorType.TYPE_ERROR,
                f"{method_name} have wrong return type",
                code[0].line_num,
            )

        if method_type == Type.CLASS:
            if return_value.value() != None:
                if method_info.method_class_type != return_value.class_type():
                    self.interpreter.error(
                        ErrorType.TYPE_ERROR,
                        f"{method_name} have wrong return type",
                        code[0].line_num,
                    )

        return ObjectDef.STATUS_RETURN, return_value

    # (print expression1 expression2 ...) where expresion could be a variable, value, or a (+ ...)
    def __execute_print(self, env, code, inherit_level):
        output = ""
        for expr in code[1:]:
            # TESTING NOTE: Will not test printing of object references
            term = self.__evaluate_expression(
                env, expr, code[0].line_num, inherit_level
            )
            val = term.value()
            typ = term.type()
            if typ == Type.BOOL:
                val = "true" if val else "false"
            # document - will never print out an object ref
            output += str(val)
        self.interpreter.output(output)
        return ObjectDef.STATUS_PROCEED, None

    # (inputs target_variable) or (inputi target_variable) sets target_variable to input string/int
    def __execute_input(self, env, code, get_string, inherit_level):
        inp = self.interpreter.get_input()
        if get_string:
            val = Value(Type.STRING, inp)
        else:
            val = Value(Type.INT, int(inp))

        self.__set_variable_aux(env, code[1], val, code[0].line_num, inherit_level)
        return ObjectDef.STATUS_PROCEED, None

    # helper method used to set either parameter variables or member fields; parameters currently shadow
    # member fields
    def __set_variable_aux(self, env, var_name, value, line_num, inherit_level):
        # parameter shadows fields
        if value.type() == Type.NOTHING:
            self.interpreter.error(
                ErrorType.TYPE_ERROR, "can't assign to nothing " + var_name, line_num
            )
        (param_val, param_type, param_class_type) = env.get(var_name)
        if param_val is not None:
            if param_type != value.type():
                self.interpreter.error(
                    ErrorType.TYPE_ERROR,
                    f"{var_name} does not have the same type as the assigned value",
                    line_num,
                )

            if param_type == Type.CLASS:
                if value.value() != None:  # if the value is not null
                    if param_class_type != value.class_type():
                        if not self.interpreter.is_parent_class(
                            value.class_type(), param_class_type
                        ):
                            self.interpreter.error(
                                ErrorType.TYPE_ERROR,
                                f"{var_name} does not have the same type as the assigned value",
                                line_num,
                            )

            env.set(var_name, value, param_type, param_class_type)
            return

        if inherit_level == -1:
            if var_name not in self.fields:
                self.interpreter.error(
                    ErrorType.NAME_ERROR, "unknown variable " + var_name, line_num
                )
            target_field = self.fields[var_name]
        else:
            if var_name not in self.inherit_fields[inherit_level]:
                self.interpreter.error(
                    ErrorType.NAME_ERROR, "unknown variable " + var_name, line_num
                )
            target_field = self.inherit_fields[inherit_level][var_name]

        # check assignment type
        if target_field.type() != value.type():
            self.interpreter.error(
                ErrorType.TYPE_ERROR,
                f"{var_name} does not have the same type as the assigned value",
                line_num,
            )

        # check class type of class reference
        if target_field.type() == Type.CLASS:
            if value.value() != None:  # if the value is not null
                if target_field.class_type() != value.class_type():
                    if not self.interpreter.is_parent_class(
                        value.class_type(), target_field.class_type()
                    ):
                        self.interpreter.error(
                            ErrorType.TYPE_ERROR,
                            f"{var_name} does not have the same type as the assigned value",
                            line_num,
                        )
        if inherit_level == -1:
            self.fields[var_name] = value
        else:
            self.inherit_fields[inherit_level][var_name] = value

    # (if expression (statement) (statement) ) where expresion could be a boolean constant (e.g., true), member
    # variable without ()s, or a boolean expression in parens, like (> 5 a)
    def __execute_if(self, env, code, method_name, inherit_level):
        condition = self.__evaluate_expression(
            env, code[1], code[0].line_num, inherit_level
        )
        if condition.type() != Type.BOOL:
            self.interpreter.error(
                ErrorType.TYPE_ERROR,
                "non-boolean if condition " + " ".join(x for x in code[1]),
                code[0].line_num,
            )
        if condition.value():
            status, return_value = self.__execute_statement(
                env, code[2], method_name, inherit_level
            )  # if condition was true
            return status, return_value
        if len(code) == 4:
            status, return_value = self.__execute_statement(
                env, code[3], method_name, inherit_level
            )  # if condition was false, do else
            return status, return_value
        return ObjectDef.STATUS_PROCEED, None

    # (while expression (statement) ) where expresion could be a boolean value, boolean member variable,
    # or a boolean expression in parens, like (> 5 a)
    def __execute_while(self, env, code, method_name, inherit_level):
        while True:
            condition = self.__evaluate_expression(env, code[1], code[0].line_num)
            if condition.type() != Type.BOOL:
                self.interpreter.error(
                    ErrorType.TYPE_ERROR,
                    "non-boolean while condition " + " ".join(x for x in code[1]),
                    code[0].line_num,
                )
            if not condition.value():  # condition is false, exit loop immediately
                return ObjectDef.STATUS_PROCEED, None
            # condition is true, run body of while loop
            status, return_value = self.__execute_statement(
                env, code[2], method_name, inherit_level
            )
            if status == ObjectDef.STATUS_RETURN:
                return (
                    status,
                    return_value,
                )  # could be a valid return of a value or an error

    # given an expression, return a Value object with the expression's evaluated result
    # expressions could be: constants (true, 5, "blah"), variables (e.g., x), arithmetic/string/logical expressions
    # like (+ 5 6), (+ "abc" "def"), (> a 5), method calls (e.g., (call me foo)), or instantiations (e.g., new dog_class)
    def __evaluate_expression(self, env, expr, line_num_of_statement, inherit_level):
        if not isinstance(expr, list):
            # locals shadow member variables
            (val, param_type, param_class_type) = env.get(expr)
            if val is not None:
                return val
            if inherit_level == -1:
                if expr in self.fields:
                    return self.fields[expr]
            else:
                if expr in self.inherit_fields[inherit_level]:
                    return self.inherit_fields[inherit_level][expr]

            # need to check for variable name and get its value too
            value = create_infered_value(expr)
            if value is not None:
                return value
            self.interpreter.error(
                ErrorType.NAME_ERROR,
                "invalid field or parameter " + expr,
                line_num_of_statement,
            )

        operator = expr[0]
        if operator in self.binary_op_list:
            operand1 = self.__evaluate_expression(
                env, expr[1], line_num_of_statement, inherit_level
            )
            operand2 = self.__evaluate_expression(
                env, expr[2], line_num_of_statement, inherit_level
            )
            if operand1.type() == operand2.type() and operand1.type() == Type.INT:
                if operator not in self.binary_ops[Type.INT]:
                    self.interpreter.error(
                        ErrorType.TYPE_ERROR,
                        "invalid operator applied to ints",
                        line_num_of_statement,
                    )
                return self.binary_ops[Type.INT][operator](operand1, operand2)
            if operand1.type() == operand2.type() and operand1.type() == Type.STRING:
                if operator not in self.binary_ops[Type.STRING]:
                    self.interpreter.error(
                        ErrorType.TYPE_ERROR,
                        "invalid operator applied to strings",
                        line_num_of_statement,
                    )
                return self.binary_ops[Type.STRING][operator](operand1, operand2)
            if operand1.type() == operand2.type() and operand1.type() == Type.BOOL:
                if operator not in self.binary_ops[Type.BOOL]:
                    self.interpreter.error(
                        ErrorType.TYPE_ERROR,
                        "invalid operator applied to bool",
                        line_num_of_statement,
                    )
                return self.binary_ops[Type.BOOL][operator](operand1, operand2)
            if operand1.type() == operand2.type() and operand1.type() == Type.CLASS:
                if operator not in self.binary_ops[Type.CLASS]:
                    self.interpreter.error(
                        ErrorType.TYPE_ERROR,
                        "invalid operator applied to class",
                        line_num_of_statement,
                    )
                return self.binary_ops[Type.CLASS][operator](operand1, operand2)
            # error what about an obj reference and null
            self.interpreter.error(
                ErrorType.TYPE_ERROR,
                f"operator {operator} applied to two incompatible types",
                line_num_of_statement,
            )
        if operator in self.unary_op_list:
            operand = self.__evaluate_expression(env, expr[1], line_num_of_statement)
            if operand.type() == Type.BOOL:
                if operator not in self.unary_ops[Type.BOOL]:
                    self.interpreter.error(
                        ErrorType.TYPE_ERROR,
                        "invalid unary operator applied to bool",
                        line_num_of_statement,
                    )
                return self.unary_ops[Type.BOOL][operator](operand)

        # handle call expression: (call objref methodname p1 p2 p3)
        if operator == InterpreterBase.CALL_DEF:
            return self.__execute_call_aux(
                env, expr, line_num_of_statement, inherit_level
            )
        # handle new expression: (new classname)
        if operator == InterpreterBase.NEW_DEF:
            return self.__execute_new_aux(env, expr, line_num_of_statement)

    # (new classname)
    def __execute_new_aux(self, _, code, line_num_of_statement):
        obj = self.interpreter.instantiate(code[1], line_num_of_statement)
        return Value(Type.CLASS, obj, code[1])

    # this method is a helper used by call statements and call expressions
    # (call object_ref/me methodname p1 p2 p3)
    def __execute_call_aux(self, env, code, line_num_of_statement, inherit_level):
        # determine which object we want to call the method on
        obj_name = code[1]
        inherit_level = -1
        if obj_name == InterpreterBase.ME_DEF:
            obj = self
        elif obj_name == InterpreterBase.SUPER_DEF:
            if self.class_def.inherit_class == None:
                self.interpreter.error(
                    ErrorType.TYPE_ERROR,
                    f"invalid call to super object by class {self.class_def.name}",
                    line_num_of_statement,
                )
            obj = self
            inherit_level = 0
        else:
            obj = self.__evaluate_expression(
                env, obj_name, line_num_of_statement, inherit_level
            ).value()
        # prepare the actual arguments for passing
        if obj is None:
            self.interpreter.error(
                ErrorType.FAULT_ERROR, "null dereference", line_num_of_statement
            )
        actual_args = []
        for expr in code[3:]:
            actual_args.append(
                self.__evaluate_expression(
                    env, expr, line_num_of_statement, inherit_level
                )
            )
        return obj.call_method(
            code[2], actual_args, line_num_of_statement, inherit_level
        )

    def __map_method_names_to_method_definitions(self):
        self.methods = {}
        for method in self.class_def.get_methods():
            self.methods[method.method_name] = method

    def __map_fields_to_values(self):
        self.fields = {}
        for field in self.class_def.get_fields():
            value = create_value(
                self.interpreter, field.default_field_value, field.field_type
            )
            self.fields[field.field_name] = value

    def __map_inherit_method_names_to_method_definitions(self):
        self.inherit_methods = []
        super_class = self.class_def.inherit_class
        while super_class is not None:  # recursively add inherit methods
            super_class_def = self.interpreter.get_class_dict()[super_class]
            super_class_methods = {}

            for method in super_class_def.get_methods():
                super_class_methods[method.method_name] = method

            self.inherit_methods.append(super_class_methods)
            super_class = super_class_def.inherit_class

    def __map_inherit_fields_to_values(self):
        self.inherit_fields = []
        super_class = self.class_def.inherit_class
        while super_class is not None:  # recursively add inherit methods
            super_class_def = self.interpreter.get_class_dict()[super_class]
            super_class_fields = {}

            for field in super_class_def.get_fields():
                value = create_value(
                    self.interpreter, field.default_field_value, field.field_type
                )
                super_class_fields[field.field_name] = value

            self.inherit_fields.append(super_class_fields)
            super_class = super_class_def.inherit_class

    def __create_map_of_operations_to_lambdas(self):
        self.binary_op_list = [
            "+",
            "-",
            "*",
            "/",
            "%",
            "==",
            "!=",
            "<",
            "<=",
            ">",
            ">=",
            "&",
            "|",
        ]
        self.unary_op_list = ["!"]
        self.binary_ops = {}
        self.binary_ops[Type.INT] = {
            "+": lambda a, b: Value(Type.INT, a.value() + b.value()),
            "-": lambda a, b: Value(Type.INT, a.value() - b.value()),
            "*": lambda a, b: Value(Type.INT, a.value() * b.value()),
            "/": lambda a, b: Value(
                Type.INT, a.value() // b.value()
            ),  # // for integer ops
            "%": lambda a, b: Value(Type.INT, a.value() % b.value()),
            "==": lambda a, b: Value(Type.BOOL, a.value() == b.value()),
            "!=": lambda a, b: Value(Type.BOOL, a.value() != b.value()),
            ">": lambda a, b: Value(Type.BOOL, a.value() > b.value()),
            "<": lambda a, b: Value(Type.BOOL, a.value() < b.value()),
            ">=": lambda a, b: Value(Type.BOOL, a.value() >= b.value()),
            "<=": lambda a, b: Value(Type.BOOL, a.value() <= b.value()),
        }
        self.binary_ops[Type.STRING] = {
            "+": lambda a, b: Value(Type.STRING, a.value() + b.value()),
            "==": lambda a, b: Value(Type.BOOL, a.value() == b.value()),
            "!=": lambda a, b: Value(Type.BOOL, a.value() != b.value()),
            ">": lambda a, b: Value(Type.BOOL, a.value() > b.value()),
            "<": lambda a, b: Value(Type.BOOL, a.value() < b.value()),
            ">=": lambda a, b: Value(Type.BOOL, a.value() >= b.value()),
            "<=": lambda a, b: Value(Type.BOOL, a.value() <= b.value()),
        }
        self.binary_ops[Type.BOOL] = {
            "&": lambda a, b: Value(Type.BOOL, a.value() and b.value()),
            "|": lambda a, b: Value(Type.BOOL, a.value() or b.value()),
            "==": lambda a, b: Value(Type.BOOL, a.value() == b.value()),
            "!=": lambda a, b: Value(Type.BOOL, a.value() != b.value()),
        }
        self.binary_ops[Type.CLASS] = {
            "==": lambda a, b: Value(Type.BOOL, a.value() == b.value()),
            "!=": lambda a, b: Value(Type.BOOL, a.value() != b.value()),
        }

        self.unary_ops = {}
        self.unary_ops[Type.BOOL] = {
            "!": lambda a: Value(Type.BOOL, not a.value()),
        }
