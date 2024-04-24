import json
from typing import Any, Generic, NamedTuple, Optional, TypeVar, Callable
from uuid import UUID

from ortools.sat.python.cp_model import IntVar, CpModel, CpSolver, BoundedLinearExpression, LinearExpr, INT32_MIN, INT32_MAX
import ortools.sat.python.cp_model as cp_model


class SolutionCallback(cp_model.CpSolverSolutionCallback):
    def __init__(self, variables: list[IntVar]):
        cp_model.CpSolverSolutionCallback.__init__(self)
        self.__variables = variables
        self.json_dict: list[dict[str, int]] = []
        self.solution_count = 0

    def set_solution_limit(self, limit: int):
        self.solution_limit = limit

    def on_solution_callback(self):
        res = {}
        for var in self.__variables:
            res[str(var)] = self.Value(var)
        self.json_dict.append(res)

        self.solution_count += 1
        if hasattr(self, 'solution_limit') and self.solution_count >= self.solution_limit:
            self.StopSearch()

    def solution_from_solver(self, solver: CpSolver) -> dict[str, int]:
        res = {}
        for var in self.__variables:
            res[str(var)] = solver.Value(var)
        return res


T = TypeVar('T')


class BTNode(Generic[T]):
    def __init__(self, data: T, left: Optional['BTNode[T]'] = None, right: Optional['BTNode[T]'] = None):
        self.data = data
        self.left = left
        self.right = right


class Operator(NamedTuple):
    operator: str
    callable: Callable[[cp_model.LinearExprT,
                        cp_model.LinearExprT], cp_model.LinearExprT | cp_model.BoundedLinearExprT]


ConstraintNodeType = Operator | int | UUID


def parse_operator(op: str) -> Operator:
    match op:
        case '+':
            def c(x, y): return x + y
        case '-':
            def c(x, y): return x - y
        case '<':
            def c(x, y): return x < y
        case '<=':
            def c(x, y): return x <= y
        case '>':
            def c(x, y): return x > y
        case '>=':
            def c(x, y): return x >= y
        case '==':
            def c(x, y): return x == y
        case '!=':
            def c(x, y): return x != y
        case '*':
            def c(x, y): return x * y
        case '//':
            def c(x, y): return x // y
        case 'mod':
            def c(x, y): return x % y
        case _:
            raise ValueError(f'Invalid operator: {op}')

    return Operator(op, c)


def parse_ast(ast: dict[str, Any]) -> BTNode[ConstraintNodeType]:
    def inner(node: dict[str, Any]) -> BTNode[ConstraintNodeType]:
        match node['type']:
            case 'operator':
                left = node['children'][0]
                right = node['children'][1]
                left_node = inner(left)
                right_node = inner(right)
                return BTNode(parse_operator(node['value']), left_node, right_node)
            case 'variable':
                return BTNode(UUID(node['value']))
            case 'literal':
                return BTNode(int(node['value']))
            case _:
                raise ValueError('Invalid node type')

    return inner(ast)


def process_arithmetic_expression(model: CpModel, ast: dict, variables: dict[UUID, IntVar]):
    root = parse_ast(ast)

    def inner(node: BTNode[ConstraintNodeType]) -> cp_model.LinearExprT | cp_model.BoundedLinearExprT:
        if isinstance(node.data, Operator):
            if node.left is None or node.right is None:
                raise ValueError('Invalid expression')

            left = inner(node.left)
            right = inner(node.right)

            if isinstance(left, BoundedLinearExpression) or isinstance(left, bool) or isinstance(right, BoundedLinearExpression) or isinstance(right, bool):
                raise ValueError('Invalid expression')

            if node.data.operator == 'mod' and (isinstance(left, LinearExpr) or isinstance(right, LinearExpr)):
                var = model.NewIntVar(INT32_MIN, INT32_MAX, '')
                if not isinstance(left, IntVar):
                    tmp = model.NewIntVar(INT32_MIN, INT32_MAX, '')
                    model.add(tmp == left)
                    left = tmp
                if not isinstance(right, IntVar):
                    tmp = model.NewIntVar(1, INT32_MAX, '')
                    model.add(tmp == right)
                    right = tmp
                model.add_modulo_equality(var, left, right)
                return var

            if node.data.operator == '//' and (isinstance(left, LinearExpr) or isinstance(right, LinearExpr)):
                var = model.new_int_var(INT32_MIN, INT32_MAX, '')
                if not isinstance(left, IntVar):
                    tmp = model.new_int_var(INT32_MIN, INT32_MAX, '')
                    model.add(tmp == left)
                    left = tmp
                if not isinstance(right, IntVar):
                    tmp = model.new_int_var_from_domain(
                        cp_model.Domain.from_intervals([[INT32_MIN, -1], [1, INT32_MAX]]), '')
                    model.add(tmp != 0)
                    model.add(tmp == right)
                    right = tmp
                model.add_division_equality(var, left, right)
                return var

            if node.data.operator == '*' and (isinstance(left, LinearExpr) and isinstance(right, LinearExpr)):
                if not isinstance(left, IntVar):
                    tmp = model.new_int_var(INT32_MIN, INT32_MAX, '')
                    model.add(tmp == left)
                    left = tmp
                if not isinstance(right, IntVar):
                    tmp = model.new_int_var(INT32_MIN, INT32_MAX, '')
                    model.add(tmp == right)
                    right = tmp
                var = model.new_int_var(INT32_MIN, INT32_MAX, '')
                model.add_multiplication_equality(var, left, right)
                return var

            return node.data.callable(left, right)
        elif isinstance(node.data, int):
            return node.data
        elif isinstance(node.data, UUID):
            return variables[node.data]
        else:
            raise ValueError('Invalid node type')

    return inner(root)


def parse_model(json_data: dict) -> tuple[CpModel, CpSolver, SolutionCallback]:
    model = CpModel()
    model_id = UUID(json_data['id'])
    model.name = str(model_id)
    solver = CpSolver()

    variables: dict[UUID, IntVar] = {}

    for var in json_data['variables']:
        var_id = UUID(var['id'])
        var_ub = int(var['ub']) if 'ub' in var else INT32_MAX
        var_lb = int(var['lb']) if 'lb' in var else INT32_MIN
        var = model.new_int_var(var_lb, var_ub, str(var_id))
        variables[var_id] = var

    solution_callback = SolutionCallback(list(variables.values()))

    for constraint in json_data['constraints']:
        constraint_id = UUID(constraint['id'])
        match constraint['type']:
            case 'arithmetic':
                expr = process_arithmetic_expression(
                    model, constraint['value'], variables)
                assert isinstance(
                    expr, BoundedLinearExpression) or isinstance(expr, bool)
                constraint = model.add(expr).with_name(str(constraint_id))
            case 'all_different':
                vars = [variables[UUID(var)] for var in constraint['value']]
                constraint = model.add_all_different(vars)
            case _:
                raise ValueError('Invalid constraint type')

    if 'options' in json_data:
        options = json_data['options']
        objective_methods = {
            'min': model.minimize,
            'max': model.maximize
        }
        for option in options:
            match option['type']:
                case 'min' | 'max':
                    if model.has_objective():
                        raise ValueError('Multiple objectives')
                    var = variables[UUID(option['value'])]
                    objective_methods[option['type']](var)
                case 'solution_limit':
                    solution_callback.set_solution_limit(int(option['value']))
                case 'time_limit':
                    millis: int = option['value']
                    solver.parameters.max_time_in_seconds = millis / 1000
                case _:
                    raise ValueError('Invalid option type')

    if (not model.has_objective()):
        solver.parameters.enumerate_all_solutions = True

    return model, solver, solution_callback


def solve_job(json_data: dict) -> list[dict[str, int]]:
    model, solver, solution_callback = parse_model(json_data)
    status = solver.solve(model, solution_callback)

    match status:
        case cp_model.OPTIMAL | cp_model.FEASIBLE:
            # the callback can get multiple solutions but we only need the one with the best objective value
            if model.has_objective():
                return [solution_callback.solution_from_solver(solver)]

            return solution_callback.json_dict
        case cp_model.MODEL_INVALID:
            raise ValueError(f'Invalid model: {model.Validate()}')
        case _:
            raise ValueError(f'No solution found: {status}')


if __name__ == '__main__':
    data = json.loads('model.json')
    res = solve_job(data)
    print(res)
