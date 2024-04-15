from typing import Generic, NamedTuple, Optional, TypeVar, Callable
from uuid import UUID

from ortools.sat.python.cp_model import IntVar, CpModel, CpSolver, LinearExprT, BoundedLinearExprT, CpSolverSolutionCallback, BoundedLinearExpression, LinearExpr, Domain, INT32_MIN, INT32_MAX
import ortools.sat.python.cp_model as cp_model


class RemoteClpResultSolutionCallback(CpSolverSolutionCallback):
    def __init__(self, variables: list[IntVar]):
        CpSolverSolutionCallback.__init__(self)
        self.__variables = variables
        self.json_dict = []

    def on_solution_callback(self):
        print('Solution found')
        res = {}
        for var in self.__variables:
            res[str(var)] = self.Value(var)
        self.json_dict.append(res)


T = TypeVar('T')


class BTNode(Generic[T]):
    def __init__(self, data: T, left: Optional[T] = None, right: Optional[T] = None):
        self.data = data
        self.left = left
        self.right = right


class Operator(NamedTuple):
    operator: str
    callable: Optional[Callable[[LinearExprT, LinearExprT], LinearExprT]]


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


def parse_ast(ast: dict) -> BTNode[ConstraintNodeType]:
    root: BTNode[ConstraintNodeType] = BTNode(ast)
    nodes = [root]
    while nodes:
        node = nodes.pop()
        match node.data['type']:
            case 'operator':
                left = node.data['children'][0]
                right = node.data['children'][1]
                left_node = BTNode(left)
                right_node = BTNode(right)
                node.left = left_node
                node.right = right_node
                nodes.append(left_node)
                nodes.append(right_node)
                node.data = parse_operator(node.data['value'])
            case 'variable':
                node.data = UUID(node.data['value'])
            case 'literal':
                node.data = int(node.data['value'])

    return root


def process_arithmetic_expression(model: CpModel, ast: dict, variables: dict[UUID, IntVar]):
    root = parse_ast(ast)

    def inner(node: BTNode[ConstraintNodeType]) -> LinearExprT | BoundedLinearExprT:
        if isinstance(node.data, Operator):
            left = inner(node.left)
            right = inner(node.right)

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
                        Domain.from_intervals([[INT32_MIN, -1], [1, INT32_MAX]]), '')
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

    return inner(root)


def parse_model(json_data: dict) -> tuple[CpModel, list[IntVar]]:
    model = CpModel()
    model_id = UUID(json_data['id'])
    model.name = str(model_id)

    variables: dict[UUID, IntVar] = {}

    for var in json_data['variables']:
        var_id = UUID(var['id'])
        var_ub = int(var['ub']) if 'ub' in var else INT32_MAX
        var_lb = int(var['lb']) if 'lb' in var else INT32_MIN
        var = model.new_int_var(var_lb, var_ub, str(var_id))
        variables[var_id] = var

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

    if 'objectives' in json_data:
        objectives = json_data['objectives']
        for objective in objectives:
            var = variables[UUID(objective['value'])]
        match objective['type']:
            case 'min':
                model.minimize(var)
            case 'max':
                model.maximize(var)

    return model, variables.values()


def solve_job(json_data: dict) -> dict:
    model, vars = parse_model(json_data)
    solver = CpSolver()

    solver.parameters.enumerate_all_solutions = True
    # if (not model.has_objective()):
    #     solver.parameters.enumerate_all_solutions = True

    solution_callback = RemoteClpResultSolutionCallback(vars)
    status = solver.solve(model, solution_callback)

    match status:
        case cp_model.OPTIMAL | cp_model.FEASIBLE:
            if model.has_objective():
                res = {}
                for var in vars:
                    res[str(var)] = solver.Value(var)
                return [res]

            return solution_callback.json_dict
        case cp_model.MODEL_INVALID:
            raise ValueError(f'Invalid model: {model.Validate()}')
        case _:
            raise ValueError(f'No solution found: {status}')


if __name__ == '__main__':
    with open('model.json', 'r') as f:
        model, vars = parse_model(f.read())
        solver = CpSolver()
        # solver.parameters.enumerate_all_solutions = True

        status = solver.solve(model, RemoteClpResultSolutionCallback(vars))

        if status == cp_model.OPTIMAL or status == cp_model.FEASIBLE:
            pass
        else:
            print('No solution found.')
