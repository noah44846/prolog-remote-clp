from typing import Generic, Optional, TypeVar, Callable
from uuid import UUID

from ortools.sat.python.cp_model import IntVar, CpModel, CpSolver, LinearExprT, BoundedLinearExprT, CpSolverSolutionCallback, BoundedLinearExpression, LinearExpr
import ortools.sat.python.cp_model as cp_model


class RemoteClpResultSolutionCallback(CpSolverSolutionCallback):
    def __init__(self, variables: list[IntVar]):
        CpSolverSolutionCallback.__init__(self)
        self.__variables = variables
        self.json_dict = []

    def on_solution_callback(self):
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


OperatorType = Callable[[LinearExprT, LinearExprT],
                        LinearExprT | BoundedLinearExprT]
ConstraintNodeType = OperatorType | int | UUID


def parse_operator(op: str) -> OperatorType:
    match op:
        case '+':
            return lambda x, y: x + y
        case '-':
            return lambda x, y: x - y
        case '*':
            return lambda x, y: x * y
        # case '/':
        #    return lambda x, y: x // y
        case '<':
            return lambda x, y: x < y
        case '<=':
            return lambda x, y: x <= y
        case '>':
            return lambda x, y: x > y
        case '>=':
            return lambda x, y: x >= y
        case '==':
            return lambda x, y: x == y
        case '!=':
            return lambda x, y: x != y
        case _:
            raise ValueError(f'Invalid operator: {op}')


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


def parse_linear_expression(ast: dict, variables: dict[UUID, IntVar]):
    root = parse_ast(ast)

    def inner(node: BTNode[ConstraintNodeType]) -> LinearExprT | BoundedLinearExprT:
        if callable(node.data):
            left = inner(node.left)
            right = inner(node.right)
            return node.data(left, right)
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
        var_ub = int(var['ub']) if 'ub' in var else cp_model.INT32_MAX
        var_lb = int(var['lb']) if 'lb' in var else cp_model.INT32_MIN
        var = model.new_int_var(var_lb, var_ub, str(var_id))
        variables[var_id] = var

    for constraint in json_data['constraints']:
        constraint_id = UUID(constraint['id'])
        match constraint['type']:
            case 'linear':
                expr = parse_linear_expression(constraint['value'], variables)
                assert isinstance(
                    expr, BoundedLinearExpression) or isinstance(expr, bool)
                constraint = model.add(expr).with_name(str(constraint_id))
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
        solver.parameters.enumerate_all_solutions = True

        status = solver.solve(model, RemoteClpResultSolutionCallback(vars))

        if status == cp_model.OPTIMAL or status == cp_model.FEASIBLE:
            pass
        else:
            print('No solution found.')
