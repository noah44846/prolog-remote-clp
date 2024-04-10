import json
from typing import Generic, Optional, TypeVar, Callable
from uuid import UUID
from math import inf

from ortools.sat.python.cp_model import IntVar, CpModel, CpSolver, BoundedLinearExpression
import ortools.sat.python.cp_model as cp_model

class VarArraySolutionPrinter(cp_model.CpSolverSolutionCallback):
    """Print intermediate solutions."""

    def __init__(self, variables: list[cp_model.IntVar]):
        cp_model.CpSolverSolutionCallback.__init__(self)
        self.__variables = variables
        self.__solution_count = 0

    def on_solution_callback(self) -> None:
        self.__solution_count += 1
        for v in self.__variables:
            print(f"{v}={self.value(v)}", end=" ")
        print()

    @property
    def solution_count(self) -> int:
        return self.__solution_count

T = TypeVar("T")

class BTNode(Generic[T]):
    def __init__(self, data, left: Optional[T] = None, right: Optional[T] =None):
        self.data = data
        self.left = left
        self.right = right

ConstraintNodeType = Callable[[int, int], int] | int | UUID

def parse_operator(op: str) -> Callable[[int, int], int]:
    match op:
        case "+":
            return lambda x, y: x + y
        case "-":
            return lambda x, y: x - y
        case "*":
            return lambda x, y: x * y
        case "/":
            return lambda x, y: x // y
        case "<":
            return lambda x, y: x < y
        case "<=":
            return lambda x, y: x <= y
        case ">":
            return lambda x, y: x > y
        case ">=":
            return lambda x, y: x >= y
        case "==":
            return lambda x, y: x == y
        case "!=":
            return lambda x, y: x != y
        case _:
            raise ValueError(f"Invalid operator: {op}")
        
def parse_ast(ast: dict) -> BTNode[ConstraintNodeType]:
    root: BTNode[ConstraintNodeType] = BTNode(ast)
    nodes = [root]
    while nodes:
        node = nodes.pop()
        match node.data["type"]:
            case "operator":
                left = node.data["children"][0]
                right = node.data["children"][1]
                left_node = BTNode(left)
                right_node = BTNode(right)
                node.left = left_node
                node.right = right_node
                nodes.append(left_node)
                nodes.append(right_node)
                node.data = parse_operator(node.data["value"])
            case "variable":
                node.data = UUID(node.data["value"])
            case "literal":
                node.data = int(node.data["value"])

    return root

def parse_bounded_linear_expression(ast: dict, variables: dict[UUID, IntVar]) -> BoundedLinearExpression:
    ast = parse_ast(ast)

    def inner(node: BTNode[ConstraintNodeType]) -> cp_model.LinearExprT:
        if callable(node.data):
            left = inner(node.left)
            right = inner(node.right)
            return node.data(left, right)
        elif isinstance(node.data, int):
            return node.data
        elif isinstance(node.data, UUID):
            return variables[node.data]
        
    return inner(ast)


def parse_model(json_payload: str) -> tuple[CpModel, list[IntVar]]:
    model = CpModel()
    model_dict = json.loads(json_payload)
    model_id = UUID(model_dict["id"])
    model.name = str(model_id)

    variables: dict[UUID, IntVar] = {}

    for var in model_dict["variables"]:
        var_id = UUID(var["id"])
        var_ub = var["ub"] if "ub" in var else inf
        var_lb = var["lb"] if "lb" in var else -inf
        var = model.new_int_var(var_lb, var_ub, str(var_id))
        variables[var_id] = var

    for constraint in model_dict["constraints"]:
        constraint_id = UUID(constraint["id"])
        expr = parse_bounded_linear_expression(constraint["ast"], variables)
        constraint = model.add(expr).with_name(str(constraint_id))

    objective = model_dict["objective"]
    if objective:
        expr = parse_bounded_linear_expression(objective["ast"], variables)
        match objective["type"]:
            case "minimize":
                model.minimize(expr)
            case "maximize":
                model.maximize(expr)

    return model, variables.values()

if __name__ == "__main__":
    with open("model.json", "r") as f:
        model, vars = parse_model(f.read())
        solver = CpSolver()
        solver.parameters.enumerate_all_solutions = True

        status = solver.solve(model, VarArraySolutionPrinter(vars))
       
        if status == cp_model.OPTIMAL or status == cp_model.FEASIBLE:
            pass
        else:
            print("No solution found.")
