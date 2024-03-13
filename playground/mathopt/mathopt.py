
# solve_math_opt_model_via_http.py

"""Example of solving a MathOpt model through the OR API.

The model is built using the Python API, and the corresponding proto is
serialized to JSON to make the HTTP request.
"""

from collections.abc import Sequence
import json

from absl import app

from ortools.math_opt.python import mathopt
from ortools.math_opt.python.ipc import remote_http_solve

def request_example() -> None:
    """Endpoint for the Operations Research API.
      https://optimization.googleapis.com/v1/mathopt:solveMathOptModel
    """

    with open("credentials.json") as f:
        credentials = json.load(f)
        api_key = credentials["key"]

    if not api_key:
        print(
            "API key is required. See"
            " https://developers.google.com/optimization/service/setup for"
            " instructions."
        )
        return

    # Build a MathOpt model
    model = mathopt.Model(name="my_model")
    x = model.add_binary_variable(name="x")
    y = model.add_variable(lb=0.0, ub=2.5, name="y")
    model.add_linear_constraint(x + y <= 1.5, name="c")
    model.add_linear_constraint(2*x + y >= -13, name="c")
    model.maximize(2 * x + y)
    try:
        print(json.dumps(remote_http_solve._build_json_payload(model, mathopt.SolverType.GSCIP, None, None)))
        # result, logs = remote_http_solve.remote_http_solve(
        #     model, mathopt.SolverType.GSCIP, api_key=api_key
        # )

        # print(result)
        # print(logs)
        
    except remote_http_solve.OptimizationServiceError as err:
        print(err)


def main(argv: Sequence[str]) -> None:
    del argv  # Unused.
    request_example()


if __name__ == "__main__":
    app.run(main)
