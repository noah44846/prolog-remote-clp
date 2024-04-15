from ortools.sat.python import cp_model

def simple_sat_program():
    """Minimal CP-SAT example to showcase calling the solver."""
    # Creates the model.
    model = cp_model.CpModel()

    # Creates the variables.
    var_upper_bound = max(50, 45, 37)
    x = model.new_int_var(0, var_upper_bound, "x")
    y = model.new_int_var(0, var_upper_bound, "y")
    z = model.new_int_var(0, var_upper_bound, "z")
    a = model.new_int_var(cp_model.INT32_MIN, cp_model.INT32_MAX, 'a')

    # Creates the constraints.
    model.add(2 * x + 7 * y + 3 * z <= 50)
    model.add(3 * x - 5 * y + 7 * z <= 45)
    model.add(5 * x + 2 * y - 6 * z <= 37)

    h = model.new_int_var(0, var_upper_bound, 'h')
    j = model.new_int_var(0, var_upper_bound, 'j')
    model.add(j == 2*x + 2*y)
    model.add_multiplication_equality(h, j, z)

    model.add(h + 3 * z == a)

    model.maximize(a)

    # Creates a solver and solves the model.
    solver = cp_model.CpSolver()
    status = solver.solve(model)
    solver.parameters.enumerate_all_solutions = True

    if status == cp_model.OPTIMAL or status == cp_model.FEASIBLE:
        print(f"Maximum of objective function: {solver.objective_value}\n")
        print(f"x = {solver.value(x)}")
        print(f"y = {solver.value(y)}")
        print(f"z = {solver.value(z)}")
    elif status == cp_model.MODEL_INVALID:
        print(model.Validate())
    else:
        print("No solution found.")

    # Statistics.
    print("\nStatistics")
    print(f"  status   : {solver.status_name(status)}")
    print(f"  conflicts: {solver.num_conflicts}")
    print(f"  branches : {solver.num_branches}")
    print(f"  wall time: {solver.wall_time} s")

if __name__ == "__main__":
    simple_sat_program()
