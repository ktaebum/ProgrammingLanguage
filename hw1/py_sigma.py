import random
import subprocess

ocaml_filename = "sigma.ml"


def sigma(a, b, f):
    if a > b:
        a, b = b, a
    return sum(list(map(lambda x: f(x), range(a, b + 1))))


if __name__ == "__main__":
    functions = [
        lambda x: x,
        lambda x: x**2,
        lambda x: 4 * (x**2) + 2 * x,
    ]

    for i in range(100):
        a = random.randint(-100, 100)
        b = random.randint(50, 1000)
        f = random.randint(0, 2)
        ocaml_result = subprocess.check_output(
            ["ocaml", ocaml_filename,
             str(a), str(b), str(f)]).strip().decode('utf-8')

        python_result = str(sigma(a, b, functions[f]))

        assert ocaml_result == python_result, "Test Fail"

        print("Test %d Success!" % (i + 1))
