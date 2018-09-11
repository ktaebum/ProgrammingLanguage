import random
import subprocess

ocaml_filename = "test_sumprod.ml"


def sumprod(f, n, k):
    # the most naive version of sumprod
    result = 0.
    for i in range(1, n + 1):
        prod = 1.
        for j in range(1, k + 1):
            prod *= f(i, j)
        result += prod
    return result


if __name__ == "__main__":
    functions = [
        lambda x, y: x * y,
        lambda x, y: x**2 + y**2,
        lambda x, y: 4 * x**2 + 10 * x - 8 * y + y**3,
    ]

    for i in range(100):
        n = random.randint(1, 10)
        k = random.randint(1, 10)
        f = random.randint(0, 2)
        ocaml_result = subprocess.check_output(
            ["ocaml", ocaml_filename,
             str(n), str(k), str(f)]).strip().decode('utf-8')

        python_result = "%.5f" % sumprod(functions[f], n, k)

        if ocaml_result == python_result:
            print("Test %d Success!" % (i + 1))
        else:
            print("Test Failed!")
            print("n = %d" % n)
            print("k = %d" % k)
            print("f = %d" % f)
            print(ocaml_result)
            print(python_result)
            break
