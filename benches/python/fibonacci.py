# fibonacci.py — iterative Fibonacci(35) benchmark
#
# Python equivalent of benches/urd/fibonacci.urd
#
# After 35 iterations starting from (a=0, b=1):
#   a holds fib(35), b holds fib(36)
#
# Expected output: fib(35) = 9227465

a, b = 0, 1
for _ in range(35):
    a, b = b, a + b

print(f"fib(35) = {a}")
