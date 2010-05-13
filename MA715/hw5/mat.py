from sympy import *


a2i = Symbol("a2i")
a4i = Symbol("a4i")

a2j = Symbol("a2j")
a4j = Symbol("a4j")

J11 = Symbol("J11")
J12 = Symbol("J12")
J21 = Symbol("J21")
J22 = Symbol("J22")

lam1 = Symbol("Lambda1")
lam2 = Symbol("Lambda2")

X = (J11 * lam1 + J12 * lam2)
Y = (J21 * lam1 + J22 * lam2)

Expr = a2i * a2j + (a4i * a2j + a2i * a4j) * Y + a4i * a4j * Y**2

I1 = integrate(Expr, (lam1, 0, 1 - lam2))
I2 = integrate(I1, (lam2, 0, 1))

print I2


