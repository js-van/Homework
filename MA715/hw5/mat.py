from sympy import *


a2i = Symbol("a2i")
a4i = Symbol("a4i")

a2j = Symbol("a2j")
a4j = Symbol("a4j")

X1  = Symbol("X1")
Y1  = Symbol("Y1")

J11 = Symbol("J11")
J12 = Symbol("J12")
J21 = Symbol("J21")
J22 = Symbol("J22")

lam1 = Symbol("Lambda1")
lam2 = Symbol("Lambda2")

X = (J11 * lam1 + J12 * lam2) + X1
Y = (J21 * lam1 + J22 * lam2) + Y1

Q0 = a2i * a2j
Q1 = a2i * a4j + a4i * a2j
Q2 = a4i * a4j


Expr = Q0 + Q1 * Y + Q2 * Y**2

I1 = integrate(Expr, (lam1, 0, 1 - lam2))
I2 = integrate(I1, (lam2, 0, 1))

#print collect(I2, (J21 + J22))


JX = J21 + J22
T = (Q0 + Y1 * (Q1 + Y1 * Q2) + JX / 3 * (Q1 + (2 * Y1 + JX / 2) * Q2)) / 2 - (J21 * J22 * Q2) / 12

#print T
print simplify(I2 - T)


