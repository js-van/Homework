from sympy import *
from sympy.abc import *

def F(A, B):
	return integrate((sin(x) + cos(x) + 2 * sin(x) * cos(x)) * (x - A)**2 / (A - B)**2, (x, A, B))
	
	
#print collect(trigsimp(collect(simplify(F(a, b)), a)), b)
	
	

X = integrate(exp(sin(x) + cos(x)) * (x - a) / (a - b), (x, a, b))
print simplify(X)
