from scipy import *
from scipy.sparse import *
from scipy.linsolve import *
from pylab import *
import sympy as sp
import sympy.abc as abc
import sympy.integrals as spi

def fe_solve(x, F2, F0, FR, X):
	M = len(x)
	A = dok_matrix((M, M))
	b = zeros(M)
	
	def delta(i,j):
		return int((i + M)%M == (j+M)%M)

	def p2(i, j, xi0, xi1, xi2):
		return F2 * (delta(i, j+1) - delta(i,j)) / (xi1 - xi0) + \
			   F2 * (delta(i, j-1) - delta(i,j)) / (xi2 - xi1)

	def p0(i, j, xi0, xi1, xi2):
		t1 = spi.Integral(F0 * (X - xi0) * ( \
			  delta(i,j)   * (X - xi0) \
			+ delta(i,j+1) * (xi1 - X)), (X, xi0, xi1)).n()  \
			/ (xi1 - xi0)**2
		t2 = spi.Integral(F0 * (xi2 - X) * ( \
			  delta(i,j)   * (xi2 - X) \
			+ delta(i,j-1) * (X - xi1)), (X, xi1, xi2)).n()  \
			/ (xi2 - xi1)**2
		return t1 + t2

	def f(i, xi0, xi1, xi2):
		return spi.Integral(FR * (X - xi0), (X, xi0, xi1)).n()  / (xi1 - xi0) \
			 + spi.Integral(FR * (xi2 - X), (X, xi1, xi2)).n()  / (xi2 - xi1)

	for i in range(M):	
		#Handle periodicity conditions
		xi0 = x[i-1]
		xi1 = x[i]
		xi2 = x[(i+1) % len(x)]
		if(xi0 > xi1):
			xi0 -= 2 * pi
		if(xi1 > xi2):
			xi2 += 2 * pi

		#Construct A
		for j in range(M):
			t = p2(i, j, xi0, xi1, xi2) + p0(i, j, xi0, xi1, xi2)
			if(abs(t) > 1e-6):
				A[i,j] = float(t)
		
		#Construct b
		b[i] = f(i, xi0, xi1, xi2)

	return spsolve(A, b)

#Use sympy to create symbolic coefficients
X = abc.x
F2 = 1 		# Must be a scalar
F0 = sp.sin(X) + sp.cos(X) + sp.sin(2 * X)
FR = sp.exp(sp.sin(X) + sp.cos(X))

#Generate grid and solve U
x = arange(-pi, pi, 2. * pi / 30.)
U = fe_solve(x, F2, F0, FR, X)

#Compute exact soln. for comparison
Uex = exp(sin(x) + cos(x))

#Plot
plot(x, U)
plot(x, Uex)
show()
