from scipy import *
from scipy.linalg import *
from scipy.sparse import *
from scipy.linsolve import *
from numpy import *
from pylab import *

M = 19
h = 1. / float(M+1.)
x = arange(h, 1., h)
xbig = hstack(([0.], x, [1.]))

# Construct A matrix:
e = ones((M))
A = spdiags([-e, 2*e, -e], range(-1,2), M, M)

# RHS functions
def fun1(x, a):
	return (x-a)*10.*(1.-10.*(x-0.5)**2)*exp(-5.*(x-0.5)**2)

# Construct RHS vector
b = zeros(M)
for i in range(M):

	#Add entries to A for extra points
	#Adjust b

    tmp1 = (fun1(xbig[i+1],xbig[i]  ) + fun1(xbig[i],  xbig[i])  )*.5*h
    tmp2 = (fun1(xbig[i+1],xbig[i+2]) + fun1(xbig[i+2],xbig[i+2]))*.5*h
    b[i] = tmp1 - tmp2

print A
print b

print A.shape
print b.shape

# Solve linear system
U = spsolve(A, b)
Ubig = hstack(([0.], U, [0.]))

print Ubig

# Exact solution
Uex = exp(-(5./4.)*(2.*xbig-1)**2) - exp((-5.)/4.)

print Uex

# Plot solution
plot(xbig, Ubig)
plot(xbig, Uex)
show()
