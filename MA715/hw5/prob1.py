from numpy import *
from pylab import *
from solver1 import *

#Do the mesh generation
hx = 0.01
hy = 0.01
grid  = mgrid[-1:1+hx:hx,-1:1+hy:hy]
mesh, nx, ny, M, R, C, get_idx = gen_regular_quad_mesh(grid)

#Compute boundary conditions
boundary = zeros((M), 'bool')
for ix in range(C):
	boundary[get_idx(ix,0)] = True
	boundary[get_idx(ix,R-1)] = True
for iy in range(R):
	boundary[get_idx(0,iy)] = True
	boundary[get_idx(C-1,iy)] = True
bvals = zeros((M))

#Construct f
def f(x,y):
	if(sqrt(x*x + y*y) < 0.2):
		return 100.
	return 1.

#Solve problem
u = fe_solve(mesh, nx, ny, f, boundary, bvals)

#Display result
X = nx.reshape(R, C)
Y = ny.reshape(R, C)
U = u.reshape(R, C)
pcolor(X, Y, U)
savefig("prob1_result.png")
show()

