from numpy import *
from scipy import *
from scipy.linalg import *
from scipy.sparse import *
from scipy.linsolve import *
from pylab import *

#A quadrilateral element
class QuadElement:
	def __init__(self, ni, nx, ny):
		self.ni = ni
		self.nx = [nx[k] for k in ni]
		self.ny = [ny[k] for k in ni]
		M = matrix([ [ 1, nx[k], ny[k], nx[k] * ny[k]] for k in ni ])
		self.alpha = inv(transpose(M))
		
	def laplacian(self):
		res = []
		for i in range(len(self.ni)):
			for j in range(len(self.ni)):
				ali = array(self.alpha[i,1:3]).flatten()
				alj = array(self.alpha[j,1:3]).flatten()
				ahi = self.alpha[i,3]
				ahj = self.alpha[j,3]
				Q0 = ali * alj
				Q1 = ali * ahj + alj * ahi
				Q2 = ahi * ahj
				S = 0.
				for k in range(2,4):
					J = matrix([ [self.nx[p] - self.nx[0], self.ny[p] - self.ny[0]] for p in range(k-1,k+1) ])
					X = array([J[1,0] + J[1,1], J[0,0] + J[0,1]])
					Y = array([self.ny[0], self.nx[0]])
					T = Q0 + Y * (Q1 + Y * Q2) + X / 3. * (Q1 + (2 * Y + X / 2.) * Q2) - Q2 * array([J[1,0]*J[1,1], J[0,0]*J[0,1]]) / 12.
					S -= sum(T) / (2. * det(J))
				res.append(((self.ni[i], self.ni[j]), S))
		return res
	
#Solves the finite element problem for the given mesh (which is just a list of elements)
def fe_solve(mesh, nx, ny, f, boundary, bvals):
	M = len(nx)
	A = dok_matrix((M, M))
	b = zeros((M))
	for e in mesh:
		for ((i,j), v) in e.laplacian():
			if(boundary[i]):
				continue
			A[i,j] += v
	for i in range(M):
		if(boundary[i]):
			b[i] = bvals[i]
			A[i,i] = 1
		else:
			b[i] = f(nx[i], ny[i])
	return spsolve(A, b)
	
#Generates a rectangular regular grid mesh over the grid G
def gen_regular_quad_mesh(grid):
	D, R, C = grid.shape
	M = R * C
	def get_index(ix, iy):
		if(ix < 0 or ix >= R or \
		   iy < 0 or iy >= C):
			return -1
		idx = ix + R * iy
		return idx
	nx = grid[0,:,:].flatten()
	ny = grid[1,:,:].flatten()
	mesh = []
	for ix in range(R-1):
		for iy in range(C-1):
			mesh.append(QuadElement(    \
				[get_index(ix,   iy),   \
				 get_index(ix+1, iy),   \
				 get_index(ix+1, iy+1), \
				 get_index(ix,   iy+1)],\
				nx, ny))
	return mesh, nx, ny, M, R, C, get_index

#Do the mesh generation
hx = 0.05
hy = 0.05
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

