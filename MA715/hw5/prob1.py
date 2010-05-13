from numpy import *
from scipy import *
from scipy.linalg import *
from scipy.sparse import *
from scipy.linsolve import *
from pylab import *
import sympy as sp
import sympy.abc as abc
import sympy.integrals as spi

class QuadElement:
	def __init__(self, ni, nx, ny):
		self.ni = ni
		self.nx = [nx[k] for k in ni]
		self.ny = [ny[k] for k in ni]
		M = matrix([ [ 1, nx[k], ny[k], nx[k] * ny[k]] for k in ni ])
		self.alpha = M.inv()			
	
	def laplacian(self):
		res = []
		for i in range(len(ni)):
			for j in range(len(ni)):
				ali = self.alpha[i,1:3]
				alj = self.alpha[j,1:3]
				ahi = self.alpha[i,3]
				ahj = self.alpha[j,3]
				S = 0.
				for k in range(2,4):
					J = matrix([ [self.nx[p] - self.nx[0], self.ny[p] - self.ny[0]] for p in range(k-1,k+1) ])
					X = array([J[0,0] + J[0,1], J[1,0] + J[1,1])
					T = ali * alj + X * ((ali * ahj + alj * ahi) / 3. + X * ahi * ahj / 6.)
					S += sum(T) / (2. * det(J))
				res.append(((ni[i], nj[i]), S))
		return res
	
	'''
	def rhs(self, f):
		res = []
		for i in range(len(ni)):
			A = self.alpha[i,:]
			S = 0.
			for k in range(2,4):
				J = matrix([ [self.nx[p] - self.nx[0], self.ny[p] - self.ny[0]] for p in range(k-1,k+1) ])
				X = J[0,0] * abc.s + J[0,1] * abc.t
				Y = J[1,0] * abc.s + J[1,1] * abc.t
				F = f(X, Y)
				PHI = A[0] + A[1] * X + A[2] * Y + A[3] * X * Y
				S += sp.integrate(sp.integrate(F, (abc.s, 0, 1 - abc.t)), (abc.t, 0, 1)).n() / det(J)
			res.append((ni[i], S))
		return res
	'''
	
def fe_solve(mesh, M, f, boundary, bvals):
	A = dok_matrix((M, M))
	b = bvals * float(boundary)
	
	for e in mesh:
		for ((i,j), v) in e.laplacian():
			if(boundary[i]):
				continue
			A[i,j] += v
		for (i, v) in e.rhs(f):
			if(boundary[i]):
				continue
			b[i] += v
	
	return spsolve(A, b)

#Generates a rectangular regular grid mesh over the grid G
def gen_regular_mesh(G):
	#Get length
	R, C, D = grid.shape
	M = R * C
	def get_index(ix, iy):
		if(ix < 0 or ix >= R or \
		   iy < 0 or iy >= C):
			return -1
		return ix + R * iy

	nx = grid[:,:,0].flatten()
	ny = grid[:,:,1].flatten()
	mesh = []
	for ix in range(R-1):
		for iy in range(C-1):
			mesh.append(Element(        \
				[get_index(ix,   iy),   \ 
				 get_index(ix+1, iy),   \
				 get_index(ix+1, iy+1), \
				 get_index(ix,   iy+1)],\
				nx, ny))
	return mesh, get_index



#Do the mesh generation
xr = arange(-1:1:0.1)
yr = arange(-1:1:0.1)
grid, get_idx = mgrid(xr, yr)
mesh, M = gen_regular_mesh(G)

#Compute boundary conditions
boundary = zeros((M), 'bool')
for ix in range(len(xr)):
	boundary[get_idx(ix,0)] = True
	boundary[get_idx(ix,len(yr)-1)] = True
for iy in range(len(yr)):
	boundary[get_idx(0,ix)] = True
	boundary[get_idx(len(xr)-1,iy)] = True
bvals = zeros((M))

#Construct f
def f(x,y):
	

#Solve problem
v = fe_solve(mesh, M

#Display result
	


