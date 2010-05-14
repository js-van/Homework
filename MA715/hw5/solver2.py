from numpy import *
from scipy import *
from math import acos
from scipy.linalg import *
from scipy.sparse import *
from scipy.linsolve import *
from pylab import *

class TriElement:
	def __init__(self, ni, nx, ny):
		self.ni = ni
		self.nx = [nx[k] for k in ni]
		self.ny = [ny[k] for k in ni]
		M = matrix([ [ 1, nx[k], ny[k] ] for k in ni ])
		self.alpha = inv(transpose(M))
		
	def quality(self):
		theta = []
		for k in range(3):
			d = [array([self.nx[(k+p)%3] - self.nx[k], \
					    self.ny[(k+p)%3] - self.ny[k]]) for p in range(1,3)]
			d = [v / norm(v) for v in d]
			theta.append(acos(sum(d[0] * d[1])))
		return min(theta)

	
	def laplacian(self):
		res = []
		for i in range(len(self.ni)):
			for j in range(len(self.ni)):
				ali = array(self.alpha[i,1:3]).flatten()
				alj = array(self.alpha[j,1:3]).flatten()
				J = matrix([ [self.nx[p] - self.nx[0], self.ny[p] - self.ny[0]] for p in range(1,3) ])
				S = -sum(ali * alj) / (2. * det(J))
				print S
				res.append(((self.ni[i], self.ni[j]), S))
		return res


'''
Draws the mesh (not very good right now)
'''
def plot_mesh(mesh, U):
	umin = min(U)
	umax = max(U)
	s = 1. / (umax - umin)
	def get_color(v):
		return str((v - umin) * s)
	for ele in mesh:
		fill(ele.nx, ele.ny, color=get_color(sum([ U[k] for k in ele.ni ])/3.) )
