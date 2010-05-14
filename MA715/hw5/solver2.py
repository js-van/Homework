from numpy import *
from scipy import *
from scipy.linalg import *
from scipy.sparse import *
from scipy.linsolve import *

class TriElement:

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
				S = 0.
				#Compute laplacian coefficient
				res.append(((self.ni[i], self.ni[j]), S))
		return res

