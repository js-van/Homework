from numpy import *
from math import atan2
from pylab import *
from solver1 import *
from solver2 import *
import scipy.sparse.linalg.eigen.arpack as arp
import pickle

#Load mesh
fin = open("mesh.pkl", "rb")
mesh 	 = pickle.load(fin)
pts 	 = pickle.load(fin)
boundary = pickle.load(fin)
fin.close()

#Construct matrix
U, A, b = fe_solve(mesh, pts[:,0], pts[:,1], lambda x,y:0, boundary, zeros((len(pts))))

#Compute top 4 Eigen values of A
w, v = arp.eigen(A, 4)

#Plot results
for k in range(4):
	clf()
	plot_mesh(mesh, abs(v[:,k]))
	savefig("prob4_eig" + str(k) + ".png")
show()

