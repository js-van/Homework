from numpy import *
from math import atan2
from pylab import *
from solver1 import *
from solver2 import *
import pickle

#Load mesh
fin = open("mesh.pkl", "rb")
mesh 	 = pickle.load(fin)
pts 	 = pickle.load(fin)
boundary = pickle.load(fin)
fin.close()

#Compute boundary conditions
def BC(x):
	r = norm(x)
	theta = atan2(x[1], x[0])
	if(r <= .251):
		return sin(5. * theta)
	return 0
bvals = array([ BC(v) for v in pts ])

#Set up RHS
def f(x,y):
	return 0

#Solve system
U, A, b = fe_solve(mesh, pts[:,0], pts[:,1], f, boundary, bvals)

#Solve the system and plot results
plot_mesh(mesh, U)
savefig("prob3_result.png")
show()

