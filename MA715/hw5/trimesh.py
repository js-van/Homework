from numpy import *
from numpy.random import uniform
from scipy import *
from math import atan2
from scipy.linalg import *
from scipy.sparse import *
from scipy.linsolve import *
import scipy.optimize as opt
from delaunay import Triangulation
from solver2 import TriElement
from pylab import *

#Filters the points to lie within a semianalytic set defined by the function f
#Points 'near' the boundary but exterior to f are pushed exactly onto the boundary
#by nonlinear optimization
def filter_points(pts, f, cutoff, lcutoff=None):
	if(lcutoff == None):
		lcutoff = -.5 * cutoff
	pz = zip(map(f, pts), pts)
	samples = [ x[1] for x in pz if x[0] <= lcutoff ]
	L = len(samples)
	for p in [ x[1] for x in pz if (x[0] > lcutoff and x[0] <= cutoff) ]:
		p0 = array([p[0], p[1], 1.], 'f')
		v = opt.fmin((lambda x : x[2] * f(x[:2])**2 + norm(x[:2] - p)), p0, maxiter=1000, maxfun=1000)
		pp = v[:2]
		if(abs(f(pp)) <= 0.01 * cutoff):
			samples.append(pp)
	return array(samples), L

#Generates a mesh from a set of base sample points using delaunay triangulation
#Removes edges which cross outside boundary
def make_tri_mesh(pts, f):
	M = pts.shape[0]
	dtri = Triangulation(pts, 2)
	mesh = []
	for t in dtri.get_elements_indices():
		edges = [ array([pts[t[k]], pts[t[(k+1)%3]]]) for k in range(3) ]
		good = True
		for e in edges:
			m = .5 * (e[0] + e[1])
			if(f(m) > 0):
				good = False
				break
		if(not good):
			continue
		mesh.append(TriElement(t, pts[:,0], pts[:,1]))
	return mesh
	
#Draw a wire frame plot of a triangle mesh
def wire_plot_mesh(mesh, pts, color):
	for poly in mesh:
		v = array([ pts[k] for k in poly.ni ])
		edges = [ array([ v[k], v[k-1] ]) for k in range(3) ]
		for e in edges:
			plot(e[:,0], e[:,1], color=color)


#Construct semianalytic variety f using R functions to minimize singularities
def f(x):
	rs = sum(x*x)
	r = sqrt(rs)
	theta = atan2(x[1], x[0])
	fo = r - .75 - .25 * sin(5. * theta)
	fi = (.25)**2 - rs
	return fo + fi + sqrt(fo**2 + fi**2)

'''
hx = 0.01
hy = 0.01
grid = mgrid[-1:1+hx:hx,-1:1+hy:hy]
nx = grid[0,:,:]
ny = grid[1,:,:]
R,C = nx.shape
U = zeros((R,C))

for i in range(R):
	for j in range(C):
		U[i,j] = f(grid[:,i,j])  <= 0

pcolor(nx, ny, U)
show()
'''

'''
Npts = 1000
pts = uniform(-1., 1., (Npts, 2))
pts, L = filter_points(pts, f, 0.1)
print pts
scatter(pts[:,0], pts[:,1], s=1)
show()
'''

hx = 0.05
hy = 0.05
grid = mgrid[-1:1+hx:hx,-1:1+hy:hy]
nx = grid[0,:,:].flatten()
ny = grid[1,:,:].flatten()
M = len(nx)
pts = transpose(array([nx, ny])) + uniform(-0.005, 0.005, (M,2))
pts, L = filter_points(pts, f, 0.06)
mesh = make_tri_mesh(pts, f)

wire_plot_mesh(mesh, pts, '#000000')
show()	

#scatter(pts[:,0], pts[:,1], s=1)
#show()

