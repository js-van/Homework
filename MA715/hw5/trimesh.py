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

#Generates a uniform triangular grid
def gen_tri_grid(xmin, xmax, xstep):
	g = mgrid[xmin[0]:xmax[0]+xstep[0]:xstep[0],xmin[1]:xmax[1]+xstep[1]:xstep[1]]
	nx = g[0,:,:].flatten()
	ny = g[1,:,:].flatten()
	b0 = matrix([[cos(pi/3.)], [sin(pi/3.)]])
	b1 = matrix([[1.], [0]])
	return transpose(array(b0 * nx + b1 * ny))
	
#Filters the points to lie within a semianalytic set defined by the function f
#Points 'near' the boundary but exterior to f are pushed exactly onto the boundary
#by nonlinear optimization
def filter_points(pts, f, cutoff, lcutoff=None):
	if(lcutoff == None):
		lcutoff = 0.
	pz = zip(map(f, pts), pts)
	samples = [ x[1] for x in pz if x[0] <= lcutoff ]
	L = len(samples)
	for p in [ x[1] for x in pz if (x[0] > lcutoff and x[0] <= cutoff) ]:
		p0 = array([p[0], p[1], 0.], 'f')
		v = opt.fmin((lambda x : x[2] * f(x[:2]) + norm(x[:2] - p)), p0, maxiter=300, maxfun=300)
		pp = v[:2]
		print pp - p
		if(norm(pp) > 2.):
			continue
		samples.append(pp)
	return array(samples), L

#Generates a mesh from a set of base sample points using delaunay triangulation
#Removes edges which cross outside boundary
def make_tri_mesh(pts, f, cutoff = 0.):
	M = pts.shape[0]
	dtri = Triangulation(pts, 2)
	mesh = []
	for t in dtri.get_elements_indices():
		edges = [ array([pts[t[k]], pts[t[(k+1)%3]]]) for k in range(3) ]
		good = True
		for e in edges:
			m = .5 * (e[0] + e[1])
			if(f(m) > cutoff):
				good = False
				break
		if(not good):
			continue
		mesh.append(TriElement(t, pts[:,0], pts[:,1]))
	return mesh
	
#Draw a wire frame plot of a triangle mesh
def wire_plot_mesh(mesh, pts, color='#000000'):
	for poly in mesh:
		v = array([ pts[k] for k in poly.ni ])
		edges = [ array([ v[k], v[k-1] ]) for k in range(3) ]
		for e in edges:
			plot(e[:,0], e[:,1], color=color)

