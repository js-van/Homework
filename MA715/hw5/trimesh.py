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
import sympy as sp
import sympy.abc as abc

'''
Generates a uniform triangular grid of sample points
'''
def gen_tri_grid(xmin, xmax, xstep):
	g = mgrid[xmin[0]:xmax[0]+xstep[0]:xstep[0],xmin[1]:xmax[1]+xstep[1]:xstep[1]]
	nx = g[0,:,:].flatten()
	ny = g[1,:,:].flatten()
	b0 = matrix([[cos(pi/3.)], [sin(pi/3.)]])
	b1 = matrix([[1.], [0]])
	return transpose(array(b0 * nx + b1 * ny))

'''	
Generates lambdas for the semianalytic constraint
'''
def make_lambdas(f_expr, X, Y):
	#Construct base lambda
	fn = sp.lambdify((X,Y), f_expr)
	f  = lambda x : fn(x[0], x[1])
	
	#Comput lagrange multiplier form/derivatives
	f_sq = f_expr**2
	f_dx = sp.diff(f_sq, X)
	f_dy = sp.diff(f_sq, Y)
	
	#Construct numerical functions
	fn_sq = sp.lambdify((X,Y), f_sq)
	fn_dx = sp.lambdify((X,Y), f_dx)
	fn_dy = sp.lambdify((X,Y), f_dy)
	
	fv      = lambda x : fn_sq(x[0], x[1])
	grad_fv = lambda x : -array([ \
		fn_dx(x[0], x[1]), \
		fn_dy(x[0], x[1])])

	return f, fv, grad_fv
	
'''	
Pushes the point p to the boundary
'''
def push_to_boundary(p, fv, grad_fv):
	return opt.fmin_ncg(fv, p, grad_fv, maxiter=400, disp=0)

'''	
Filters the points to lie within a semianalytic set defined by the function f. Points 'near' the boundary but exterior to f are pushed exactly onto the boundary by nonlinear optimization.
'''
def filter_points(pts, f, fv, grad_fv, cutoff):
	pz = zip(map(f, pts), pts)
	samples = [ x[1] for x in pz if x[0] <= 0 ]
	bstart = len(samples)
	for p in [ x[1] for x in pz if (x[0] > 0 and x[0] <= cutoff) ]:
		v = push_to_boundary(p, fv, grad_fv)
		if(abs(f(v)) < 1e-8):
			samples.append(v)
	return array(samples)

'''
Generates a mesh from a set of base sample points using delaunay triangulation.  Removes edges which cross outside boundary.  Low quality elements near the boundary are also killed
'''
def make_tri_mesh(pts, f, cutoff = 0.):
	M = pts.shape[0]
	dtri = Triangulation(pts, 2)
	mesh = []
	for t in dtri.get_elements_indices():
		edges = [ array([pts[t[k]], pts[t[(k+1)%3]]]) for k in range(3) ]
		#Check for edges that cross outside region
		good = True
		for e in edges:
			m = .5 * (e[0] + e[1])
			if(f(m) >= cutoff):
				good = False
				break
		if(not good):
			continue
			
		#Add element
		mesh.append(TriElement(t, pts[:,0], pts[:,1]))
	return mesh
	
'''
Removes bad elements
'''
def refine_mesh(pts, mesh, f, fv, grad_fv, qcutoff):
	M = pts.shape[0]
	bad_pt = zeros((M))
	npts = []
	min_q = 1e10
	for t in mesh:
		if(any([ bad_pt[k] for k in t.ni ])):
			continue
		if(t.quality() < qcutoff):
			min_q = min(min_q, t.quality())
			v = array([ pts[k] for k in t.ni ])
			v = array([sum(v[:,0]), sum(v[:,1])]) / len(t.ni)
			if(any([ abs(f(pts[k])) < 1e-8 for k in t.ni ])):
				v = push_to_boundary(v, fv, grad_fv)
			npts.append(v)
			for k in t.ni:
				bad_pt[k] = 1
	npts.extend([ p for (i,p) in enumerate(pts) if not bad_pt[i] ])
	return array(npts), sum(bad_pt)

'''	
Draw a wire frame of a triangle mesh
'''
def wire_plot_mesh(mesh, pts, color='#000000'):
	for poly in mesh:
		v = array([ pts[k] for k in poly.ni ])
		edges = [ array([ v[k], v[k-1] ]) for k in range(3) ]
		for e in edges:
			plot(e[:,0], e[:,1], color=color)

