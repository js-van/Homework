from trimesh import *
import sympy as sp
from sympy.abc import X, Y

#Construct semianalytic variety f using R functions to minimize singularities
def F(X, Y):
	rs = X * X + Y * Y
	r = sp.sqrt(rs)
	c = X / (r + 0.001)
	s = Y / (r + 0.001)
	fo = r - .75 - .25 * (5 * s**4 * c - 10 * c**3 * s**2 + c**5)
	fi = 2. * ((.25)**2 - rs)
	return fo + fi + sp.sqrt(fo**2 + fi**2)
	
	
#Make lambdas
f, fv, grad_fv = make_lambdas(F(X, Y), X, Y)

#Make grid
pts = gen_tri_grid([-2,-2],[2,2],[0.05,0.05])
pts += uniform(-0.002, 0.002, pts.shape) #jitter points a bit
pts = filter_points(pts, f, fv, grad_fv, 0.05)

#Generate mesh
nbad_elements = 1
while(nbad_elements > 0):
	mesh = make_tri_mesh(pts, f, 0.01)
	pts, nbad_elements = refine_mesh(pts, mesh, f, fv, grad_fv, pi/20.)


#Save to file

wire_plot_mesh(mesh, pts)
savefig('prob2_result.png')
show()	

