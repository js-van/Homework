from trimesh import *

#Construct semianalytic variety f using R functions to minimize singularities
def f(x):
	rs = sum(x*x)
	r = sqrt(rs)
	theta = atan2(x[1], x[0])
	fo = r - .75 - .25 * sin(5. * theta)
	fi = (.25)**2 - rs
	return fo + fi + sqrt(fo**2 + fi**2)

#Make grid
hx = 0.05
hy = 0.05
grid = mgrid[-1:1+hx:hx,-1:1+hy:hy]
nx = grid[0,:,:].flatten()
ny = grid[1,:,:].flatten()
M = len(nx)

#Adjust point samples
pts = transpose(array([nx, ny])) + uniform(-0.005, 0.005, (M,2))
pts, L = filter_points(pts, f, 0.2, 0.0)

#Generate mesh
mesh = make_tri_mesh(pts, f, 0.04)

wire_plot_mesh(mesh, pts)
savefig('prob2_result.png')
show()	
