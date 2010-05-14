from trimesh import *

#Construct semianalytic variety f using R functions to minimize singularities
def f(x):
	rs = sum(x*x)
	r = sqrt(rs)
	theta = atan2(x[1], x[0])
	fo = r - .75 - .25 * sin(5. * theta)
	fi = 2. * ((.25)**2 - rs)
	return fo + fi + sqrt(fo**2 + fi**2)

#Make grid
pts = gen_tri_grid([-2,-2],[2,2],[0.05,0.05])
pts += uniform(-0.002, 0.002, pts.shape)
pts, L = filter_points(pts, f, 0.01, -0.04)

scatter(pts[:,0], pts[:,1])
'''
#Generate mesh
mesh = make_tri_mesh(pts, f, 0.01)

wire_plot_mesh(mesh, pts)
savefig('prob2_result.png')
'''
show()	

