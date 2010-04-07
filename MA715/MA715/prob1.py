from sympy import *;

def u_exact(x):
	return exp(sin(x)) * exp(cos(x));
	
def q1(x):
	return sin(x) * cos(x);
	
def q2(x):
	return -2 * cos(x)**4 + cos(x)**3 + (8 + 3 * sin(x)) * cos(x)**2 - (1 + sin(x)) * cos(x);

def ode(u, x):
	ux = diff(u(x), x);
	uxx = diff(ux, x);
	uxxx = diff(uxx, x);
	return simplify(uxxx + q1(x) * uxx + q2(x) * u(x) - 3 * exp(sin(x)) * exp(cos(x)));

def check_exactness():
	print trigsimp(ode(u_exact, Symbol('x'))) == 0;
	
def p(x,xj,h):
	return sin(pi * (x - xj) / h) / (2 * pi / h * tan((x - xj) / 2));

def dot_prod(f, g, x):
	u = f * g;
	print u;
	return integrate(u, x);

def D(n):
	x = Symbol('x');
	h = Symbol('h');
	u = p(x, 0, h);
	for k in range(n):
		u = diff(u, x);
	return simplify(trigsimp(u));

