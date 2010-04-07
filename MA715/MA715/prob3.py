from sympy import *;

def test():
	phi = Function('y');
	sigma = Function('s');
	x = Symbol('x');
	lam = Symbol('l');
	t = Symbol('t');

	def soln(x):
		return sigma(x)**(-.25) * sin( sqrt(lam) * integrate(sqrt(phi(t)), (t, 0, x)));

	ode = diff(diff(phi(x), x), x) + lam * sigma(x) * phi(x);
	ode_s = ode.subs(phi, soln);

	print simplify(ode_s);


