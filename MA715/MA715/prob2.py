from scipy import *;

'''
def ode(u, x):
	return diff(diff(u(x), x), x) + 4 * diff(u(x),x) + exp(x) * u(x) - sin(8 * x);
	
'''

#Adapted from Trevethen's notes
def cheb(N):
	if N==0:
		return (0, 1);
	x = cos(pi * array(range(N+1)) / N);
	X = zeros((N+1,N+1));
	c = zeros((N+1));
	for ii in range(N+1):
		c[ii] = (-1.)**ii;
		X[:,ii] = x;
	c[0] *= 2.;
	c[N] *= 2.;
	dX = X - X.transpose();
	D = outer(c, 1. / c) / (dX + eye(N+1));
	D = D - diag(map(sum,D.transpose()));
	return (x, D);
	
def chebfft(v):
	N = len(v)-1;
	if N==0:
		return 0;
	x = cos(array(range(N+1))*pi/N);

	#FFT
	U = zeros((2*N + 1));
	U[:N] = v;
	U[N:] = v.reverse();
	U = real(fft(U));
	
	#Differentiate
	W = zeros(U.shape);	
	w = zeros((N+1));
	for k in range(N):
		W[k] *= 1.j * k;
		W[2*N - k] *= -1.j * (k+1);
		w[0] += (k*k) * U[k] / N;
		w[N] += (-1.)**(k+1) * k * k * U[k] / N
	W = real(ifft(W));	
	w[1:N] = -W[1:N] / sqrt(1. - x[1:N]**2.);
	w[0] += .5 * N * U[N];
	w[N] += .5 * (-1)**(N+1) * N * U[N];
	
	return w;

