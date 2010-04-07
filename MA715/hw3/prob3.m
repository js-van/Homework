

N = 32;


[D,x] = cheb(N);
D2 = D^2;

v = x .^ 3 + sin(x);

d2v_c = D2 * v;
d2v_ft = chebfft2(v);

plot(x, v, x, d2v_c, x, d2v_ft);




