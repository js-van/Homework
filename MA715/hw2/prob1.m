
N = 64;

x = (0:N-1) * 2 * pi / N;
D0 = sinc_d(N, 0);
D1 = sinc_d(N, 1);
D2 = sinc_d(N, 2);
D3 = sinc_d(N, 3);

s = sin(x);
c = cos(x);

q1 = s .* c;
q2 = -2.*c.^4 + c.^3 + (8. + 3. * s) .* c.^2 - (1. + s) .* c;
f = 3. * exp(s) .* exp(c);

L = D3 + diag(q1) * D2 + diag(q2);
u = L \ f';


exact = exp(s) .* exp(c);
plot(x, u, x, exact);
plot(x, D0(1,:), x, D1(1,:), x, D2(1,:), x, D3(1,:));
%plot(x, D1(1,:), x, .5 * (-1).^(0:N-1) .* cot(x / 2))
%plot(x, D2(1,:), x, .5 * (-1).^(1:N) ./ (sin(x / 2).^2))



