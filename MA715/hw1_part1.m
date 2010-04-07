function [ x, C ] = hw1_part1( N )

B = zeros(N, 5);
C = zeros(N, 1);

for k=1:N
    B(k,1) = -.5*i;
    B(k,2) = .5*(1 - i);
    B(k,3) = -(k-floor(N/2)-1)^2;
    B(k,4) = .5*(1 + i);
    B(k,5) = .5*i;
    t = (k - 1) * 2. * pi / N;
    C(k) = exp(cos(t)) * exp(sin(t));
end

A = fftshift(fft(C));
M = spdiags(B, -2:2, N, N);
x = real(ifft(ifftshift(M \ A)));

end
