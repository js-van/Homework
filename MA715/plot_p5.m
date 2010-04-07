function [ output_args ] = plot_p5( N, h )

x = [-floor(N/2):floor(N/2)];


K2 = [1 0 -1] ./ 2;
K4 = [-1 8 0 -8 1] ./ 12.;

g2 = zeros(N,1);
g2(N/2:N/2+2) = K2;
g2 = fftshift(g2);
g2 = fftshift(fft(g2));

g4 = zeros(N,1);
g4(N/2-1:N/2+3) = K4;
g4 = fftshift(g4);
g4 = fftshift(fft(g4));

ginf = x .* 1j * 2 * pi / N;

figure,plot(x,imag(g2),x,imag(g4),x,imag(ginf));



end
