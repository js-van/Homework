function [ D ] = sinc_d( N, d )

k = [0:(N/2) (1-N/2):-1];

ch = ones(1,N);
chd = (i * k).^d .* ch;
dc = real(ifft(chd));
D = toeplitz(dc, dc([1 N:-1:2]));

end
