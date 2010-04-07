% CHEBFFT  Chebyshev differentiation via FFT. Simple, not optimal.  
%          If v is complex, delete "real" commands.

  function w = chebfft2(v)
  
  N = length(v)-1; 
  
  nn = [0:(N-1) 0 1-N:-1]';
  c = cos((1:N-1)'*pi/N);
  s = sqrt(1. - c.^2);
  
  
  V = [v; flipud(v(2:N))];
  B = real(fft(V));
  
  PT    = real(ifft(1i * nn .* B));
  PTT   = -real(ifft(nn .* nn .* B));
  
  w = zeros(N+1,1);
  w(2:N) = (s .* PTT(2:N) - c .* PT(2:N)) ./ (s.^3);
  w(1) = sum((nn.^4 - nn.^2) .* B ./ (6. * N));
  w(N+1) = sum((-1).^nn .* (nn.^4 - nn.^2) .* B ./ (6. * N));
  
