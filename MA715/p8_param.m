function [ res ] = p8_param( N, m )
  L = 8;                            
  h = 2*pi/N; x = h*(1:N); x = L*(x-pi)/pi;
  column = [-pi^2/(3*h^2)-1/6 ...
      -.5*(-1).^(1:N-1)./sin(h*(1:N-1)/2).^2];
  D2 = (pi/L)^2*toeplitz(column); 
  eigenvalues = sort(eig(-D2 + diag(x.^m)));
  res = eigenvalues(1:20);
end
