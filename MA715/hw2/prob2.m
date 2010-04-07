
  N = 64;
  
  [D,x] = cheb(N);        
  D2 = D^2;              
  D2 = D2(2:N,2:N);                   % boundary conditions
  f = sin(8. * x(2:N));
  
  D0 = diag(exp(x(2:N)));
  D1 = 4. * D(2:N,2:N);
  M = D2 + D1 + D0;
  
  u = M\f;                           % Poisson eq. solved here
  u = [0;u;0];              
  
  
  clf, subplot('position',[.1 .4 .8 .5])
  plot(x,u,'.','markersize',16)
  xx = -1:.01:1;
  uu = polyval(polyfit(x,u,N),xx);    % interpolate grid data
  line(xx,uu);
  
  grid on
%  exact = ( exp(4*xx) - sinh(4)*xx - cosh(4) )/16; 
%  title(['max err = ' num2str(norm(uu-exact,inf))],'fontsize',12)

