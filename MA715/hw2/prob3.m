
  N = 36;

  [D,x] = cheb(N);
  
  %Perform change of variables
  D = 2. * D;
  x = .5 * (x + 1);
  
  %Set up eigenvalue problem
  sigma = 1. + x;
  
  %Build operator
  D2 = D^2; 
  D2 = D2(2:N,2:N);
  L = diag( 1 ./ sigma(2:N) ) * D2;
 
  [V,Lam] = eig(L); 
  lam = diag(Lam);
  [foo,ii] = sort(-lam);          % sort eigenvalues and -vectors
  lam = lam(ii); V = V(:,ii); clf
  
  
  for j = 1:4
    u = [0;V(:,j);0]; subplot(7,1,j)
    plot(x,u,'.','markersize',12), grid on
    xx = 0:.01:1; 
    uu = polyval(polyfit(x,u,N),xx);
    line(xx,uu)
    %text(-.4,.5,sprintf('eig %d =%20.13f*4/pi^2',j,lam(j)*4/pi^2))
    %text(.7,.5,sprintf('%4.1f  ppw', 4*N/(pi*j)))
  end  

  
  lam(1:7)