  N = 64;
  [D,x] = cheb(N); 
  D2 = D^2; 
  D2 = D2(2:N,2:N);
  
  delta_t = 0.000001;

  M = inv(eye(N-1) - .5 * delta_t * D2) * (eye(N-1) + .5 * delta_t * D2);
  v = zeros(N-1,1);
  it = 0;
  
  for t = 0:delta_t:3.55
    vs  = log( 2. * exp(v) ./ (2. - exp(v) * delta_t) );
    vss = M * vs;
    vnew = log( 2. * exp(vss) ./ (2. - exp(vss) * delta_t));
    
    disp (t);
    disp (v(N/2+1));
    
    
    if(5.1 < v(N/2 + 1))
        break;
    end
    
    v = vnew;
    it = it+1;
  end
  
  u = v;
  u = [0;u;0];
  clf, subplot('position',[.1 .4 .8 .5])
  plot(x,u,'.','markersize',16)
  xx = -1:.01:1;
  uu = polyval(polyfit(x,u,N),xx);
  line(xx,uu), grid on
  title(sprintf('no. steps = %d      u(0) =%18.14f',it,u(N/2+1)))
