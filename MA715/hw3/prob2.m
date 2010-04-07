  N = 50; 
  dt = 6/N^2;
  
  %Construct chebyshev matrix
  [D, x] = cheb(N);
  y = x';
  D2 = dt^2 * (D * D);
  D2(1,:) = 0;
  D2(N+1,:) = 0;
  I = eye(N+1);
  L = kron(I, D2) + kron(D2, I);  
  
  [xx,yy] = meshgrid(x,y);
  plotgap = round((1/3)/dt); dt = (1/3)/plotgap;
  vv = exp(-40*((xx-.4).^2 + yy.^2));
  vvold = vv; 
  
  [ay,ax] = meshgrid([.56 .06],[.1 .55]); clf
  for n = 0:3*plotgap
    t = n*dt;
    if rem(n+.5,plotgap)<1     % plots at multiples of t=1/3
      i = n/plotgap+1;
      subplot('position',[ax(i) ay(i) .36 .36])
      [xxx,yyy] = meshgrid(-1:1/16:1,-1:1/16:1);
      vvv = interp2(xx,yy,vv,xxx,yyy,'cubic');
      mesh(xxx,yyy,vvv), axis([-1 1 -1 1 -0.15 1])
      colormap(1e-6*[1 1 1]); title(['t = ' num2str(t)]), drawnow
    end
    
    vvnew = 2*vv - vvold + reshape(L * reshape(vv, (N+1)^2, 1), N+1,N+1);
    vvold = vv; vv = vvnew;
  end
