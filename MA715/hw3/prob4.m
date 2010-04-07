
% Set up grid and two-soliton initial data:
  N = 64; 
  eps = 0.25;
  
  dt = .4/N^2; 
  x = (2*pi/N)*(-N/2:N/2-1)';
  
  A = 25; 
  B = 16; 
  clf, drawnow, set(gcf,'renderer','zbuffer')
  
  %Initial conditions
  u = zeros(N,1);
  u(1:N/2) = sin(x(1:N/2)).^2;
  
  v = fft(u); 
  k = [0:N/2-1 0 -N/2+1:-1]'; 
  ik2 = -eps * k.^2;

% Solve PDE and plot results:
  tmax = 3.0; 
  nplt = floor((tmax/25)/dt); 
  nmax = round(tmax/dt);
  udata = u; 
  tdata = 0; 
  h = waitbar(0,'please wait...');
  
  for n = 1:nmax
    t = n*dt; 
    g = -.5i*dt*k;
    E = exp((.5*dt)*ik2); 
    E2 = E.^2;
    
    a = g.*fft(real( ifft(     v    ) ).^2);
    b = g.*fft(real( ifft(E.*(v+a/2)) ).^2);     % 4th-order
    c = g.*fft(real( ifft(E.*v + b/2) ).^2);     % Runge-Kutta
    d = g.*fft(real( ifft(E2.*v+E.*c) ).^2);
    v = E2.*v + (E2.*a + 2*E.*(b+c) + d)/6;
    if mod(n,nplt) == 0 
      u = real(ifft(v)); waitbar(n/nmax)
      udata = [udata u]; tdata = [tdata t];
    end
  end
  waterfall(x,tdata,udata'), colormap(1e-6*[1 1 1]); view(-20,25)
  xlabel x, ylabel t, axis([-pi pi 0 tmax 0 5]), grid off
  set(gca,'ztick',[0 2000]), close(h), pbaspect([1 1 .13])
