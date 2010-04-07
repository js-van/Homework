% p6.m - variable coefficient wave equation
function [res, a, b, m] = p6_param(T,N)
  h = 2*pi/N; x = h*(1:N); t = 0; dt = h/4;
  c = .2 + sin(x-1).^2;
  v = exp(-100*(x-1).^2); 

  data = [v];
  vold = exp(-100*(x-.2*dt-1).^2);
  
  for i = 1:floor(T / dt)
      t = t+dt;
      v_hat = fft(v);
      w_hat = 1i*[0:N/2-1 0 -N/2+1:-1] .* v_hat;
      w = real(ifft(w_hat)); 
      vnew = vold + 2*dt*c.*w; 
      vold = v; 
      v = vnew;
      data(i+1,:) = v;
  end
  res = data;
  a = data(1,:);
  b = data(size(data,1),:);
  m = max(abs(a-b));
end