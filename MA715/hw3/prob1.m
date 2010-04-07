
N = 1024;
K = 64;
P = 16;

fft_times = zeros(N,1);
bf_times = zeros(N,1);
data = zeros(K,1);

for n=2:N
    
    [D,x] = cheb(n);
    
    for k = 1:K
        tic;
        for p = 1:P
            chebfft(x);
        end
        data(k) = toc / P;
    end
    fft_times(n) = median(data);
    
    for k = 1:K
        tic;
        for p = 1:P
            xx = D * x;
        end
        data(k) = toc / P;
    end
    bf_times(n) = median(data);
    
end

plot( 2:N, fft_times(2:N), 2:N, bf_times(2:N) );