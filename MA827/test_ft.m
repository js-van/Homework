function [ output_args ] = test_ft( res )

N = 2 * res + 1

f = zeros(1, N);
for i=2:res
    f(i) = 1./ log(i);
    f(N + 2 - i) = - 1. / log(i);
end

figure,plot(f)
figure,plot(abs(fft(f)))


end
