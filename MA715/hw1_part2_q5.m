xfunction [ D, g, gg ] = hw1_part2_q5( n, s )


D = zeros(s, 1);

for j=1:(n+1)
    v = (j-1);
    jj = mod(ceil(n/2) - v + 2*s, s);
    nj = mod(jj + s - 1, s);
    cc = (-1)^v * nchoosek(n,v);
    
    if(mod(n, 2) == 1)
        D(jj+1) = D(jj+1) + .5 * cc;
        D(nj+1) = D(nj+1) + .5 * cc;
    else
        D(jj+1) = cc;
    end
end

g = zeros(s, 1);
for kk=1:s
    k = kk - 1;
%    x = 0.;
%    for jj=1:(n+1)
%        j = jj - 1;
%        x = x + nchoosek(n, j) * exp(sqrt(-1) * (k + .5 * s) * j * 2 * pi / s);
%    end
%    g(kk) = x * exp(-.5 * sqrt(-1) * k * n * 2 * pi / s);
    g(kk) = exp(-.5 * sqrt(-1) * k * n * 2 * pi / s) * (1 - exp(sqrt(-1)*k * 2 * pi / s))^n;
end

gg = fft(D);

end
