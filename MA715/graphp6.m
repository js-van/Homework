
i = 1

x = []
y = []

for n=32:32:512
    [d,a,b,m] = p6_param(10 * pi / sqrt(6), n);
    x(i) = n;
    y(i) = m;
    i = i + 1;
end

plot(x,y);