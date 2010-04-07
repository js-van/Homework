function [ D ] = hw1_part1_grapher( N, step )

D = zeros(N-1,1);
for j=2:N
    [x,C] = hw1_part1(j * step);
    v = x - C;
    D(j-1) = sqrt(sum( v .* v) / (j * step)) ;
end



end
