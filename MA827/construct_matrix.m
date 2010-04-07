function [ M ] = construct_matrix( N )

M = zeros(N*N,N*N);

for x=1:N*N
    for y=1:N*N
        
        rx = ceil(x / N);
        ry = ceil(y / N);
        
        cx = mod(x, N);
        cy = mod(y, N);
        
        if abs(rx-ry) + abs(cx - cy) <= 1
            M(x,y) = 1;
        end
    end
end

end
