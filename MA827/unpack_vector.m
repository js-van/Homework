function [ R ] = unpack_vector( V )

N = size(V,1);

nn = int32(floor(sqrt(N)));
R = reshape(V, nn, nn);


end
