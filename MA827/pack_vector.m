function [ V ] = pack_vector( R )

V = reshape(R, size(R,1) * size(R,2), 1);

end
