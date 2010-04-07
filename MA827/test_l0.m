function [ g ] = test_l0( f )

g = zeros(size(f,1),1);

for i=1:size(f)
    
    k = zeros(size(f,1),1);
    k(1) = 1;
    k(i) = 1;
    
    g(i) = sum([conv(k, f) > 0.001]);
end

figure,plot(f);
figure,plot(g);

end
