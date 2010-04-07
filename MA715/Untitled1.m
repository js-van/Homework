function [ res ] = Untitled1( m )


prev = p8_param(20,m);

for n=21:100
    
    next = p8_param(n,m);
    d = prev - next;
    prev = next;
    res(n) = sqrt(sum(d.^2));
end

end
