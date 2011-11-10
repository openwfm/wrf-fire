function i=findn(a)
% i=findn(a)
% find nonzeros in an array and return their indices as rows
f=find(a(:));
s=size(a);
switch ndims(a)
    case 1
        i=find(a);
    case 2
        [i1,i2]=ind2sub(s,f);
        i=[i1,i2];
    case 3
        [i1,i2,i3]=ind2sub(s,f);
        i=[i1,i2,i3];
    case 4        
        [i1,i2,i3,i4]=ind2sub(s,f);
        i=[i1,i2,i3,i4];  
    otherwise
            error('at most 4 dimensions supported')
end
end
