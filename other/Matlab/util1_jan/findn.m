function i=findn(a)
% i=findn(a)
% find nonzeros in an array and return their indices as rows
f=find(a(:));
switch ndims(a)
    case 1
        i=f;
    case 2
        [i1,i2]=ndgrid(1:size(a,1),1:size(a,2));
        i=[i1(f),i2(f)];  
    case 3
        [i1,i2,i3]=ndgrid(1:size(a,1),1:size(a,2),1:size(a,3));
        i=[i1(f),i2(f),i3(f)];
    case 4        
        [i1,i2,i3,i4]=ndgrid(1:size(a,1),1:size(a,2),1:size(a,3),1:size(a,4));
        i=[i1(f),i2(f),i3(f),i4(f)];  
    otherwise
            error('at most 4 dimensions supported')
end
end
