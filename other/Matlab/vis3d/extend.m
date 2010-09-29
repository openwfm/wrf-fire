function x0=extend(x1,x2,etype)
switch etype(1)
    case 'c'  
        x0=x1;  % constant
    case 'r'
        x0=x1+(x1-x2); % reflection
    otherwise
        type
        error('extend: unknown type')
end
end