function [a_bu,a_bv]=interp_w2buv(a)
% interpolate horizontally values at w points to bottom of cell 
% under the u and v points

% extend values at at w by 1 on each side by continuation
etype='constant'; % extend by constant
s=size1(a,4); 
alt=zeros(s(1)+2,s(2)+2,s(3),s(4)); % extend laterally by 1
alt(2:end-1,2:end-1,:,:)=a;         % embded original array
alt(1,2:end-1,:,:)=extend(a(1,:,:,:),a(2,:,:,:),etype); % extend by reflection
alt(end,2:end-1,:,:)=extend(a(end,:,:,:),a(end-1,:,:,:),etype);
alt(2:end-1,1,:,:)=extend(a(:,2*1,:,:),a(:,2,:,:),etype); % 
alt(2:end-1,end,:,:)=extend(a(:,end,:,:),a(:,end-1,:,:),etype);

% interpolate to bottom cell locations under u and v
a_bu=0.5*(alt(1:end-1,:,:,:)+alt(2:end,:,:,:));
a_bv=0.5*(alt(:,1:end-1,:,:)+alt(:,2:end,:,:));
end

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