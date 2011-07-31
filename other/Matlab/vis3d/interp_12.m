function r=interp_12(a,i1,i2)
% horizontal interpolation: in the first two coordinates, to given i1 i2
    if isempty(a),
        r=[];
    else
        nv=size(a,3); % number of vertical layers
        r=zeros(nv,1);
        for i=1:nv
            b=squeeze(double(a(:,:,i)));
            r(i)=interp2(b,i1,i2);
        end
    end
end
       