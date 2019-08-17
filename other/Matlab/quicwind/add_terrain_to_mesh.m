function XX=add_terrain_to_mesh(X, kind, how, val)
check_mesh(X);

x = X{1}(:,:,1); y=X{2}(:,:,1);z=X{3};
kmax=size(X{1},3);
if ischar(kind),
    switch kind
        case 'hill'
            cx = mean(x(:));
            cy = mean(y(:));
            hz = max(z(:))-min(z(:));
            rx = mean(abs((x(:)-cx)));
            ry = mean(abs((y(:)-cy)));
            a = ((x-cx)./rx).^2 + ((y-cy)./ry).^2 ;
            t = hz*exp(-a*2)*val;;
        otherwise
            error('add_terrain_to_mesh: unknown kind')
    end
elseif isnumeric(kind),
    t=kind;
else
    error('kind must be string or numeric')
end

switch how
    case {'shift','s'}
        disp('shifting mesh by terrain vertically')
        XX=X;
        for k=1:kmax
            XX{3}(:,:,k)=X{3}(:,:,k)+t;
        end
    case {'compress','c','squash'}
        disp('compressing mesh keeping top unchanged')
        XX=X;
        for k=1:kmax
            XX{3}(:,:,k)=X{3}(:,:,k)+t*(kmax-k)/(kmax-1);
        end
    otherwise
        error('add_terrain_to_mesh: unknown how')
end
check_mesh(XX)
end