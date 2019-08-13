function XX=add_terrain_to_mesh(X, t, kind)
check_mesh(X)
XX=X;
kmax=size(X{1},3);
switch kind
    case {'shift','s'}
        disp('shifting mesh by terrain vertically')
        XX=X;
        for k=1:kmax
            XX{3}(:,:,k)=X{3}(:,:,k)+t;
        end
    case {'compress','c'}
        disp('compressing mesh keeping top unchanged')
        XX=X;
        for k=1:kmax
            XX{3}(:,:,k)=X{3}(:,:,k)+t*(kmax-k)/(kmax-1);
        end
    otherwise
        error('add_terrain_to_mesh: unknown kind')
end
check_mesh(XX)
end