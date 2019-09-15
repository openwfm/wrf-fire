function check_mesh(X)

x = X{1}; y = X{2}; z = X{3};
if ndims(x)~=3|ndims(y)~=3|ndims(z)~=3,
    error('test_mesh: x y z must be 3D')
end

[nx1,ny1,nz1] = size(x);
if any(size(y)~=[nx1,ny1,nz1])|any(size(z)~=[nx1,ny1,nz1]),
    error('check_mesh: arrays x y z must be the same size')
end
if ~all(all((x(2:end,:,:)>x(1:end-1,:,:)))),
    error('check_mesh: x increments in array x must be positive')
end
if ~all(all((y(:,2:end,:)>y(:,1:end-1,:)))),
    error('check_mesh: y increments in array y must be positive')
end
if ~all(all((y(:,2:end,:)>y(:,1:end-1,:)))),
    error('check_mesh: z increments in array z must be positive')
end

