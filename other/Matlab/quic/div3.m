function u=div3(f,h)
% u=div3(f,h)
% compute divergence of vector field
% arguments:
%    f{1}, f{2}, f{3} vector components on staggered mesh (midpoints of sides)
% output: 
%    u = df{1}/x1 + df{2}/dx2 + df{3}/dx3
f11 = (f{1}(2:end,:,:)-f{1}(1:end-1,:,:))/h(1);
f22 = (f{2}(:,2:end,:)-f{2}(:,1:end-1,:))/h(2);
f33 = (f{3}(:,:,2:end)-f{3}(:,:,1:end-1))/h(3);
u = f11 + f22 + f33;
end

