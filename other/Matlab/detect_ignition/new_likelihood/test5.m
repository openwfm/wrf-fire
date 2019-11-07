%test5
close all
fire_cone = @(x,y) 10*sqrt(( x.^2 + y.^2));

position = linspace(-10,10,201);
[x,y]= meshgrid(position,position);

z = fire_cone(x,y);

fire_top = z > 100;
z(fire_top) = 100;
mesh(x,y,z);
hold on
scatter3(7,0,49,'r*');
xlabel('x');


slice_time = [47 49 51];
fires = zeros(size(x));
%fires(171,100) = 9;
fires(30:40,30:40) = 5;

tot = 0;
% for i = 1:3
%     tot = 0;
%     input_time = slice_time(i) - z;
%     temp = like_spline(input_time(fires > 7));
%     tot = tot + sum(temp);
%     t1 = like_spline(input_time(fires < 1));
%     temp = log(1 - exp(1).^t1);
%     tot = tot + sum(temp)
% end

% move fire pixel across
input_time = slice_time(2) - z;
tots = zeros(1,201);
for i = 1:201
    scatter3(x(100,i),y(100,i),slice_time(2),'r*');
    t_mask = fires;
    t_mask(100,i) = 9;
    temp = like_spline(input_time(t_mask > 7));
    tots(i) = tots(i) + sum(temp);
    t1 = like_spline(input_time(t_mask < 1));
    temp = log(1 - exp(1).^t1);
    tots(i) = tots(i) + sum(temp);
end
hold off
%figure,plot(input_time(100,:),tots);
    


