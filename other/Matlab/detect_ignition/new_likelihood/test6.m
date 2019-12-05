function [] = test6()


close all

new_like = input_num('Use new likelihood?',1);

%make splines
if new_like
    fprintf('Making splines \n');
    [p_like_spline,p_deriv_spline,n_deriv_spline] = make_spline(100,2000);
    save splines.mat p_like_spline p_deriv_spline n_deriv_spline
end

%make fire data
cone_slope = 10;
fire_cone = @(x,y) cone_slope*sqrt(( x.^2 + y.^2));
g = 100;
grid_size =2*g+1;
position = linspace(-10,10,grid_size);
[x,y]= meshgrid(position,position);

z = fire_cone(x,y);
fire_top = z > 100;
z(fire_top) = 100;
mesh(x,y,z);
hold on
xlabel('x');


slice_time = [47 49 51];
fires = -1*ones(size(x));


contour3(x,y,z,[49 49],'k')
fires = 5*ones(size(x));
num_pts = 1000;
rng(1);
x_coords = 1+round(2*g*rand(1,num_pts));
rng(2);
y_coords = 1+round(2*g*rand(1,num_pts));
%figure,scatter(x_coords,y_coords);
%make fire mask
hold on
radius = slice_time(2)/10;
angle = linspace(0,2*pi);
perim = zeros(length(angle),2);
perim(:,1) = radius*cos(angle)';
perim(:,2) = radius*sin(angle)';
plot(perim(:,1),perim(:,2),'k')

for i = 1:num_pts
    u = x(x_coords(i),y_coords(i));
    v = y(x_coords(i),y_coords(i));
    zt = norm([u v]);
    if abs(zt - radius) < 2  && zt < radius %49
        fires(x_coords(i),y_coords(i)) = 9;
        scatter(u,v,'r*');
    else
        if rand < 0.98
            fires(x_coords(i),y_coords(i)) = -1;
            scatter(u,v,'b');
        else
            fires(x_coords(i),y_coords(i)) = 1;
            scatter(u,v,'r*');
        end
    end
end
hold off

%make all fire detections
%fires = ones(grid_size,grid_size);


%plot cone and detections
% hold off
% figure, mesh(x,y,z);
% xlabel('x'),ylabel('y'),zlabel('time')
% hold on
% scatter3(x(1:20:end),y(1:20:end),-49*fires(1:20:end),'og');

% evaluate and plot likelihoods
t = slice_time(2) - z;
%new likelihood
if new_like
    [like,deriv]= temp_liker(fires,t,p_like_spline,p_deriv_spline,n_deriv_spline);
else
    %old likelihood
    params.stretch = [0.5,10,5,10];
    [like,deriv] = like2(fires,t,params.stretch);
end

fprintf('paused for plotting, etc... \n');
figure,mesh(like),title('like')
figure,mesh(deriv),title('deriv')
figure,plot(t(100,:),like(100,:)),title('like slice')
figure,plot(t(100,:),deriv(100,:)),title('deriv slice')



end % function

