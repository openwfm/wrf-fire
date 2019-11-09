function rs = test5(test,like_spline)

%test5
close all
cone_slope = 10;
fire_cone = @(x,y) cone_slope*sqrt(( x.^2 + y.^2));

% if ~exist('like_spline')
%     load spline_data.mat;
%     [ls,fit1] = createFit(t,log_like);
% end
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
fires = zeros(size(x));



%test #1
if test == 1
    fire_z = 49;
    fire_y = 0;
    fire_x = fire_z/10;
    b = -10*(g+1)/g;
    x_dex = (fire_z-b)/cone_slope;
    scatter3(fire_x,fire_y,fire_z,'r*');
    
    fires(171,g) = 9;
    fires(30:40,30:40) = 5;
    tot = 0;
    for i = 1:3
        tot = 0;
        input_time = slice_time(i) - z;
        temp = like_spline(input_time(fires > 7));
        tot = tot + sum(temp);
        t1 = like_spline(input_time(fires < 1));
        temp = log(1 - exp(1).^t1);
        tot = tot + sum(temp)
    end
end %test 1


% test 2
% move fire pixel across
if test == 2
    input_time = slice_time(2) - z;
    tots = zeros(1,grid_size);
    for i = 1:grid_size
        scatter3(x(g,i),y(g,i),slice_time(2),'r*');
        t_mask = fires;
        t_mask(g,i) = 9;
        temp = like_spline(input_time(t_mask > 7));
        tots(i) = tots(i) + sum(temp);
        t1 = like_spline(input_time(t_mask < 1));
        temp = log(1 - exp(1).^t1);
        tots(i) = tots(i) + sum(temp);
    end
    hold off
    figure,plot(x(g,:),tots);
end %test 2

if test == 3
    contour3(x,y,z,[49 49],'k')
    fires = 5*ones(size(x));
    num_pts = 1000;
    x_coords = 1+round(2*g*rand(1,num_pts));
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
        if abs(zt - radius) < 2 && zt < radius%49
            fires(x_coords(i),y_coords(i)) = 9;
            scatter(u,v,'r*');
        else
            if rand < 0.98
                fires(x_coords(i),y_coords(i)) = 0;
                scatter(u,v,'b');
            else
                fires(x_coords(i),y_coords(i)) = 9;
                scatter(u,v,'r*');
            end
            
        end
    end
    
    num_slices = 41;
    %times = linspace(min(z(:))+1,max(z(:))-1,num_slices);
    times = linspace(20,70,num_slices);
    tots = zeros(size(times));
    for i = 1:num_slices
        input_time = times(i) - z;
        t_mask = fires;
        temp = like_spline(input_time(t_mask > 7));
        tots(i) = tots(i) + sum(temp);
        t1 = like_spline(input_time(t_mask < 1));
        temp = log(1 - exp(1).^t1);
        tots(i) = tots(i) + sum(temp);
    end
    hold off
    figure,plot(times,tots)
    
end % test 3

end
    


