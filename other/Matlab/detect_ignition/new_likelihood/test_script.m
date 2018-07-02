% testing whether the new likelihood works to find ignition point

domain_size = 1000;

%set up true values
ignition_time = 300;
ignition_x = 500;
ignition_y = 500;
true_tign = make_times(ignition_x,ignition_y,ignition_time,domain_size);
t_now = 700;
%mesh(true_tign);

%make fire mask
mask = zeros(domain_size,domain_size);
centers = [187 751;128 653;106 539;639 126;842 301;783 219];
[m n] = size(centers);
pixel_size = 1;
for ic = 1:m
    for ii = centers(ic,1)-pixel_size:centers(ic,1)+pixel_size
        for jj = centers(ic,2)-pixel_size:centers(ic,2)+pixel_size
            mask(ii,jj) = 1;
        end
    end
end

close all
hold on
contour(mask)
contour(true_tign)

% set up "simlulation grid"
dx = 100; x_steps = 9;
dy = 100; y_steps = 9;
dt = 100; time_steps = 3;

data = [];
counter = 0;

data_spec = 'data_%d_%d_%d';
file_spec = 'data_%d_%d_%d.mat';
save_spec = 'C:\cygwin64\home\paulc\wrf-fire\other\Matlab\detect_ignition\new_likelihood\data\data_%d_%d_%d.mat';

load files.mat
%files = [];
% 
% %create and save data for "simulations"
% for ig_time = 0:time_steps
%     ignition_time = ig_time*dt+100;
%     for ig_x = 1:x_steps
%         ignition_x = ig_x*dx;
%         for ig_y = 1:y_steps
%             ignition_y = ig_y*dy;
%             counter = counter + 1;
%             
%             tign = make_times(ignition_x,ignition_y,ignition_time,domain_size);
%             data.tign = tign;
%             name = sprintf(data_spec,ignition_time,ignition_x,ignition_y);
%             data.name = name;
%             file = sprintf(file_spec,ignition_time,ignition_x,ignition_y);
%             files{counter} = file;
%             %save_str = sprintf(save_spec,ignition_time,ignition_x,ignition_y);
%             
%             
%             
%             %saving the data
%             cd('data')
%             save(file,'data')
%             cd('../')
%             
%             
%             %close all
%             %mesh(tign)
%             
%             
%         end
%     end
% end

%compute likelihoods

%get weights first
radius = 10;
weight = gauss_weight(radius);

%loop to compute
[mm nn] = size(files);
like_list = zeros(nn,1);
for i=1:nn
    cd('data')
    clear('tign')
    load(files{i})
    cd('../')
    
    i
    heat = make_heats(data.tign,t_now);
    like = compute_likelihood(heat,mask,radius,weight)

    like_list(i) = like;
    %contour(heat);
    
    
    
end

hold off
% figure
% contour(reshape(like_list(1:81,1),[9 9]))
% figure
% contour(reshape(like_list(82:2*81,1),[9 9]))
% figure
% contour(reshape(like_list(163:3*81,1),[9 9]))

g_points = x_steps*y_steps;
for figs =1:time_steps+1
    figure
    contour(reshape(like_list((figs-1)*g_points+1:figs*g_points,1),[x_steps y_steps]))
end

for figs =1:time_steps+1
    figure
    mesh(reshape(like_list((figs-1)*g_points+1:figs*g_points,1),[x_steps y_steps]))
end


    

