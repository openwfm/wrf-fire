function [like_spline,deriv_spline] = make_spline(time_bounds,num_pts)
%[like_spline,deriv_spline] = make_spline(time_bounds,num_pts)
% Function returns a spline for the log-likelihood an its derivative
% requires curve fitting toolbox
% inputs:
%   time_bounds - hours before and after fire arrival time
%   num pts - number of pts to use in creating the splines, should be an
%   even number
% outputs:
%   like_spline - spline which gives the log-likehood of detection as a
%      function of seconds since fire arrival time
%   deriv_spline - the derivative of like_spline, computed by center
%      difference approximation
%

domain_size = num_pts;
time_start = -time_bounds; time_end = 1.5*time_bounds;
time_strip = linspace(time_start,time_end,num_pts);
ts2 = linspace(0,1,domain_size); %dummy dimension
[x,y] = meshgrid(time_strip,ts2);
times = zeros(domain_size);
for i = 1:domain_size
    times(i,:) = time_strip;
end
%figure,quick_mesh(times)
dp = detection_probability(times);
d_strip = dp(domain_size/2,:);
figure,plot(time_strip,d_strip),title('detection probability'),
xlabel('time since fire arrival')
ylabel('probability of detection')
ylim([0 1]);

like = [];
radius = 60; %size over which to "integrate"
%weight = gauss_weight(radius); %not needed, we shift the curve up
dx = 30; %in meters, this is grid spacing, dy = dx also, maybe compute this by passing 
         % in some tign data
% make distance matrix one time and then pass into the gaussin computation
d_squared = zeros(radius*2+1,radius*2+1);
[m,n] = size(d_squared);
for i = 1:m
    for j = i:n
        d_squared(i,j) = i^2+j^2+2*(radius+1)^2-2*(radius+1)*(i+j);
        if i == j
            d_squared(i,j) = 0.5*d_squared(i,j);
        end
    end
end

d_squared = d_squared+d_squared';
d_squared = dx^2*d_squared;
sig = 1000/3; %1000 meters at sig 3 

counter = 1;
for i=radius+1:domain_size-radius
    %counter
    temp_prob = dp(domain_size/2-radius:domain_size/2+radius,i-radius:i+radius);
    %l =  compute_pixel_probability(500,i,heat,radius, weight, detection_probs );
    gauss = exp(-d_squared/(2*sig^2));
    prob = gauss.*temp_prob;
    l = log(sum(prob(:))/(2*pi*sig^2));
    like(counter) = l;
    counter = counter +1;
end

short_time = time_strip(1,radius+1:end-radius);
l2 = like-max(like(:))+max(like(:))/1000;
figure,plot(short_time,l2),title('pixel log likelihood')
xlabel('Hours since fire arrival')
ylabel('Log likelihood of detection')

% compute derivative
l2_prime = zeros(1,length(short_time));
h = short_time(2) - short_time(1);
for i = 2:length(short_time)-1
    l2_prime(i) = (l2(i-1)-l2(i+1))/(2*h);
end
l2_prime(1) = l2_prime(2);
l2_prime(end)=l2_prime(end-1);
figure,plot(short_time,l2_prime),title('Derviative')

%make spline from the data short_time, l2, l2_prime, converting to seconds
[like_spline, fit1] = createFit(3600*short_time, l2);
[deriv_spline, fit2] = createFit(3600*short_time, l2_prime);



end
