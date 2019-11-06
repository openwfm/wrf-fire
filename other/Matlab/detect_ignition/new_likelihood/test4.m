% test 4

domain_size = 2000;
mask = zeros(domain_size,domain_size);
%strip = ones(domain_size,1);
mask(:,domain_size/2) = 1;

time_start = -10; time_end = 40
time_strip = linspace(time_start,time_end,domain_size);
ts2 = linspace(0,1,domain_size);
[x,y] = meshgrid(time_strip,ts2);
times = zeros(domain_size);
for i = 1:domain_size
    times(i,:) = time_strip;
end

dp = detection_probability(times);
d_strip = dp(domain_size/2,:);
figure,plot(time_strip,d_strip),title('detection probability'),
xlabel('time since fire arrival')
ylabel('probability of detection')

figure,plot(time_strip,log(d_strip)),title('log detection probability'),
xlabel('time since fire arrival')
ylabel('log probability of detection')

% sig = 2.2778;
% gauss_strip = 1/(2*pi*sig^2)*exp(-time_strip.^2/(2*sig^2));
%gauss_strip = exp(-time_strip.^2/(2*sig^2));

% figure,plot(time_strip,gauss_strip),title('geolocation stuff');
% xlabel('time since fire arrival')
% ylabel('gaussian')

% log_g = log(gauss_strip);
% figure,plot(time_strip,log_g),title('log geolocation stuff');
% xlabel('time since fire arrival')
% ylabel('log gaussian')

% new = log(gauss_strip)+log(d_strip);
% figure,plot(time_strip,new),title('new')



like = [];
radius = 40;
weight = gauss_weight(radius);
dx = 300; %in meters, this is grid spacing, dy = dx also

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
sig = 1000/3;
%figure,mesh(exp(-d_squared/(2*pi*sig^2)));

counter = 1;
for i=radius+1:domain_size-radius
    
%     counter
    temp_prob = dp(domain_size/2-radius:domain_size/2+radius,i-radius:i+radius);
    %l =  compute_pixel_probability(500,i,heat,radius, weight, detection_probs );
    gauss = exp(-d_squared/(2*pi*sig^2));
    prob = gauss.*temp_prob;
    l = log(sum(prob(:))/(2*pi*sig^2));
    like(counter) = l;
    counter = counter +1;
end


short_time = time_strip(1,radius+1:end-radius);
l2 = like-max(like(:));
figure,plot(short_time,l2),title('pixel log likelihood')

%figure,plot(like)
