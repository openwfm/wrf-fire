% test 4

domain_size = 1000;
mask = zeros(domain_size,domain_size);
%strip = ones(domain_size,1);
mask(:,500) = 1;

time_strip = linspace(-10,30,domain_size);
ts2 = linspace(0,1,domain_size);
[x,y] = meshgrid(time_strip,ts2);
times = zeros(domain_size);
for i = 1:domain_size
    times(i,:) = time_strip;
end

dp = detection_probability(times);
d_strip = dp(500,:);
figure,plot(time_strip,d_strip),title('detection probability'),
xlabel('time since fire arrival')
ylabel('probability of detection')

figure,plot(time_strip,log(d_strip)),title('log detection probability'),
xlabel('time since fire arrival')
ylabel('log probability of detection')

sig = 2.2778;
gauss_strip = 1/(2*pi*sig^2)*exp(-time_strip.^2/(2*sig^2));
%gauss_strip = exp(-time_strip.^2/(2*sig^2));

figure,plot(time_strip,gauss_strip),title('geolocation stuff');
xlabel('time since fire arrival')
ylabel('gaussian')

log_g = log(gauss_strip);
figure,plot(time_strip,log_g),title('log geolocation stuff');
xlabel('time since fire arrival')
ylabel('log gaussian')

new = log(gauss_strip)+log(d_strip);
figure,plot(time_strip,new),title('new')



like = [];
radius = 30;
weight = gauss_weight(radius);

counter = 1;
for i=radius+1:domain_size-radius
    
%     counter
    l =  compute_pixel_probability(500,i,heat,radius, weight, detection_probs );
    like(counter) = log(l);
    counter = counter +1;
end

figure,plot(like)
