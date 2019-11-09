% second test script looking for tails???


domain_size = 1000;



%create fire mask
mask = zeros(domain_size,domain_size);
strip = ones(domain_size,1);
mask(:,round(domain_size/2)) = strip;

%make heat map
heat = zeros(domain_size,domain_size);
heat_strip = zeros(1,domain_size);
a = 0.01;
fire_start = 300;
for i = fire_start:domain_size
    heat_strip(1,i) = exp(-(i-fire_start)*a);
end
for i = 1:domain_size
    heat(i,:) = heat_strip;
end
figure, plot(heat(50,:)), title('Heat fraction')
%make probability matrix
detection_probs = zeros(domain_size,domain_size);

for i = 1:domain_size
    for j = 1:domain_size
        detection_probs(i,j) = detection_probability(heat(i,j));
    end
end
figure,plot(detection_probs(round(domain_size/2),:)),title('Detection probability')

    


%set up of likelihood stuff
like = [];
radius = 30;
weight = gauss_weight(radius);


%loop to make likelihoods
counter = 1;
for i=radius+1:domain_size-radius
    
%     counter
    l =  compute_pixel_probability(round(domain_size/2),i,heat,radius, weight, detection_probs );
    like(counter) = log(l);
    counter = counter +1;
end

figure,plot(like)


    
% testing region
% x = 1:domain_size;
% center = fire_start*ones(1,domain_size);
% g = @(x,x_0,sig) exp(-(x-x_0).^2/2/sig^2);
% 
% sig = 16;
% y = g(x,center,sig);
% figure,plot(y),title('gaussian');
% yg = log(y(1,1:940));
% log_prob = log(detection_probs(round(domain_size/2),1:940));
% liker = yg+log_prob;
% 
% figure,plot(yp),title('prob');
% figure,plot(liker),title('like')

