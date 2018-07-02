% second test script looking for tails???


domain_size = 1000;


%create fire mask
mask = zeros(domain_size,domain_size);
strip = ones(domain_size,1);
mask(:,500) = strip;

%make heat map
heat = zeros(domain_size,domain_size);
heat_strip = zeros(1,domain_size);
a = 0.02;
for i = 500:domain_size
    heat_strip(1,i) = exp(-(i-500)*a);
end
for i = 1:domain_size
    heat(i,:) = heat_strip;
end
mesh(heat)
%make probability matrix
detection_probs = zeros(domain_size,domain_size);

for i = 1:domain_size
    for j = 1:domain_size
        detection_probs(i,j) = detection_probability(heat(i,j));
    end
end


    


%set up of likelihood stuff
like = [];
radius = 70;
weight = gauss_weight(radius);


%loop to make likelihoods
counter = 1;
for i=radius+1:domain_size-radius
    
%     counter
    l =  compute_pixel_probability(500,i,heat,radius, weight, detection_probs );
    like(counter) = log(l);
    counter = counter +1;
end

plot(like)


    




