%test 3   -   put reported detection on the fire line, move the actual
%location away

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
%mesh(heat)
contour(heat)
%make probability matrix
detection_probs = zeros(domain_size,domain_size);

for i = 1:domain_size
    for j = 1:domain_size
        detection_probs(i,j) = detection_probability(heat(i,j));
    end
end

%move actual pixel across domain, passing through location of reported
%location

radius = 50;
like = [];
for i = 1:domain_size
    dist2 = (500-i)^2;
    l = exp(-dist2/(3*radius)^2)*detection_probs(500,i);
    like(i) = log(l);
end

close all
plot(like)
