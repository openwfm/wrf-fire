function [ pix_prob ] = compute_pixel_probability(pixel_x,pixel_y,heats,sig, weight, detection_probabilities )
%[ pix_prob ] = compute_pixel_probability(pixel_x,pixel,_y,heats,radius )
% computes the probability of detection at a specific fire pixel.
% inputs
%     pixel_x, pixel_y : location of fire pixel
%     heats : array with simulated heat output
%     sig : standard deviation of gaussian
%     pixel_probabilities:matrix of probabilities of detection
% outputs
%     pix_prob : probability of detection at the pixel given by satellite

%sig = 0.0004;
%sig = 4;
[m,n] = size(heats);

%since gaussian decays fast, no need to integrate over the whole domain.
%compute how much we need to look at here
%radius = round(sig*sqrt(32*log(10)))+1;
%const = 1/(2*pi*sig^2); % not needed for discrete
radius = sig;
total = 0;

ros = 1; % m/s


for iii = pixel_x-radius+1:pixel_x+radius % this can fail if detections too close to the domain boundary, fix it
    for jjj = pixel_y-radius+1:pixel_y+radius
        detect_prob = detection_probabilities(iii,jjj);
        %dp = detection_probability(heats(iii,jjj)); % matrix of heat
        %passed instead
        gp = gauss_part(pixel_x,pixel_y,iii,jjj,sig);
        total = total + detect_prob*gp*weight;
    end
end


% doing integral formal instead...
% for iii = pixel_x-radius:pixel_x+radius % this can fail if detections too close to the domain boundary, fix it
%     for jjj = pixel_y-radius:pixel_y+radius
%         gp = const*gauss_part(pixel_x,pixel_y,iii,jjj,sig);
%         total = total + detection_probabilities(iii,jjj)*gp;
% 
%     end
% end
% 
% for iii = border:m-border
%     for jjj = border:n-border
%         %gp is just the exponential part of gaussian
%         gp = gauss_part(pixel_x,pixel_y,iii,jjj,sig);
%         total = total + detection_probabilities(iii,jjj)*gp*dx*dy;
%     end
% end

pix_prob = total;

end

