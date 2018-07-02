function [ pix_prob ] = compute_pixel_probability(pixel_x,pixel_y,heats,radius, weight, pixel_probabilities )
%[ pix_prob ] = compute_pixel_probability(pixel_x,pixel_y,heats,radius )
% computes the probability of detection at a specific fire pixel.
% inputs
%     pixel_x, pixel_y : location of fire pixel
%     heats : array with simulated heat output
%     radius : std deviation of Gaussian "pixel smearing
%     pixel_probabilities:matrix of probabilities of detection
% outputs
%     pix_prob : probability of detection at the pixel given by satellite

sum = 0;

for iii = pixel_x-radius+1:pixel_x+radius % this can fail if detections too close to the domain boundary, fix it
    for jjj = pixel_y-radius+1:pixel_y+radius
        gp = gauss_part(pixel_x,pixel_y,iii,jjj,radius);
        sum = sum + pixel_probabilities(iii,jjj)*gp*weight;
    end
end

pix_prob = sum;

end

