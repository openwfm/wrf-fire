function [ log_sum ] = new_objective( w, red, g )
%[ likelihood ] = new_objective( w, red, g )
%returns data log-likelihood for s specified ignition point
%inputs:
%   w : processed wrf-out information
%   red : sturct coming from
%   g : struct coming from load_subset_detections
%outputs
%   likelihood : likelihood of data given ignition pt 

sig = 4;
radius = round(sig*sqrt(32*log(10)))+1;
weight = gauss_weight(sig);

log_sum = 0;
% loop over appropriate subset of detections
for k = 1:length(g)
    like  = 0;
    heat = make_heats(red.tign_g,g(k).time);
    mask = get_fire_pixels(g(k).fxdata);
    dp_matrix = make_detection_probabilities(heat);
    %skip if no detections in
    if norm(mask) >0
        [mm, nn] = size(mask);
        % loop over detection pixels in mask
        for ii = radius:mm-radius
            for jj =radius:nn-radius
                if mask(ii,jj) > 0
                    pp = compute_pixel_probability(ii,jj,heat,sig,weight,dp_matrix);
                    like = like+log(pp);
%                 else
%                     pp = 1 - compute_pixel_probability(ii,jj,heat,radius,weight);
%                     like = like+log(pp);
                end
            end
        end
    end
    log_sum = log_sum + like;
end


end

