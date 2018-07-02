function [ log_sum ] = new_objective( w, red, g )
%[ likelihood ] = new_objective( w, red, g )
%returns data log-likelihood for s specified ignition point
%inputs:
%   w : processed wrf-out information
%   red : sturct coming from
%   g : struct coming from load_subset_detections
%outputs
%   likelihood : likelihood of data given ignition pt 

radius = 30;
log_sum = 0;

weight = gauss_weight(radius);


% loop over appropriate subset of detections
for k = 1:length(g)
    like  = 0;
    heat = make_heats(red.tign,g(k).time);
    mask = get_fire_pixels(g(k).fxdata);
    %skip if no detections in
    if norm(mask) >0
        [mm nn] = size(mask);
        for ii = radius:mm-radius
            for jj =radius:nn-radius
                if mask(ii,jj) > 0
                    pp = compute_pixel_probability(ii,jj,heat,radius,weight);
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

