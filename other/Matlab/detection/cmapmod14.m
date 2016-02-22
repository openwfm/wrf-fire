function cmap=cmapmod14
% cmap=cmapmod14
% colormap for active fires detection level 2 MOD14 files and derived
% black = notprocessed
% blue  = water
% gray  = cloud
% green = land
% red   = fire (saturation is confidence level)

cmap= [ ...
    0   0   0      %0 not processed (missing input data), black
    0   0   0      %1 not used, black
    0   0   0      %2 not processed (other reason)
    0   0   0.2    %3 water, dark blue
    0.2 0.4 0.4    %4 cloud, purple gray
    0   0.3   0    %5 non-fire clear land, green
    0   0   0      %6 unknown
    0.6 0.2 0.2    %7 low-confidence fire
    0.8 0.25  0.25   %8 nominal-confidence fire
    1.0 0.3  0.3   %9 high-confidence fire
];
end