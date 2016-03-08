function [cmap,varargout]=cmapmod14
% cmap=cmapmod14
% colormap for active fires detection level 2 MOD14 files and derived
% black = notprocessed
% blue  = water
% gray  = cloud
% green = land
% red   = fire (saturation is confidence level)

imax=20;
cpap=zeros(7+3*(imax+1),3);
cmap(1:7,:)= [ ...
    0   0   0      %0 not processed (missing input data), black
    0   0   0      %1 not used, black
    0   0   0      %2 not processed (other reason)
    0   0   0.2    %3 water, dark blue
    0.2 0.4 0.4    %4 cloud, purple gray
    0   0.3   0    %5 non-fire clear land, green
    0   0   0      %6 unknown
];
 
 %7 low-confidence fire
 %8 nominal-confidence fire
 %9 high-confidence fire
 % then repeat transition to old
red=[1 0 0];
yellow=[1  1  0];

atten=5;
for i=0:imax
    w=exp(-atten*i/imax)-exp(-atten);
    col=(1-w)*yellow+w*red;
    cmap(8+3*i  ,:)=col;
    cmap(8+3*i+1,:)=0.8*col;
    cmap(8+3*i+2,:)=0.6*col;
end
if size(cmap,1)>256,
    error('imax too large')
end
if nargout>1,
    varargout(1)={imax};
end