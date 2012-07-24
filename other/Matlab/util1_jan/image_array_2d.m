function image_array_2d(a)
% write_array_2d(a)
% Purpose: visualized 2d matrix written by write_array_2d
% for input to WRF-Fire so that it looks the same as the array
% in ncview from the arrays in WRF state
%
[m,n]=size(a);
h=imagesc(flipud(a'));
axis image;
t=get(gca,'YTickLabel');  %  put vertical label upside down
set(gca,'YTickLabel',flipud(t))
colorbar
end