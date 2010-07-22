function image_array_2d(a)
% write_array_2d(a)
% Purpose: visualized 2d matrix written by write_array_2d
% for input to WRF-Fire so that it looks the same as the array
% in ncview from the arrays in WRF state
%
[m,n]=size(a);
h=imagesc(flipud(a'));
axis image
warning('the vertical label is upside down')
colorbar
end