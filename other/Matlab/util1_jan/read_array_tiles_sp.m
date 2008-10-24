function [varargout]=read_array_tiles_sp(root,tiles,num1,num2);
% [i,j,a]=read_array_tiles(root,ntiles,num1,num2)
% read array produced by matching calls write_array_m
% in module_fr_sfire_util.F
% from files root[_num1[_num2]].nnnn.txt,  nnnn=1:ntiles
% if tiles=[] same as read_array_sp(root,num1,num2)

% Jan Mandel, 2008

if ~exist('num1','var'),
    num1=-1;
end
if ~exist('num2','var'),
    num2=-1;
end
ff=file_name(root,num1,num2);
if isempty(tiles),
    a=read_array_sp([ff,'.txt']);
    return
end
i=[];j=[];a=[];tile=[];
for itile=1:length(tiles)
    f=sprintf('%s.%4.4i.txt',ff,tiles(itile));
    [ii,jj,aa]=read_array_sp(f);
    i=[i;ii]; j=[j;jj]; a=[a;aa];
    tile=[tile;itile*ones(size(ii))];
end
[k,kix]=sort(i+max(i)*(j-1));
same=find(k(2:end)==k(1:end-1));
idiff=a(kix(same+1))~=a(kix(same));
if any(idiff)
        %id=find(idiff);
        %for ix=id(:)'
        %    i1=kix(ix);
        %   i2=kix(ix+1);
        %    fprintf('tile %i: %i %i %g tile %i: %i %i %g diff %g\n',...
        %        tile(i1),i(i1),j(i1),a(i1),tile(i2),i(i2),j(i2),a(i2),a(i2)-a(i1))
        %end
        error('inconsistent values at overlap')
end
switch nargout
    case 1
        varargout{1}=sparse(i,j,a);
    case 3
        varargout{1}=i;
        varargout{2}=j;
        varargout{3}=a;
    otherwise
        error('bad number of output arguments')
end
end