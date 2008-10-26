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
% check for consistent duplicates
isize=max(i)-min(i)+1;
ij=i+isize*(j-1);    % coded pairs (i,j)
[k,kix]=sort(ij);        % k=ij(kix) 
n=length(k);
ikix=zeros(n,1);
ikix(kix)=[1:n]';        % ij=k(ikix);
same=find(k(2:end)==k(1:end-1));
idiff=same(a(kix(same+1))~=a(kix(same)));
id1=kix(idiff);
id2=kix(idiff+1);
if any(idiff)
    i1=i(id1);
    i2=i(id2);
    j1=j(id1);
    j2=j(id2);
    a1=a(id1);
    a2=a(id2);
    t1=tile(id1);
    t2=tile(id2);
    for m=1:length(idiff);
            fprintf('tile %i: a(%i,%i)=%g tile %i: a(%i,%i)=%g diff %g\n',...
                t1(m),i1(m),j1(m),a1(m),t2(m),i2(m),j2(m),a2(m),a1(m)-a2(m))
    end
    error('inconsistent values at overlap')
end
% take out the duplicates
i(kix(same))=[];
j(kix(same))=[];
a(kix(same))=[];
% create the output
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