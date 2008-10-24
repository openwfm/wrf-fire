function a=read_array_tiles(root,tiles,num1,num2);
% a=read_array_tiles(root,ntiles,num1,num2)
% read array produced by matching calls write_array_m
% in module_fr_sfire_util.F
% from files root[_num1[_num2]].nnnn.txt,  nnnn=1:ntiles

% Jan Mandel, 2008

if ~exist('num1','var'),
    num1=-1;
end
if ~exist('num2','var'),
    num2=-1;
end
ff=file_name(root,num1,num2);
for i=1:length(tiles)
    f=sprintf('%s.%4.4i.txt',ff,tiles(i));
    tile{i}=read_array_sp(f);
    mm(i)=size(tile{i},1);
    nn(i)=size(tile{i},2);
end
m=max(mm);
n=max(nn);
a=zeros(m,n);
for i=1:length(tiles)
    b=zeros(m,n);
    b(1:mm(i),1:nn(i))=tile{i};
    if(any(a(:) ~=0 & b(:)~=0)),
        error('tiles overlap')
    end
    a=a+b;
end
end
