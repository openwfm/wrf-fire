function [varargout]=read_array_tiles_sp(root,dmtiles,mptiles,num1,num2);
% [i,j,a]=read_array_tiles_sp(root,dmtiles,mptiles,num1,num2);
% a=read_array_tiles_sp(root,dmtiles,mptiles,num1,num2);
%
% read array produced by matching calls write_array_m in module_fr_sfire_util.F
% cycle tiles over dmtiles and mptiles
% from files root[_num1[_num2]].nnnn.txt,  nnnn=1:dmntiles
% if mptiles nonempry, add tile*10000 to num1
% if dmtiles=[] mptiles=[] same as read_array_sp(root,num1,num2)
% output in [i j a] format works even when some indices are <1

% Jan Mandel, October 2008

if ~exist('num1','var'),
    num1=-1;
end
if ~exist('num2','var'),
    num2=-1;
end
ff=file_name(root,num1,num2);
if ~exist('dmtiles','var')
    dmtiles=[];
end
if ~exist('mptiles','var')
    mptiles=[];
end
if isempty(dmtiles) & isempty(mptiles)
   [i,j,a]=read_array_sp([ff,'.txt']);
else
   i=[];j=[];a=[];tile=[];
   if isempty(dmtiles),
      dmtiles=-1;
   end
   if isempty(mptiles),
      mptiles=0;
   end
   for dmtile=dmtiles
      for mptile=mptiles
         if dmtile >= 0,
            f=sprintf('%s_%5.5i.%4.4i.txt',root,num1+10000*mptile,dmtile);
	 else
            f=sprintf('%s_%5.5i.txt',root,num1+10000*mptile);
	 end
         [ii,jj,aa]=read_array_sp(f);
         i=[i;ii]; j=[j;jj]; a=[a;aa];
         tile=[tile;ones(length(ii),1)*[dmtile,mptile]];
      end
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
      t1=tile(id1,:);
      t2=tile(id2,:);
      for m=1:length(idiff);
            fprintf('tile %i %i: a(%i,%i)=%g tile %i %i: a(%i,%i)=%g diff %g\n',...
                t1(m,:),i1(m),j1(m),a1(m),t2(m,:),i2(m),j2(m),a2(m),a1(m)-a2(m))
      end
      warning('inconsistent values at overlap, taking the first seen')
   end
   % take out the duplicates
   i(kix(same))=[];
   j(kix(same))=[];
   a(kix(same))=[];
end
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
