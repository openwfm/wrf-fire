function [varargout]=read_rec(fid,n,types)
if n == 0,
	n = nargout;
end
if ischar(types),
	type=types;
	begbytes=n*type2len(type);
else
	items=length(types);
	if items ~= n, error('bad number of types'), end
	ilen=zeros(items,1);
	for  i=1:n,
		ilen(i)=type2len(types{i});
	end
	begbytes=sum(ilen);
end
	
bytes=fread(fid,1,'int');
if(bytes ~= begbytes),
	error(sprintf('record length is %i instead of %i',bytes,begbytes))
end
if ischar(types),
	rec=fread(fid,n,type);
else
	for i=1:n,
		rec(i)=fread(fid,1,types{i});
	end
end
bytend=fread(fid,1,'int');
if bytes ~= bytend,
	error(sprintf('end byte count %i instead of %i',bytend,bytes))
end
if nargout == 1,
	varargout{1}=rec;
else
	if nargout == n,
		for i=1:n,
			varargout{i}=rec(i);
		end
	else
		error(sprintf('record items %i should be %i\n',n,nargout))
	end
end
return

function len=type2len(type)
if strcmp(type,'int'),
	len=4;
elseif strcmp(type,'double'),
	len = 8;
else
	error('bad type')
end
return

