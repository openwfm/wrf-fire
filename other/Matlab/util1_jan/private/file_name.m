function id=file_id(name,num1,num2)
% id=file_id(name,num1,num2)
% create part of filename of the form _nnnnn_nnnnn
% Jan Mandel 22-apr-05

if num1>=0 & num2>=0,
	id=sprintf('%s_%08i_%05i',name,num1,num2);
elseif num1>=0 & num2<0,
	id=sprintf('%s_%08i',name,num1);
elseif num1<0 & num2>=0,
	id=sprintf('%s_%08i',name,num2);
elseif num1<0 & num2<0,
	id=name;
end
