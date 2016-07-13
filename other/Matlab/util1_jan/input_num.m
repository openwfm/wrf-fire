function value=input_num(prompt,default,force)
% value=input_num(prompt,default)
% numeric input with default
% if force present and true, do not ask and use default in any case
if exist('force','var'),
    use_default=force;
else
    use_default=0;
end

disp(['Enter ',prompt])
s=['[',num2str(default),'] '];
if use_default,
    value=[];
else
    value = input(s);
end
if isempty(value),
    value=default;
end
disp(value)
end
