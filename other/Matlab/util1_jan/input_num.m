function value=input_num(prompt,default)
% value=input_num(prompt,default)
% numeric input with default
disp(['Enter ',prompt])
s=['[',num2str(default),'] '];
value = input(s);
if isempty(value),
    value=default;
end
disp(value)
end
