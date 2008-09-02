function v=ncextract(p)
% v=ncextract(p)
% extract v as matlab array from structure returned by ncdump
% for one variable
v=squeeze(double(p.var_value));
end