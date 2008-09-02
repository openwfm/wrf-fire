function [nc,m]=ncdatatype(d)
% function [nc,m]=ncdatatype(d)
% d = vartype or datatype from netcdf
% nc = 'TEXT' or 'DOUBLE' to create read statements 
% m =  guess of what the type actually is in matlab 

%types_nc={'', '', 'TEXT', 'DOUBLE','DOUBLE','DOUBLE','DOUBLE'};  %  ??? 
types_nc={'', '', 'text', 'short','int','float','double'};  %  ??? 
types_m= {'', '', 'char', 'int16','int32','single','double'};  %  ??? 
%         0    1    2       3       4     5        6

nc=types_nc{d+1};
m=types_m{d+1};

end
