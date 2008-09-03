function nccheck(status)
% internal, called by nc* functions
% check status after mexnc call 
% error exit with an appropriate error message if nonzero

% Jan Mandel, September 2008

if(status ~= 0),
    fstat=mexnc('strerror',status);
    error(fstat)
end
return
