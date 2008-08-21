function nccheck(status)
% internal, called by nc* functions
% check status after mexnc call 
% error exit with an appropriate error message if nonzero
if(status ~= 0),
    fstat=mexnc('strerror',status);
    error(fstat)
end
return
