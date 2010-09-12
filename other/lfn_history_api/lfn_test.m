% Test the matlab lfn_history api.  Assumes that a valid wrfinput_d01 file
% already exists in the current directory.

function lfn_test

    idom=1;
    nhist=5;
    eps=10^-5;
    
    % test get_grid_info 
    nx=lfn_history.get_grid_info(idom,'nx');
    ny=lfn_history.get_grid_info(idom,'ny');
    dx=lfn_history.get_grid_info(idom,'dx');
    dy=lfn_history.get_grid_info(idom,'dy');
    dt=lfn_history.get_grid_info(idom,'dt');
    try
        lfn_history.get_grid_info(idom,'notvalid');
        fprintf('a call that should have failed, didn''t\n');
    catch
    end
    
    
    % construct our lfn and time variables somehow
    lfn=randn(nx,ny,nhist);
    time=(1:nhist)';
    
    
    % write to file
    lfn_history.write_lfn_history(idom,lfn,time);
    
    
    % read from the file to a different variable
    [lfn1,time1]=lfn_history.read_lfn_history(idom);
    
    % check that the result is the same
    if length(size(lfn)) ~= length(size(lfn1)) || any(any(any(size(lfn) ~= size(lfn1)))) || ...
            any(any(any(abs(lfn - lfn1) > eps)))
        error('level function read failed')
    end
    if length(time) ~= length(time1) || ...
            any(any(abs(time - time1) > eps))
        error('time read failed')
    end
    
    fprintf('test successful\n')
    
end