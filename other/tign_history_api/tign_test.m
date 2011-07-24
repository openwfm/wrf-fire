% Test the matlab tign_history api.  Assumes that a valid wrfinput_d01 file
% already exists in the current directory.

function tign_test

    idom=1;
    eps=10^-5;
    
    % test get_grid_info 
    nx=tign_history.get_grid_info(idom,'nx');
    ny=tign_history.get_grid_info(idom,'ny');
    dx=tign_history.get_grid_info(idom,'dx');
    dy=tign_history.get_grid_info(idom,'dy');
    dt=tign_history.get_grid_info(idom,'dt');
    try
        tign_history.get_grid_info(idom,'notvalid');
        fprintf('a call that should have failed, didn''t\n');
    catch
    end
    
    
    % construct our tign and time variables somehow
    tign=randn(nx,ny);
    
    % write to file
    tign_history.write_tign_history(idom,tign);
    
    
    % read from the file to a different variable
    tign1=tign_history.read_tign_history(idom);
    
    % check that the result is the same
    if length(size(tign)) ~= length(size(tign1)) || any(any(any(size(tign) ~= size(tign1)))) || ...
            any(any(any(abs(tign - tign1) > eps)))
        error('level function read failed')
    end
    
    fprintf('test successful\n')
    
end
