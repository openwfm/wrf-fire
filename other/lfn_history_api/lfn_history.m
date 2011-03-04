% !*** lfn_history ***
% !
% ! A simple api for reading/writing level function histories in wrfinput files.
% !
% ! The convention in this code is that two extra variables will be added to 
% ! the wrfinput_d## files.  
% !
% ! 1. float LFN_HIST(nx,ny,ntime) contains the level function history for a 
% !      total number of ntime time slices.  The array in the file will have 
% !      3rd dimension of NHIST >= ntime, extra slices are to be ignored.
% !
% ! 2. float LFN_TIME(ntime) contains the time of each lfn history array.  
% !      This time is defined as the number of seconds after the start of 
% !      the simulation.  These values should all be positive, a negative
% !      value (in particular INVALID) will indicate the the level function,
% !      and all time slices after it, does not contain valid data.
% !
% ! The api takes care of all the particulars of reading/writing... it should
% ! be invisible to the caller.  There are 3 subroutines and one parameter
% ! contained in this api (see implementation for calling syntax):
% !
% ! parameter:
% !   nhist, the maximum number of history slices the api can handle.
% !
% ! subroutine:
% !   write_lfn_history, write (or add) level function history data to
% !                      the input file
% !   read_lfn_history, read all level function history data from the 
% !                     input file
% !   get_grid_info, get any relevant information (such as grid size) from 
% !                  input file

classdef lfn_history
    properties(Constant)
        nhist=15;
    end
    properties(Constant,Access=protected)
        filefmt='wrfinput_d%02i';
        xname='west_east_subgrid';
        yname='south_north_subgrid';
        xatm='west_east_stag';
        yatm='south_north_stag';
        lfn_hist_name='LFN_HIST';
        lfn_hist_time='LFN_TIME';
        lfn_hist_dim='i_lfn_history';
        dtname='DT';
        dxname='DX';
        dyname='DY';
        xtype='float';
        invalid=-9999999;
    end
    methods(Static)
        function write_lfn_history(idom,lfn,time)
            % The subroutine writes a level function to a wrfinput file in the
            % current directory.
            %
            % idom : Input integer describing the the domain number that we will
            %        write the array to.  This is only to determine the file name
            %        as printf 'wrfinput_d%02i' idom.
            %
            % lfn(nx,ny,ntime) : The level function history from time(1) to time(ntime).
            %
            % time(ntime) : The time of each level function history slice relative to the
            %               simulation start time in seconds.  All times must be >= 0, with
            %               The last time being the final history before the main fire
            %               code takes over computation.
            %
            % Input arrays must have the correct size according the output of
            % get_grid_info.
            
            ltime=lfn_history.invalid*ones(1,lfn_history.nhist);
            ltime(1:length(time))=time;
            f=lfn_history.get_file_name(idom);
            n=netcdf.open(f,'NC_WRITE');
            netcdf.redef(n);
            
            try
                tdim=netcdf.defdim(n,lfn_history.lfn_hist_dim,lfn_history.nhist);
            catch
                tdim=netcdf.inqdimid(n,lfn_history.lfn_hist_dim);
            end
            xdim=netcdf.inqdimid(n,lfn_history.xname);
            ydim=netcdf.inqdimid(n,lfn_history.yname);
            try
                vhid=netcdf.defvar(n,lfn_history.lfn_hist_name,lfn_history.xtype,[xdim ydim tdim]);
            catch
                vhid=netcdf.inqvarid(n,lfn_history.lfn_hist_name);
            end
            try
                vtid=netcdf.defvar(n,lfn_history.lfn_hist_time,lfn_history.xtype,tdim);
            catch
                vtid=netcdf.inqvarid(n,lfn_history.lfn_hist_time);
            end
            netcdf.enddef(n);
            
            nx=lfn_history.get_grid_info(idom,'nx');
            ny=lfn_history.get_grid_info(idom,'ny');
            
            netcdf.putVar(n,vhid,[0 0 0],[nx ny size(lfn,3)],lfn);
            netcdf.putVar(n,vtid,ltime);
            netcdf.close(n);
        end
        function [lfn,time]=read_lfn_history(idom)
            % The subroutine reads a level function from a wrfinput file in the
            % current directory.
            % 
            % idom : Input integer describing the the domain number that we will
            %        read the array from.  This is only to determine the file name
            %        as printf 'wrfinput_d%02i' idom.
            % 
            % returns:
            %
            % lfn(nx,ny,ntime) : The level function history from time(1) to time(ntime),
            %                    on return.
            %
            % time(ntime) : The time of each level function history slice relative to the
            %               simulation start time in seconds.
            
            nx=lfn_history.get_grid_info(idom,'nx');
            ny=lfn_history.get_grid_info(idom,'ny');
            
            n=netcdf.open(lfn_history.get_file_name(idom),'NC_NOWRITE');
            lid=netcdf.inqvarid(n,lfn_history.lfn_hist_name);
            tid=netcdf.inqvarid(n,lfn_history.lfn_hist_time);
            lfn=netcdf.getVar(n,lid);
            time=netcdf.getVar(n,tid);
            ntime=find(time<0,1);
            if isempty(ntime)
                ntime=length(time);
            else
                ntime=ntime-1;
            end
            lfn=lfn(1:nx,1:ny,1:ntime);
            time=time(1:ntime);
            netcdf.close(n);
        end
        function out=get_grid_info(idom,argin)
            
            % This subroutine inquires a wrfinput file in the current directory about
            % information relevant to the computation and manipulation of lfn history
            % arrays.  
            %
            % idom : Input integer describing the the domain number that we will
            %        read the array from.  This is only to determine the file name
            %        as printf 'wrfinput_d%02i' idom.
            %
            % argin : a string specifying what is to be returned.  Current
            %         valid inputs are:
            %
            %   nx,ny : The dimensions of the fire grid in the given file.
            %
            %   dx,dy : The grid resolution of the fire grid in meters.
            %
            %   dt : The atmospheric time step in seconds.
            %
            %   sr_x,sr_y : The atmospheric/fire grid refinement factor.
            
            n=netcdf.open(lfn_history.get_file_name(idom),'NC_NOWRITE');
            
            ix=netcdf.inqdimid(n,lfn_history.xname);
            iy=netcdf.inqdimid(n,lfn_history.yname);
            ax=netcdf.inqdimid(n,lfn_history.xatm);
            ay=netcdf.inqdimid(n,lfn_history.yatm);
            [t,nx]=netcdf.inqdim(n,ix);
            [t,ny]=netcdf.inqdim(n,iy);
            [t,ax]=netcdf.inqdim(n,ax);
            [t,ay]=netcdf.inqdim(n,ay);
            sr_x=nx/ax;
            sr_y=ny/ay;
            
            switch argin
                
                case 'nx'
                    out=nx-sr_x;
                    
                case 'ny'
                    out=ny-sr_y;
                    
                case 'dx'
                    out=netcdf.inqatt(n,netcdf.getConstant('NC_GLOBAL'),lfn_history.dxname);
                    
                case 'dy'
                    out=netcdf.inqatt(n,netcdf.getConstant('NC_GLOBAL'),lfn_history.dyname);

                case 'dt'
                    out=netcdf.inqatt(n,netcdf.getConstant('NC_GLOBAL'),lfn_history.dtname);
                    
                case 'sr_x'
                    out=sr_x;
                    
                case 'sr_y'
                    out=sr_y;
                    
                otherwise
                    error('invalid input %s',argin);
            end
            
    
            netcdf.close(n);
        end
    end
    methods(Static,Access=protected)
        function f=get_file_name(idom)
            f=sprintf(lfn_history.filefmt,idom);
        end
        function check(ncerr)
            if ncerr ~= 0
                error(netcdf.strerror(ncerr))
            end
        end
    end
end