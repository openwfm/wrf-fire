% !*** tign_history ***
% !
% ! A simple api for reading/writing fire histories in wrfinput files for 
% ! gradual ignition.
% !
% ! 1. float tign_HIST(nx,ny) contains the fire history in the form of 
% !    seconds from ignition at each point.
% !
% ! The api takes care of all the particulars of reading/writing... it should
% ! be invisible to the caller.  There are 3 subroutines
% ! contained in this api (see implementation for calling syntax):
% !
% ! subroutine:
% !   write_tign_history, write (or add) ignition history data to
% !                      the input file
% !   read_tign_history, read all history data from the 
% !                     input file
% !   get_grid_info, get any relevant information (such as grid size) from 
% !                  input file

classdef tign_history
    properties(Constant,Access=protected)
        filefmt='wrfinput_d%02i';
        xname='west_east_subgrid';
        yname='south_north_subgrid';
		tname='Time';
        xatm='west_east_stag';
        yatm='south_north_stag';
        tign_hist_name='TIGN_G';
        dtname='DT';
        dxname='DX';
        dyname='DY';
        xtype='float';
        invalid=-9999999;
		write_time=0;
    end
    methods(Static)
        function write_tign_history(idom,tign)
            % The subroutine writes a level function to a wrfinput file in the
            % current directory.
            %
            % idom : Input integer describing the the domain number that we will
            %        write the array to.  This is only to determine the file name
            %        as printf 'wrfinput_d%02i' idom.
            %
            % tign(nx,ny) : The fire history written to the file.
            %
            % Input arrays must have the correct size according the output of
            % get_grid_info.
            
            f=tign_history.get_file_name(idom);
            n=netcdf.open(f,'NC_WRITE');
            netcdf.redef(n);
           
			tdim=netcdf.inqdimid(n,tign_history.tname);
            xdim=netcdf.inqdimid(n,tign_history.xname);
            ydim=netcdf.inqdimid(n,tign_history.yname);
            try
                vhid=netcdf.defvar(n,tign_history.tign_hist_name,tign_history.xtype,[xdim ydim tdim]);
            catch
                vhid=netcdf.inqvarid(n,tign_history.tign_hist_name);
            end
            netcdf.enddef(n);
            
            nx=tign_history.get_grid_info(idom,'nx');
            ny=tign_history.get_grid_info(idom,'ny');
            
            netcdf.putVar(n,vhid,[0 0 tign_history.write_time],[nx ny 1],tign);
            netcdf.close(n);
        end
        function tign=read_tign_history(idom)
            % The subroutine reads a level function from a wrfinput file in the
            % current directory.
            % 
            % idom : Input integer describing the the domain number that we will
            %        read the array from.  This is only to determine the file name
            %        as printf 'wrfinput_d%02i' idom.
            % 
            % returns:
            %
            % tign(nx,ny,ntime) : The fire history from the file.
            
            nx=tign_history.get_grid_info(idom,'nx');
            ny=tign_history.get_grid_info(idom,'ny');
            
            n=netcdf.open(tign_history.get_file_name(idom),'NC_NOWRITE');
            lid=netcdf.inqvarid(n,tign_history.tign_hist_name);
            tign=netcdf.getVar(n,lid);
            tign=squeeze(tign(1:nx,1:ny,1));
            netcdf.close(n);
        end
        function out=get_grid_info(idom,argin)
            
            % This subroutine inquires a wrfinput file in the current directory about
            % information relevant to the computation and manipulation of tign history
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
            
            n=netcdf.open(tign_history.get_file_name(idom),'NC_NOWRITE');
            
            ix=netcdf.inqdimid(n,tign_history.xname);
            iy=netcdf.inqdimid(n,tign_history.yname);
            ax=netcdf.inqdimid(n,tign_history.xatm);
            ay=netcdf.inqdimid(n,tign_history.yatm);
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
                    out=netcdf.inqatt(n,netcdf.getConstant('NC_GLOBAL'),tign_history.dxname);
                    
                case 'dy'
                    out=netcdf.inqatt(n,netcdf.getConstant('NC_GLOBAL'),tign_history.dyname);

                case 'dt'
                    out=netcdf.inqatt(n,netcdf.getConstant('NC_GLOBAL'),tign_history.dtname);
                    
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
            f=sprintf(tign_history.filefmt,idom);
        end
        function check(ncerr)
            if ncerr ~= 0
                error(netcdf.strerror(ncerr))
            end
        end
    end
end
