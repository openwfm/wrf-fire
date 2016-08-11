function [ out ] = make_grid(dims )
%function sets up grids for running simulations
%run this in the /wrf-fire/wrfv2_fire/test/ folder to populate the 
%testing grid and to start simulations at the various ignition points
%Inputs:
%  dims : size of grid, example make_grid(7) creates a 7x7 grid
%
%Outputs:
%   out : unused, what was this for ??

% lon,lat are hardwired --->> can change this
lon_min = -112.66;
lon_max = -112.63;
lat_min = 40.358;
lat_max = 40.382;

% lon_min=min(lon1,lon2);
% lon_max=max(lon1,lon2);
% lat_min=min(lat1,lat2);
% lat_max=max(lat1,lat2);

%set grid parameters
pts = zeros(dims^2,2);
d_lat = (lat_max-lat_min)/(dims-1);
d_lon = (lon_max-lon_min)/(dims-1);
lon_square = zeros(dims,dims);
lat_sqaure = lon_square;

%lists of lat, lon already done, not used....
%lat_done = [40.358 40.366 40.374 40.382];
%lon_done = [-112.66 -112.65 -112.64 -112.63];

%loop to set things up
pt_ct = 1;
for i = 1:4
    for j = 1:4
        pts_done(pt_ct,1) = lon_done(j);
        pts_done(pt_ct,2) = lat_done(i);
        pt_ct = pt_ct+1;
       
    end
end


%make square matrices
lat_col = linspace(lat_max,lat_min,dims)';
lon_row = linspace(lon_min,lon_max,dims);
for i=1:dims
    lon_square(i,:)=lon_row;
    lat_square(:,i)=lat_col;
end

%loop to create pts array
ct=1;
for i=1:dims
    for j=1:dims
        pts(ct,1)=lon_square(i,j);
        pts(ct,2)=lat_square(i,j);
        ct=ct+1;
    end
end
%create cell array with directory names
lon_str=num2str(pts(:,1));
lat_str=num2str(pts(:,2));
lon_cell=cellstr(lon_str);
lat_cell=cellstr(lat_str);

%make list of names for runs etc...
times = zeros(5,1);
names = [];
dirs = [];
spec1 = 'g_run_%d_%.4f_%.4f';
%spec2 = 'mkdir run_%d_%.4f_%.4f';
spec2 = 'cp -a em_utfire_1d_med_4km_200m g_run_%d_%.4f_%.4f';
spec3 = 'cat namelist.template | sed ''s/IGN_LON/%.4f/'' | sed ''s/IGN_LAT/%.4f/'' | sed ''s/IGN_TIME/%d/'' > namelist.input';
%loop to create directories
%uses five different ignition times --->> change this
%lp_ct =1;
for j = 1:5
    times(j) = 3600*(2*j + 3);
    for i=1:dims^2
        %fill in the names field, not used
        %names{lp_ct} = sprintf(spec1,3600*(2*j + 3),-pts(i,1),pts(i,2));
        
        %copy the directory
        system(sprintf(spec2,3600*(2*j + 3),-pts(i,1),pts(i,2)));
        
        %cd to new directory
        cd(sprintf(spec1,3600*(2*j + 3),-pts(i,1),pts(i,2)));
    
        %edit namelist
        system(sprintf(spec3,pts(i,1),pts(i,2),3600*(2*j+3)));
        
        %start job in the hex.q
        system('qsub -q hex.q run_wrf.colibri')
        
        %cd up
        cd('../')
		
        %lp_ct = lp_ct+1;
        %%%%%%%% scratch space %%%%%%%%%%
        %qsub -q hex.q run_wrf.colibri
        %cat namelist.template | sed 's/IGN_LON/-112.66/' | sed 's/IGN_LAT/40.37/' | sed 's/IGN_TIME/25200/' > namelist.input
    end
end
pause;
end

