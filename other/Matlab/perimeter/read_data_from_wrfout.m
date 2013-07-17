function ros=read_data_from_wrfout(wrfout,m,n,time)

% for witch its 3100 2600

format long

ncid = netcdf.open(wrfout,'NC_NOWRITE');

varid = netcdf.inqVarID(ncid,char('F_ROS11'));
f_ros11=netcdf.getVar(ncid,varid,[0,0,time],[m,n,1]);

varid = netcdf.inqVarID(ncid,char('F_ROS12'));
f_ros12=netcdf.getVar(ncid,varid,[0,0,time],[m,n,1]);

varid = netcdf.inqVarID(ncid,char('F_ROS13'));
f_ros13=netcdf.getVar(ncid,varid,[0,0,time],[m,n,1]);

varid = netcdf.inqVarID(ncid,char('F_ROS21'));
f_ros21=netcdf.getVar(ncid,varid,[0,0,time],[m,n,1]);

varid = netcdf.inqVarID(ncid,char('F_ROS23'));
f_ros23=netcdf.getVar(ncid,varid,[0,0,time],[m,n,1]);

varid = netcdf.inqVarID(ncid,char('F_ROS31'));
f_ros31=netcdf.getVar(ncid,varid,[0,0,time],[m,n,1]);

varid = netcdf.inqVarID(ncid,char('F_ROS32'));
f_ros32=netcdf.getVar(ncid,varid,[0,0,time],[m,n,1]);

varid = netcdf.inqVarID(ncid,char('F_ROS33'));
f_ros33=netcdf.getVar(ncid,varid,[0,0,time],[m,n,1]);

netcdf.close(ncid);

ros=zeros(m,n,3,3);

ros(:,:,1,1)=f_ros11;

ros(:,:,1,2)=f_ros12;

ros(:,:,1,3)=f_ros13;

ros(:,:,3,1)=f_ros31;

ros(:,:,3,2)=f_ros32;

ros(:,:,3,3)=f_ros33;

ros(:,:,2,1)=f_ros21;

ros(:,:,2,3)=f_ros23;
end

function a=ncread(filename,varname,time)
% a=ncread(filename,varname)
% return one variable as matlab array without extra dimensions

% Jan Mandel, September 2008

a=ncextract2(ncdump(filename,varname),time);
end

function v=ncextract(p)
% v=ncextract(p)
% extract v as matlab array from structure returned by ncdump
% for one variable

% Jan Mandel, September 2008

% time needed = 49

v=squeeze(double(p.var_value(:,:,time)));
end
      





