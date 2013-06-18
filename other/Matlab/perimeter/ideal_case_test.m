for i=1:12
	for j=1:12
		long(i,j)=i+100;
		lat(i,j)=j+100;
      %  vx(i,j)=1;
      %  ux(i,j)=1;
      %  dzdxf(i,j)=1;
      %  dzdyf(i,j)=1;
	end
end
uf=ones(12,12);
vf=ones(12,12);
dzdxf=ones(12,12);
dzdyf=ones(12,12);
data='data_ideal_case.txt';
addpath ../../other/Matlab/perimeter
addpath ../../other/Matlab/util1_jan
fid = fopen(data);
data = fscanf(fid,'%g %g',[2 inf]); % It has two rows now.
data = data';
fclose(fid)
data_size=size(data);

time_now=data(1,1);

bound=100+data(2:data_size(1),:);

plot(bound(:,1),bound(:,2),'-');

result=perimeter(long,lat,uf,vf,dzdxf,dzdyf,time_now,bound);
figure
surf(result)
