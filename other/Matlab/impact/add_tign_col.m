function out=add_tign_col(raw,t)
% out=add_tign_col(raw,t)
% Input:
%    raw    spreadsheed loaded by [num,txt,raw]=xlsread('filename.xls')
%           row 1 are headings
%           column 1 is latitude
%           column 2 is longitude
%    t      structure created from wrfout as follows:
%           t=read_wrfout_tign('wrfout...')
%           save t, copy t.mat to another computer if needed, load t
% Output:
%    out    cell array ready to be written to csh file by
%           cell2csv(out,'filename.csv')
%           One column is added with the fire arrival time.
%           Load the csv file into excel and change the format of the
%           column to "Time"
%
% Usage:
%    load t
%    [num,txt,raw]=xlsread('AssetsDB-20170216.xls');
%    out=add_tign_col(raw,t);
%    cell2csv(out,'AssetsDB.csv')

graphics=0;

insert_col_pos=5; % number of the column to add 
end_times=t.times(end,:);
end_datenum = datenum(end_times); % in days from some instant in the past
end_minutes=t.xtime(end); % from simulation start
start_datenum=end_datenum-end_minutes/(24*60);
fprintf('Simulation start %s\n',datestr(start_datenum));
[m,n]=size(raw);
out=cell(m,n+1);
out(:,1:insert_col_pos-1)=raw(:,1:insert_col_pos-1);
out(:,insert_col_pos+1:end)=raw(:,insert_col_pos:end);
out(1,insert_col_pos)={'Time burned'};
lats=cell2mat(raw(2:end,1));
lons=cell2mat(raw(2:end,2));
tign=gridinterp(t.fxlat,t.fxlong,t.tign_g,lats,lons);

if (graphics),
disp('drawing')
clf
%h=mesh(t.fxlat,t.fxlong,t.tign_g);
%alpha=0.1;set(h,'EdgeAlpha',alpha,'FaceAlpha',alpha)
hold off
h=contour3(t.fxlat,t.fxlong,t.tign_g,[0:4*3600:max(tign)],'b');
hold on 
plot3(lats,lons,tign,'k*')
hold off
drawnow
end

burn_datenum = start_datenum + tign/(24*60*60);
for i=1:length(lats)
    fprintf('lat=%8.4f long=%8.4f tign=%g\n',lats(i),lons(i),tign(i))
    %plot(lats(i),lons(i),'k*'),drawnow
    if isnan(burn_datenum(i)) | burn_datenum(i) >= end_datenum-1/100,
        burn_datestr{i}='';
    else
        burn_datestr{i} = datestr(burn_datenum(i),'yyyy-mm-dd HH:MM:SS');
    end
end
out(2:end,insert_col_pos)=burn_datestr;
    % disp(out(i+1,:))
hold off
grid on
end
