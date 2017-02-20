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
%           load t
%    out    cell array ready to be written to csh file by
%           cell2csv(out,'filename.csv')
%           One column is added with the fire arrival time.
%           Load the csv file into excel and change the format of the
%           column to "Time"


insert_col_pos=5; % number of the column to add 
end_times=char(t.times(:,end)');
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
tign_g_interp = scatteredInterpolant(t.fxlat(:),t.fxlong(:),t.tign_g(:));
for i=2:m
    burn_seconds=tign_g_interp(raw{i,1},raw{i,2});
    burn_datenum = start_datenum + burn_seconds/(24*60*60);
    if isnan(burn_datenum)
        burn_datestr=''
    else 
        burn_datestr = datestr(burn_datenum,'yyyy-mm-dd HH:MM:SS');
    end
    out(i,insert_col_pos)={burn_datestr};
    disp(out(i,:))
end
