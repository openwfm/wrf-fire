function h=read_wrfout_sel(files,vars)
% h=readsel_wfout(files,vars)
% read selected time levels from wrfouts
% in 
%     files     cell array of file names
%     vars     cell array of variable names
% out
%     h         structure with the selected variables

h=[];
for k=1:length(files)
    [f,dims]=nc2struct(files{k},vars,{});
    if isempty(h),
        h=f;
    else
        for j=1:length(vars)
            field=lower(vars{j});
            n=length(dims.(field));
            h.(field)=cat(n,h.(field),f.(field));
        end
    end
end


