function h=ncreadandcat(files,vars)
% h=ncreadandjoin(files,vars)
% in 
%      files      cell array, names of files to read
%      vars       cell array, names of variables to read 
%
% out 
%      h          h.vars{i} contains the variable, concatenated along the last dimension 
for i=1:length(files)
    [f,dims]=nc2struct(files{i},vars,{});
    if ~exist('h','var'),
        h=f;
    else
        for j=1:length(vars)
            field=lower(vars{j});
            n=length(dims.(field));
            h.(field)=cat(n,h.(field),f.(field));
        end
    end
end
     
