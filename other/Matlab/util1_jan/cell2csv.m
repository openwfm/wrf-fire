function cell2csv(raw,file)
% cell2csv(raw,file)
% Write cell array raw to csv file
% Needed because xlswrite does not work
% Usage:
%     [num,txt,raw] = xlsread('file.xls')
%     cell2csv(raw,'file.csv')
% will copy xls file to csv file
f=fopen(file,'w');
[m,n]=size(raw);
for i=1:m
    for j=1:n
        a=raw{i,j};
        if isnan(a),
        elseif isnumeric(a) & isscalar(a),
            fprintf(f,'%20.12g',a);
        elseif ischar(a),
            fprintf(f,'"%s"',a);
        else
            error('entries must be numeric scalars or strings')
        end
        if j<n,
            fprintf(f,',');
        else
            fprintf(f,'\n');
        end
    end
end
fclose(f);
%t=cell2table(raw);
%writetable(t,file,'Delimiter',',','QuoteStrings',true,...
%    'WriteVariableNames',false)
%end
        