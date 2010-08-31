function err=diff_runs(runs,vars,root,wrffile,timestep)
% test if runs give the same results
% example
%runs={'1x1','1x6','6x1','2x3','2x3','1x2','2x1'}
%vars={'UAH','VAH','UF','VF'}
%root='hill-';
%wrffile='wrfrst_d01_0001-01-01_00:01:00';
%diff_runs(runs,vars,root,wrffile,[])
%wrffile='wrfout_d01_2006-02-23_12:42:00'
%diff_runs(runs,vars,root,wrffile,1)

file=[root,runs{1},'/',wrffile];
p1=nc2struct(file,vars,{},timestep);
for i=2:length(runs)
    run=runs{i};
    file=[root,run,'/',wrffile];
    p=nc2struct(file,vars,{},timestep);
    for j=1:length(vars)
        var=lower(vars{j});
        e=big(getfield(p1,var)-getfield(p,var));
        err(i,j)=e;
    end
end
timestep
runs
err
