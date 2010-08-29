% test if parallel runs give the same results
runs={'1x1','1x6','6x1','2x3','2x3','1x2','2x1'}
vars={'UAH','VAH','UF','VF'}
root='hill-';
wrffile='wrfrst_d01_0001-01-01_00:01:00';
p1=nc2struct([root,runs{1},'/',wrffile],vars,{},[])
for i=2:length(vars)
    run=runs{i};
    p=nc2struct([root,run,'/',wrffile],vars,{},[])
    run
    for j=1:length(vars)
        var=lower(vars{j})
        err(i,j)=big(getfield(p1,var)-getfield(p,var))
    end
end
