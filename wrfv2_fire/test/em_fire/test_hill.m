% test if parallel runs give the same results
runs={'1x1','1x6','6x1','2x3','2x3','1x2','2x1'}
vars={'UAH','VAH','UF','VF'}
p1=nc2struct(['hill-',runs{1},'/wrfrst_d01_0001-01-01_00:01:00'],vars,{},[])
for i=2:length(cases)
    run=runs{i};
    p=nc2struct(['hill-',run,'/wrfrst_d01_0001-01-01_00:01:00'],vars,{},[])
    run
    for j=1:length(vars)
        var=lower(vars{j})
        err(i,j)=big(getfield(p1,var)-getfield(p,var))
    end
end
