function diff_fig(old,new,var,i,j)
var_old=old.(var);
var_old=var_old(i,j);
var_new=new.(var);
var_new=var_new(i,j);
[ii,jj]=ndgrid(i,j);
subplot(1,3,1)
mesh(ii,jj,var_old)
title([var,' old'],'Interpreter','none')
subplot(1,3,2)
mesh(ii,jj,var_new)
title([var,' new'],'Interpreter','none')
subplot(1,3,3)
mesh(ii,jj,var_new-var_old)
title([var,' dif'],'Interpreter','none')
end


    