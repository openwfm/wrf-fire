function fig
f=gcf;
set([gca],'FontSize', 8);
i=f.Number
file=['plot_like_new',num2str(i)]
f.PaperUnits = 'inches';
mult=0.95;
f.PaperSize = mult*[3 2];
f.PaperPosition = mult*[0 0 3 2];
savefig(i,file)
% print('-deps',file)
% print('-dpng',file)
print('-dpdf',file)