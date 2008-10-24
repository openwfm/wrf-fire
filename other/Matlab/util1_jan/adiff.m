function d=adiff(root1,tiles1,root2,tiles2,steps)
% d=adiff(root1,tiles1,root2,tiles2,steps)
n=length(steps);
for i=1:n
        step=steps(i);
	a1=read_array_tiles(root1,tiles1,step);
	a2=read_array_tiles(root2,tiles2,step);
	d(i)=big(a1-a2)/max([big(a1),big(a2),realmin]);
	disp(d(i))
end
        
