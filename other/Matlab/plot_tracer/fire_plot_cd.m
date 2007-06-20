function k=fire_plot_cd(xfg,yfg,xcd,ycd)
% k=fire_plot(xfg,yfg) - plot tracer arrays and return number of cells with
% nonzero tracers
if ndims(xfg)~=3,
    error('xfg must have 3 dimensions')
end
if ndims(xfg)~=3,
    error('xfg must have 3 dimensions')
end
[m,n,k]=size(xfg);
if k~=4,
    error('dim3 of tracer array must be 4')
end
if any(size(xfg)~=size(yfg)),
    error('xfg and yfg must be same dimension')
end
clf
k=0;
p=[1,2,4,3];
 q=[1,2,3];
 qq=[1,2];
for i=1:m,
    for j=1:n,
        tr_x=squeeze(xfg(i,j,:));
        fire_x=squeeze(xcd(i,j,:));        
        tr_y=squeeze(yfg(i,j,:));
        fire_y=squeeze(ycd(i,j,:));
      
        %if(any(tr_x) | any(tr_y) | any(fire_x) | any(fire_y)),
        if(any(tr_x) | any(tr_y)),  
        %if(any(fire_x) | any(fire_y)),
            k=k+1;
            xcoord=tr_x(p)+i;
            ycoord=tr_y(p)+j;
            fire_xcoord=fire_x(q)+i;
            fire_ycoord=fire_y(q)+j;
            fire_xcoord1=fire_x(qq)+i;
            fire_ycoord1=fire_y(qq)+j;
            fill(xcoord,ycoord,[1,0,0]);
            hold on
            %if(any(fire_x) | any(fire_y)),
            if((fire_x(q)~=0) | (fire_y(q)~=0)),  
            plot(fire_xcoord,fire_ycoord,'ok');
            plot([fire_xcoord(1),fire_xcoord(2),fire_xcoord(3)],[fire_ycoord(1),fire_ycoord(2),fire_ycoord(3)],'-b')
            end 
            if ((fire_x(qq)~=0) | (fire_y(qq)~=0)),
            plot(fire_xcoord1,fire_ycoord1,'ok');
            plot([fire_xcoord1(1),fire_xcoord1(2)],[fire_ycoord1(1),fire_ycoord1(2)],'-b')  
            end
            plot(xcoord,ycoord,'*g')
            plot(i+[-0.5,+0.5,+0.5,-0.5,-0.5],j+[-0.5,-0.5,+0.5,+0.5,-0.5],'-k')
            % drawnow
        end
    end
    end

hold off
axis equal
fprintf('%g cells with nonzero tracers\n',k)