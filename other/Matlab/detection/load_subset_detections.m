function g = load_subset_detections(prefix,p,red,time_bounds,fig)
% 

itime=find(p.time>=time_bounds(1) & p.time<=time_bounds(2));
d=p.file(itime);      % files within the specified time
t=p.time(itime);
fprintf('Selected %i files in the given time bounds, from %i total.\n',...
    length(d),length(p.time))

k=0;
for i=1:length(d),
    file=d{i};
    fprintf('%s file %s ',stime(t(i),red),file);
    v=readmod14(prefix,file,'silent');
    % select fire detection within the domain
    xj=find(v.lon > red.min_lon & v.lon < red.max_lon);
    xi=find(v.lat > red.min_lat & v.lat < red.max_lat);
    ax=[red.min_lon red.max_lon red.min_lat red.max_lat];
    if isempty(xi) | isempty(xj)
        fprintf('outside of the domain\n');
    else
        x=[];
        x.data=v.data(xi,xj);    % subset data
        x.det(1)=sum(x.data(:)==3); % water 
        x.det(2)=sum(x.data(:)==5);  % land
        x.det(3)=sum((x.data(:)==7)); % low confidence fire
        x.det(4)=sum((x.data(:)==8)); % medium confidence fire
        x.det(5)=sum((x.data(:)==9)); % high confidence fire
        if ~any(x.det) 
            fprintf(' no data in the domain\n')
        else
            k=k+1;
            fprintf('water %i land %i fire low %i med %i high %i\n',x.det)
            x.axis=[red.min_lon,red.max_lon,red.min_lat,red.max_lat];
            x.file=v.file; 
            x.time=v.time;
            x.lon=v.lon(xj);
            x.lat=v.lat(xi);
            [x.xlon,x.xlat]=meshgrid(x.lon,x.lat);
            x.fxdata=interp2(v.lon,v.lat,v.data,red.fxlong,red.fxlat,'nearest');
            if fig.fig_interp,  % map interpolated data to reduced domain
                figure(fig.fig_interp)
                cmap=cmapmod14;
                c=reshape(cmap(x.fxdata+1,:),[size(x.fxdata),3]);
                surf(red.fxlong,red.fxlat,zeros(size(red.fxlat)),c,'EdgeAlpha',0.2);
                title(['Detection interpolated to fire mesh ',stime(x.time,red)])
            end
            g(k)=x;   % store the data structure
            if fig.fig_map,
                figure(fig.fig_map);clf
                showmod14(x)
                hold on
                contour(red.fxlong,red.fxlat,red.tign,[v.time v.time],'-k');
                %fprintf('image time            %s\n',datestr(x.time));
                if plot_also_wind, 
                    if x.time >= ss.min_time && x.time <= ss.max_time,
                        step=interp1(ss.time,ss.num,x.time);
                        step0=floor(step);
                        if step0 < ss.steps,
                            step1 = step0+1;
                        else
                            step1=step0;
                            step0=step1-1;
                        end
                        w0=step1-step;
                        w1=step-step0;
                        uu=w0*ss.uh(:,:,step0)+w1*ss.uh(:,:,step1);
                        vv=w0*ss.vh(:,:,step0)+w1*ss.vh(:,:,step1);
                        fprintf('wind interpolated to %s from\n',datestr(x.time))
                        fprintf('step %i %s weight %8.3f\n',step0,datestr(ss.time(step0)),w0)
                        fprintf('step %i %s weight %8.3f\n',step1,datestr(ss.time(step1)),w1)
                        fprintf('wind interpolated to %s from\n',datestr(x.time))
                        sc=0.006;quiver(w.xlong,w.xlat,sc*uu,sc*vv,0);
                    end
                end
                hold off
                axis(ax)
                drawnow
                % M(k)=getframe(gcf);
                % print(fig.fig_map,'-dpng',['fig',v.timestr]);
            end
            if fig.fig_3d,
                hold on; fire_pixels_3d(fig.fig_3d,x,red.base_time)
            end
        end
    end
end
if ~exist('g','var'),
    fprintf('No files found.')
    g=struct([]);
end
fprintf('%i detections selected\n',length(g))
