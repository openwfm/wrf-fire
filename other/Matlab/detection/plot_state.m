    function plot_state(fig,red,s,T,obs,c)
        % fig   figure number
        % T     variable displayed
        % s     title string
        % obs   observations structure
        % red   reduced structure, for bounds
        % c     contour heights
        base_time=red.min_tign;
        fprintf('Figure %i %s',fig,s) 
        figure(fig), clf
        if exist('T') && ~isempty(T),
            mint=min(T(:));
            maxt=max(T(:));
            fprintf(' tign min %g max %g',mint,maxt)
            tol=1;
            T(T(:)>max(T(:))-tol)=NaN;      
            h=surf(red.fxlong,red.fxlat,T-base_time);
            set(h,'EdgeAlpha',0,'FaceAlpha',0.5); % show faces onl   
            hold on
        end
        if exist('c') && ~isempty(c),
            fprintf(' contour at %s',num2str(c))
            contour3(red.fxlong,red.fxlat,T-base_time,c-base_time,'k');
            hold on
    end
        if exist('obs') && ~isempty(obs)
            for i=1:length(obs),
                fire_pixels_3d(obs(i))
                hold on
            end
        end
        hold off
        a=[red.min_lon,red.max_lon,red.min_lat,red.max_lat,...
            red.min_tign-base_time-1,red.max_tign-base_time];
        axis manual
        axis(a)
        xlabel('Longitude'),ylabel('Latitude'),zlabel('Days')
        title(s)
        grid on
        drawnow
        fprintf('\n')
  

    function fire_pixels_3d(x)
        kk=find(x.data(:)>=7);
        if ~isempty(kk),
        rlon=0.5*abs(x.lon(end)-x.lon(1))/(length(x.lon)-1);
        rlat=0.5*abs(x.lat(end)-x.lat(1))/(length(x.lat)-1);
        lon1=x.xlon(kk)-rlon;
        lon2=x.xlon(kk)+rlon;
        lat1=x.xlat(kk)-rlat;
        lat2=x.xlat(kk)+rlat;
        X=[lon1,lon2,lon2,lon1]';
        Y=[lat1,lat1,lat2,lat2]';
        Z=ones(size(X))*(x.time-base_time);
        cmap=cmapmod14;
        C=cmap(x.data(kk)'+1,:);
        C=reshape(C,length(kk),1,3);
        patch(X,Y,Z,C);
    end
end
    end