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
            num_hours=(maxt-mint)*24;
            step_hours=max(6,round(num_hours/100));
            hours=[mint:step_hours/24:maxt];
            % T(T(:)>max(T(:))-tol)=NaN;      
            h=contour3(red.fxlong,red.fxlat,T-base_time,hours-base_time);
            % set(h,'EdgeAlpha',0,'FaceAlpha',0.5); % show faces onl   
            hold on
        end
        if exist('c') && ~isempty(c),
            fprintf(' contour at %s',num2str(c))
            contour3(red.fxlong,red.fxlat,T-base_time,c-base_time,'k');
            hold on
        end
        if exist('obs') && ~isempty(obs)
            fire_pixels3d(obs,base_time)
        end
        disp_bounds=[red.min_lon,red.max_lon,red.min_lat,red.max_lat];
        a=[disp_bounds,red.min_tign-base_time-1,red.max_tign-base_time];
        axis manual
        axis(a)
        xlabel('Longitude'),ylabel('Latitude'),zlabel('Days')
        title(s)
        grid on
        drawnow
        fprintf('\n')
end
  