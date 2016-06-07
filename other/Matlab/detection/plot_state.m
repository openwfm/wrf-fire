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
            fire_pixels(obs,base_time,3)
        end
        a=[red.min_lon,red.max_lon,red.min_lat,red.max_lat,...
            red.min_tign-base_time-1,red.max_tign-base_time];
        axis manual
        axis(a)
        xlabel('Longitude'),ylabel('Latitude'),zlabel('Days')
        title(s)
        grid on
        drawnow
        fprintf('\n')
end
  