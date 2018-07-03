%function plot_like_new

        % function h=heat(T)
        c = 1;  % time constant (h) : how long the heat takes to drop to exp(-1)
        %********************************
        heat = @(T) (T<=0).*exp(min(T,0)/c);
        %********************************
        % T = fire arrival time
        % 0 if T > T_now = 0, 1 at T=0, exp decay for T>= 0
        figure(1);fplot(heat,[-5*c,c]);
        xlabel('time (h)');ylabel('heat fraction (1)')
        
        % function p=prob(h)
        false_detection_rate = 0.001;  % = 1/(1+exp(b))
        b = log(1/false_detection_rate  -1);
        half_prob_heat = 0.05; % 1+exp(-a*h + b) = 2 for h=half_prob_heat 
        a = b / half_prob_heat;
        %********************************
        prob = @(h) 1./(1+exp(-a*h + b));
        %********************************
        err1 = false_detection_rate - prob(0)  
        err2 = 0.5 - prob(half_prob_heat)   
        figure(2);fplot(prob,[0,half_prob_heat*5]);
        xlabel('heat fraction (1)');ylabel('probability of detection');
        
        figure(3);fplot(@(T) prob(heat(-T)),[-c,10*c]);
        xlabel('time since fire arrival (h)');ylabel('probability of detection');
        
        m = 10                  % mesh refinement
        dx=300/m, dy=300/m,    % grid step
        ix = 10*m, iy=10*m     % grid center indices 
        nx = 2*ix-1, ny=2*iy-1     % grid dimension
        sigma = 500,     % geolocation error stdev
        R = 1            % rate of spread  m/s
        R = 3600*R       % rate of spread  m/h
     
        [x,y]=ndgrid([0:nx-1]*dx,[0:ny-1]*dy);  % mesh
        
        ssum = @(a) sum(a(:));    % utility: sum of array

        % gaussian weights
        d2 = (x(ix,iy)-x).^2 + (y(ix,iy)-y).^2; % distance ^2  
        w=exp(-d2/(sigma^2));                   % gaussian kernel
        w=w/sum(w(:));                          % weights add up to 1
        
        %********************************
        tign = @(T)T+(x-x(ix,iy))/R;            % fire arrival time
        prbx = @(T)prob(heat(tign(T)));         % detection probability 
        pgeo = @(T)ssum(w.*prob(heat(tign(T))));% weighted by geolocation error
        ageo = @(T)arrayfun(pgeo,T);            % same for array argument
        %********************************
        figure(4);fplot(@(T) log(ageo(-T)),[-c,10*c]);
        xlabel('time since fire arrival (h)');ylabel('probability of detection');
        %********************************
        %********************************
        
        
        


 %end