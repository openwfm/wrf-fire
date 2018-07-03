function plot_like_new

        % function h=heat(T)
        c = 1;  % time constant: how long the heat takes to drop to exp(-1)
        %********************************
        heat = @(T) (T<=0).*exp(T/c);
        %********************************
        % T = fire arrival time
        % 0 if T > T_now = 0, 1 at T=0, exp decay for T>= 0
        figure(1);fplot(heat,[-5*c,c]);
        xlabel('time (h)');ylabel('heat fraction (1)')
        
        % function p=prob(h)
        false_detection_rate = 0.01;  % = 1/(1+exp(b))
        b = log(1/false_detection_rate  -1);
        half_prob_heat = 0.01; % 1+exp(-a*h + b) = 2 for h=half_prob_heat 
        a = b / half_prob_heat;
        %********************************
        prob = @(h) 1./(1+exp(-a*h + b));
        %********************************
        err1 = false_detection_rate - prob(0)   
        err2 = 0.5 - prob(half_prob_heat)   
        figure(2);fplot(prob,[0,half_prob_heat*5]);
        xlabel('heat fraction (1)');ylabel('probability of detection');



 end