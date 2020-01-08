    function varargout=detection_objective(tign,h,g,params,red,varargin)
    % J              =detection_objective(tign,h,g,params,red)
    % [J,delta]      =detection_objective(tign,h,g,params,red)
    % [J,delta,f0,f1]=detection_objective(tign,h,g,params,red)
    % compute objective function and optionally gradient delta direction
    %
    % input:
    %     tign   prior state
    %     h      increment
    %     g      detection structur?e from load_subset_detections
    %     params parameters set in detect_fit_level2
    %
    % The objective function is
    % J = h'*A*h - log likelihood of tign+h
    % to compute data likelihood only, set h=0 and params.alpha=0
    %
    % output
    %     J      objective function = penalty minus log likelihood
    %     delta  (optional) preconditioned search direction, grad J(tign+h)
    %     f0     (optional) contributions of mesh cells to log likelihood
    %     f1     (optional) contributions of mesh cells to the derivative
    %                   of the log likelihood 
    
    if nargin >=6
        new_like = 1;
        p_like_spline = varargin{1};
        p_deriv_spline = varargin{2};
        n_deriv_spline = varargin{3};
    else
        new_like = 0;
    end

    like_plots=0;
    
        T=tign+h;
        f0=0;
        f1=0;
        %%% altered weighting of the different data sources, store params
        %%% first
        params_bak = params;
        
        %% different weighting scheme if perimter data present
        perims_present = 0;
        for k = 1:length(g)
            if strcmp(g(k).file(1:3),'PER')
                if ~perims_present
                    fprintf('Perimeter data present \n');
                    perims_present = input_num('1 to adjust satellite weights ', 1);
                end
            end
        end
            
        % compute new likelihood stuff here,
        
        if new_like
            fprintf('Using likelihood splines.\n')
            % load splines.mat;
            %[like_spline,fitness1] = createFit(t,log_like);
            %[deriv_spline,fitness2] = createFit(t,log_like_deriv);
        end
        
        for k=1:length(g)
            % load params in case it has been changed
            params = params_bak;
            
% routine for weighting modis and virrs less when [perimeter data present
            non_perim_weight = 0.5;
            if ~strcmp(g(k).file(1:3),'PER') && perims_present
                fprintf('Changing weight of satellite detection \n');
                params.weight(3:5) = non_perim_weight*params.weight(3:5);
            end
              
            %weight by time
            time_weight = 1;
            if time_weight
                pwr = 0.5;
                %multiplier = exp((g(k).time-g(length(g)).time))^pwr;
                multiplier = ((g(k).time-min(tign(:)))/(g(length(g)).time-min(tign(:))))^pwr;
                params.weight = multiplier*params.weight;
                fprintf('Changing weights by %f \n',multiplier);
            end
            
% routine for changing modis weights
%             modis_weight = 0.2;
%             if strcmp(g(k).file(1:3),'MOD') | strcmp(g(k).file(1:3),'MYD')
%             %if g(k).file(1:3) == 'MOD' | g(k).file(1:3) == 'MYD'
%                 fprintf('Changing detection weights for MODIS data \n');
%                 params.weight(3:5) = modis_weight*params.weight(3:5);
%             end
            psi = ...
                + params.weight(1)*(g(k).fxdata==3)... 
                + params.weight(2)*(g(k).fxdata==5)...
                + params.weight(3)*(g(k).fxdata==7)...
                + params.weight(4)*(g(k).fxdata==8)...
                + params.weight(5)*(g(k).fxdata==9); 
            if new_like
                [f0k,f1k] = evaluate_likes(psi,g(k).time-T,p_like_spline,p_deriv_spline,n_deriv_spline);
                f0k = f0k - max(f0k(:));
                f1k = f1k-max(f1k(:));
                %f1k = -f1k; %% why
            else
                [f0k,f1k]=like2(psi,g(k).time-T,params.TC*params.stretch);
            end
            detections=sum(psi(:)>0);
            f0=f0+f0k;
            f1=f1+f1k;
%             if k == length(g)
%                 figure,mesh(f0),title('likelihood');
%                 figure,mesh(f1),title('derivative');
            if like_plots>1,
                plot_loglike(4,f0k,'Data likelihood',red)
                plot_loglike(5,f1k,'Data likelihood derivative',red)
                plot_loglike(6,psi,'psi',red)
                plot_loglike(7,g(k).time-T,'Time since fire arrival',red)
                hold on;contour3(red.fxlong,red.fxlat,g(k).time-T,[0 0],'r')
                hold off
                drawnow
            end
        end
        if like_plots>0,
            plot_loglike(2,f0,'Data likelihood',red)
            plot_loglike(3,f1,'Data likelihood derivative',red)
        drawnow
        end
        F=f1;
        % objective function and preconditioned gradient
        Ah = poisson_fft2(h,[params.dx,params.dy],params.power);
        % compute both parts of the objective function and compare
        J1 = 0.5*(h(:)'*Ah(:));
        J2 = -ssum(f0);
        J = params.alpha*J1 + J2;
        fprintf('Objective function J=%g (J1=%g, J2=%g)\n',J,J1,J2);
        varargout{1}=J;
        if nargout==1,
            return
        end
        gradJ = params.alpha*Ah + F;
        fprintf('Gradient: norm Ah %g norm F %g\n', norm(Ah,2), norm(F,2));
        if params.doplot,
            plotstate(7,f0,'Detection likelihood',0);
            plotstate(8,F,'Detection likelihood derivative',0);
            plotstate(10,gradJ,'gradient of J',0);
        end
        if params.alpha>0,
            delta = solve_saddle(params.Constr_ign,h,F,0,...
                @(u) poisson_fft2(u,[params.dx,params.dy],-params.power)/params.alpha);
            bump_remove = 1;
            if bump_remove
                % change search to keep bump from forming here
                temp_h = -delta/big(delta);
                temp_analysis = tign+temp_h;
                %figure,mesh(temp_analysis)
                corner_time = min([temp_analysis(1,1),temp_analysis(1,end),...
                    temp_analysis(end,1),temp_analysis(end,end)]);
                if max(temp_analysis(:)) > corner_time
                    fprintf('Bump forming, inside the detection_objective function \n');
                    %figure,mesh(temp_analysis);
                    mask_h = temp_analysis >= corner_time;
                    delta(mask_h) = 0;
                    %figure,mesh(delta);
                end
            end %bump removal
            varargout{2}=delta; %search in detect_fit_level2
        end
        if nargout >= 3,
            varargout{3}=f0;
        end
        if nargout >= 4,
            varargout{4}=f1;
        end
        % figure(17);mesh(red.fxlong,red.fxlat,delta),title('delta')
        % plotstate(11,delta,'Preconditioned gradient',0);
        %fprintf('norm(grad(J))=%g norm(delta)=%g\n',norm(gradJ,'fro'),norm(delta,'fro'))
    end


