function [out_params,fval] = est_learning_rate(ptp_data,params,plotEstimation)
    
    % fminsearch will find the best gamma
    if plotEstimation
        options = optimset('PlotFcns',@optimplotfval);    
        [out_params,fval] = fminsearch(@fit_learning,params,options);
    else
        options = optimset('MaxFunEvals',2e3);
        [out_params,fval] = fminsearch(@fit_learning,params,options);
    end
    % A nested function, so that the @mink_distance could also take in
    % extra acrguments (diag, side1, and side2) from the parent function
    % @find_min_gamma
    function sse = fit_learning(x)
       
        trials = 1:length(ptp_data);
        
        % For optimize both rate and offset
        y_hat = x(1) + (1 - exp(-x(2) * (trials - 1))) * (1-x(1))';
        
        % If the intercept is below 0, have a terrible sse
        if x(1) < 0 
            sse = Inf;
        else
            sse = nansum(abs(ptp_data - y_hat).^2);
        end
    end

end