function y=flat(fun,x,nn,varargin)    
    % y=flat(fun,x,nn)
    % y=flat(fun,x,nn,a,...)
    % reshape x to nn execute fun and flatten the result
    % f can have additional arguments given at the end
    xx=reshape(x,nn);
    y=fun(xx,varargin{:});
    y=y(:);
end