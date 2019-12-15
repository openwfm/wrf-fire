function y=restriction_2d(x)
    % in:
    %    x   array
    % out
    %    y   bilinear average on twice coarser grid
    % this is transpose of prolongation with weighting
    
    % to test: 
    % x=magic(2); restriction_2d(x)

    % map vector to 2d grid with zero boundary
    % average from neighbors with the same weights as prolongation
    % scaled to sum one
    % y = zeros(size(xx(2:2:end-1,2:2:end-1)));
    if any(mod(size(x),2))==0
        error('restriction_2d: input dimensions must be odd')
    end
    tw=1/(1+4*1/2+4*1/4);
    i1=0; i2=0;
    y = tw*x(2+i1:2:end-1+i1,2+i2:2:end-1+i2); 
    for i1=-1:1
        for i2=-1:1
            if (i1 ~=0) || (i2 ~=0) 
                w = tw/((1+abs(i1))*(1+abs(i2)));
                y = y + w*x(2+i1:2:end-1+i1,2+i2:2:end-1+i2);
            end
        end
    end
end