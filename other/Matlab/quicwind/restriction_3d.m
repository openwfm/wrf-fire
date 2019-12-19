function y=restriction_3d(x)
    % in:
    %    x   array
    % out
    %    y   bilinear average on twice coarser grid
    % this is transpose of prolongation with weighting
    
    % to test: 
    % x=magic(2); restriction_3d(x)

    % map vector to 3d grid with zero boundary
    % average from neighbors with the same weights as prolongation
    % scaled to sum one
    if any(mod(size(x),2))==0
        error('restriction_3d: input dimensions must be odd')
    end
    tw=1/(1+6*1/2+12*1/4+8*1/8);
    i1=0; i2=0; i3=0;
    y = tw*x(2+i1:2:end-1+i1,2+i2:2:end-1+i2,2+i3:2:end-1+i3); 
    for i1=-1:1
        for i2=-1:1
            for i3=-1:1
                if (i1 ~=0) || (i2 ~=0) || (i3 ~=0)
                    w = tw/((1+abs(i1))*(1+abs(i2))*(1+abs(i3)));
                    y = y + w*x(2+i1:2:end-1+i1,2+i2:2:end-1+i2,2+i3:2:end-1+i3);
                end
            end
        end
    end
end