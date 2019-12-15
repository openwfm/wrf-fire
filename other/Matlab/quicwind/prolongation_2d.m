function y=prolongation_2d(x)
    % in:
    %    x   2d array
    %    nn  shape of x
    % out
    %    y   bilinear interpolation wrapped by zeros
    
    % to test: 
    % x=magic(2); prolongation_2d(x)

    % map vector to 2d grid with zero boundary
    nn=size(x);
    xx=zeros(nn+2);
    xx(2:nn(1)+1,2:nn(2)+1)=reshape(x,nn);
    % allocate output
    y=zeros(2*nn+1);
    % copy values on coarse points
    y(2:2:end-1,2:2:end-1)=xx(2:end-1,2:end-1);
    % averages to coarse edge midpoints in direction 1
    y(1:2:end,2:2:end-1)=0.5*(xx(1:end-1,2:end-1)+xx(2:end,2:end-1));
    % averages to coarse edge midpoints in direction 2
    y(2:2:end-1,1:2:end)=0.5*(xx(2:end-1,1:end-1)+xx(2:end-1,2:end));
    % averages to coarse cell centers in directions 1 and 2
    y(1:2:end,1:2:end)=0.25*(xx(1:end-1,1:end-1)+xx(2:end,1:end-1)+...
                             xx(1:end-1,2:end)  +xx(2:end,2:end)); 
end