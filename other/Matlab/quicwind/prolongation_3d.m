function y=prolongation_3d(x)
    % in:
    %    x   3d array
    %    nn  shape of x
    % out
    %    y   bilinear interpolation wrapped by zeros
    
    % to test: 
    % x=magic(2); prolongation_3d(x)

    % map vector to 3d grid with zero boundary
    nn=size(x);
    xx=zeros(nn+2);
    xx(2:nn(1)+1,2:nn(2)+1,2:nn(3)+1)=reshape(x,nn);
    % allocate output
    y=zeros(2*nn+1);
    % copy values on coarse points
    y(2:2:end-1,2:2:end-1,2:2:end-1)=xx(2:end-1,2:end-1,2:end-1);
    
    % averages to coarse edge midpoints in direction 1
    y(1:2:end,2:2:end-1,2:2:end-1)=0.5*(xx(1:end-1,2:end-1,2:end-1)+xx(2:end,2:end-1,2:end-1));
    % averages to coarse edge midpoints in direction 2
    y(2:2:end-1,1:2:end,2:2:end-1)=0.5*(xx(2:end-1,1:end-1,2:end-1)+xx(2:end-1,2:end,2:end-1));
    % averages to coarse edge midpoints in direction 3
    y(2:2:end-1,2:2:end-1,1:2:end)=0.5*(xx(2:end-1,2:end-1,1:end-1)+xx(2:end-1,2:end-1,2:end));
    
    % averages to coarse cell face centers in directions 1 and 2
    y(1:2:end,1:2:end,2:2:end-1)=0.25*(xx(1:end-1,1:end-1,2:end-1)+xx(2:end,1:end-1,2:end-1)+...
                             xx(1:end-1,2:end,2:end-1) + xx(2:end,2:end,2:end-1));
    % averages to coarse cell face centers in directions 2 and 3
    y(1:2:end,2:2:end-1,1:2:end)=0.25*(xx(1:end-1,2:end-1,1:end-1)+xx(2:end,2:end-1,1:end-1)+...
                             xx(1:end-1,2:end-1,2:end) + xx(2:end,2:end-1,2:end));
    % averages to coarse cell face centers in directions 2 and 3
    y(2:2:end-1,1:2:end,1:2:end)=0.25*(xx(2:end-1,1:end-1,1:end-1)+xx(2:end-1,2:end,1:end-1)+...
                             xx(2:end-1,1:end-1,2:end) + xx(2:end-1,2:end,2:end));

    % averages to coarse cell centers
    y(1:2:end,1:2:end,1:2:end)=0.125*(xx(1:end-1,1:end-1,1:end-1)+xx(2:end,1:end-1,1:end-1)+...
                             xx(1:end-1,2:end,1:end-1) + xx(1:end-1,1:end-1,2:end)+...
                             xx(2:end,2:end,1:end-1) + xx(2:end,1:end-1,2:end)+...
                             xx(1:end-1,2:end,2:end) + xx(2:end,2:end,2:end));
    
end