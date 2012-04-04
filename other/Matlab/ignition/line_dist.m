function dist=line_dist(x1,y1,x2,y2,x3,y3)

% Volodymyr Kondratenko           August 2011

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% finds the distance from (x3,y3) to line through (x1,y1) and (x2,y2)
A=y1-y2;
B=x2-x1;
C=x1*y2-y1*x2;
dist=(abs(A*x3+B*y3+C))/(sqrt(A*A+B*B));

end






