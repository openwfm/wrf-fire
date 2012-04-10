function [x,y]=line_inter(x1,y1,x2,y2,x3,y3,x4,y4)

% Volodymyr Kondratenko           August 2011

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% finds the distance from (x3,y3) to line through (x1,y1) and (x2,y2)

m21=(y2-y1)/(x2-x1);
m43=(y4-y3)/(x4-x3);
x = (x1*m21-x3*m43-y1+y3)/(m21-m43);
y = y1 + m21*(x-x1);

if (x2==x1) 
    x=x1;
    m21=0;
y = y3 + m43*(x-x3);

end

if (x3==x4)
    x=x3;
    m43=0;
y = y1 + m21*(x-x1);

end

end

