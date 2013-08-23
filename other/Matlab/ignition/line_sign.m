function signn=line_sign(x1,y1,x2,y2,x3,y3)

% Volodymyr Kondratenko           August 2011

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Defines from which sideof the line going through (x1,y1,x2,y2) lies x3,y3
% if sign is positive -> from one side, negative -> from another
signn=(y3-y1)*(x2-x1)-(y2-y1)*(x3-x1);
end






