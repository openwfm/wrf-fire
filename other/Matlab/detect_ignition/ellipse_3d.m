function [ ] = ellipse_fit( data,ci ,rate_vector,test_flag)
% function takes in a matrix of points (data) and a confidence interval
% (ci)and plots a 3d cone of the fire. rate_vector is best guess as what
% direction in which the fire spreads most rapidly

%test_flag =1 tells function you are
% using random data. Used to scale figure window 

% fitting of ellipse based on code from 
% http://www.visiondummy.com/2014/04/draw-error-ellipse-representing-covariance-matrix/

covariance = cov(data);
[eigenvec, eigenval ] = eig(covariance);

% Get the index of the largest eigenvector
[largest_eigenvec_ind_c, r] = find(eigenval == max(max(eigenval)));
largest_eigenvec = eigenvec(:, largest_eigenvec_ind_c);

% Get the largest eigenvalue
largest_eigenval = max(max(eigenval));

% Get the smallest eigenvector and eigenvalue
if(largest_eigenvec_ind_c == 1)
    smallest_eigenval = max(eigenval(:,2));
    smallest_eigenvec = eigenvec(:,2);
else
    smallest_eigenval = max(eigenval(:,1));
    smallest_eigenvec = eigenvec(1,:);
end


% Calculate the angle between the x-axis and the largest eigenvector
angle = atan2(largest_eigenvec(2), largest_eigenvec(1));

% This angle is between -pi and pi.
% Let's shift it such that the angle is between 0 and 2pi
if(angle < 0)
    angle = angle + 2*pi;
end

% Get the coordinates of the data mean
avg = mean(data);

% Get the 95% confidence interval error ellipse
%chisquare_val = 2.4477;
%chisquare_val = 2.2;
chisquare_val = ci;

%parameters of initial ellipse x^2/a^2 + y^2/b^2 = 1 
phi = angle;
X0=avg(1);
Y0=avg(2);
a=chisquare_val*sqrt(largest_eigenval);
b=chisquare_val*sqrt(smallest_eigenval);

%set up mesh
theta_incs = 40;
theta_grid = linspace(0,2*pi,theta_incs);
time_incs = 20;
time_grid = linspace(0,1,time_incs);
[u,t] = meshgrid(theta_grid,time_grid);



% the ellipse in x and y coordinates 
ellipse_x_r  = a*cos( theta_grid );
ellipse_y_r  = b*sin( theta_grid );

%Define a rotation matrix
R = [ cos(phi) sin(phi); -sin(phi) cos(phi) ];

%let's rotate the ellipse to some angle phi
r_ellipse = [ellipse_x_r;ellipse_y_r]' * R;

% Draw the error ellipse
% axis square
plot(r_ellipse(:,1) + X0,r_ellipse(:,2) + Y0,'-')
hold on;

%find dot product between rate_vector and axis of ellipse
largest_eigenvec
axis_dot = dot(largest_eigenvec,rate_vector)

%find location of focus of ellipse
f = sqrt(a^2-b^2);
%if axis_dot >=0
if (axis_dot >= 0)
   f_x = X0-f*cos(phi);
   f_y = Y0-f*sin(phi);
%if axis_dot <0
else
   f_x = X0+f*cos(phi);
   f_y = Y0+f*sin(phi);
end %if   
format long g
disp('Coordinates of focus: ')
fprintf('Lon: %d  Lat: %d  \n',f_x,f_y)
%plot location of focus of ellipse
plot(f_x,f_y,'*');

%generate surface for unrotated system

if (axis_dot >= 0)
   x_s =  (f*t + a*cos(u).*t);
   y_s =  b*sin(u).*t;
else
   x_s =  -(f*t + a*cos(u).*t);
   y_s =  -b*sin(u).*t;
end %if
z_s = t;

%Define a rotation matrix
rot = [cos(phi) sin(phi) ; sin(phi) -cos(phi) ];

%rotate layers and shift 
x_r = zeros(time_incs,theta_incs);
y_r = x_r;

for i = 1:time_incs
  new =  rot*[x_s(i,:);y_s(i,:)];
  x_r(i,:) =  f_x + new(1,:);
  y_r(i,:) =  f_y + new(2,:);
end

%plot cone surface
view(3)
surfc(x_r,y_r,z_s)
hold on

% Draw the error ellipse
plot(r_ellipse(:,1) + X0,r_ellipse(:,2) + Y0,'-')
hold on;

% Plot the original data
plot(data(:,1), data(:,2), '.');


x_min = min(data(:,1));
x_max = max(data(:,1));
y_min = min(data(:,2));
y_max = max(data(:,2));
xlim([x_min-0.04,x_max+0.04]);
ylim([y_min-0.04,y_max+0.04]);

if test_flag == 1
    mindata = min(min(data));
    maxdata = max(max(data));    
    xlim([mindata-3, maxdata+3]);
    ylim([mindata-3, maxdata+3]);
end    

hold on;

% Plot the eigenvectors
 quiver(X0, Y0, -largest_eigenvec(1)*sqrt(largest_eigenval), -largest_eigenvec(2)*sqrt(largest_eigenval), '-m', 'LineWidth',2);
 %quiver(X0, Y0, smallest_eigenvec(1)*sqrt(smallest_eigenval), smallest_eigenvec(2)*sqrt(smallest_eigenval), '-g', 'LineWidth',2);


end

