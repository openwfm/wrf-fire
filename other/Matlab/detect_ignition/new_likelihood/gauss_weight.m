function [ g_weight ] = gauss_weight(sig)
%computes normalizing constant for mixture density

% radius is calculated for the the distance at which the gaussian goes to
% machine precision 10^-16
radius = round(sig*sqrt(32)*log(10))+1;
%compute weight
w_sum=0;
for i = 1:2*radius
    for j =1:2*radius
        w_sum = w_sum + gauss_part(radius,radius,i,j,sig);
    end
end

g_weight = 1/w_sum;

end

