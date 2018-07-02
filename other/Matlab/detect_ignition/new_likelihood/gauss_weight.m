function [ g_weight ] = gauss_weight( radius)
%computes normalizing constant for mixture density

%compute weight
w_sum=0;
for i = 1:2*radius
    for j =1:2*radius
        w_sum = w_sum + gauss_part(radius,radius,i,j,radius);
    end
end

g_weight = 1/w_sum;

end

