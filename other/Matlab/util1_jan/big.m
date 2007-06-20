function s=big(a)
% s=big(a)  
% max absolute value of any array, arbitrary number of dimensions
s=big2(abs(a));

function s=big2(a)
if isvector(a),
        s=full(max(a));
else
	b=max(a,[],ndims(a));
	s=big2(b);
end

