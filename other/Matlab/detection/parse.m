function val=parse(str,sep1,sep2)
    len1=length(sep1);
    len2=length(sep2);
    p1=strfind(str,sep1);
    if length(p1) ~= 1,
s        error(['beging string ',sep1,' must occur exactly once'])
    end
    p1 = p1 + length(sep1);
    p2=strfind(str,sep2);
    p2=p2(p2>p1);
    if length(p2) == 0,
        error(['matching end string ',sep2,' not found'])
    end
    val = str(p1:p2-1);
end