function T=blending(Tf,Ta,T0,T1)
% T=blending(Tf,Ta,T0,T1)
% fire arrival times Tf at T0 and Ta at T1 into T

if all(Tf>=T0),
    T=Ta;
else
    p=2;
    f=max(Tf-T0,0).^p;
    a=max(T1-Ta,0).^p;
    T=(f.*Ta+a.*Tf)./(a+f);
end
if any(isnan(T(:))),warning('T0 too large'),end
end