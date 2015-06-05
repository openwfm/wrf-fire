function plot_like(nodetw,stretch)
Tmin=stretch(1);Tmax=stretch(2);Tneg=stretch(3);Tpos=stretch(4);
T=[Tmin-3*Tneg,Tmax+3*Tpos];
n=1000;
t=[T(1):(T(2)-T(1))/n:T(2)];
[v0_n,v1_n]=like1(-nodetw,t,stretch);
[v0_y,v1_y]=like1(1,t,stretch);
figure(11)
plot(t,v0_n,'--',t,v0_y,'-')
title('Data log likelihood')
xlabel('Time from fire arrival (h)')
legend('No fire detection','Fire detected')
va=1;
a=axis;a(3)=-va;a(4)=va;axis(a)
grid
figure(12)
plot(t,v1_n,'--',t,v1_y,'-')
title('Forcing')
xlabel('Time from fire arrival (h)')
legend('No fire detection','Fire detected')
a=axis;a(3)=-va;a(4)=va;axis(a)
grid
end
