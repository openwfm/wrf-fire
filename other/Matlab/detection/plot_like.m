function plot_like(nodetw,Peak,Wpos,Wneg)
T=[Peak-3*Wneg,Peak+5*Wpos];
n=1000;
T=[T(1):(T(2)-T(1))/n:T(2)];
[v0_n,v1_n]=like1(-nodetw,T,Peak,Wpos,Wneg);
[v0_y,v1_y]=like1(1,T,Peak,Wpos,Wneg);
figure(11)
plot(T,v0_n,'--',T,v0_y,'-')
title('Data likelihood')
xlabel('Time from fire arrival')
legend('No fire detection','Fire detected')
a=axis;a(3)=0;a(4)=1.5;axis(a)
grid
figure(12)
plot(T,v1_n,'--',T,v1_y,'-')
title('Forcing')
xlabel('Time from fire arrival')
legend('No fire detection','Fire detected')
grid
end
