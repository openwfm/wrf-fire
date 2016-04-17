function like_test(like)
stretch=[0.5,10,5,10];
t=[-10:0.01:30];
[p0,p1]=like(1,t,stretch);
[n0,n1]=like(-1,t,stretch);
% figure(1),clf
figure
plot(t,p0,'-',t,n0,'--');
xlabel('Time since fire arrival (h)')
ylabel('Log probability')
legend('Fire detected','Land/water no fire')
grid on
figure
% figure(2),clf
plot(t,p1,t,n1)
grid on

