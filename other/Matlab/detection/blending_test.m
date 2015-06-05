 Tf=[0:0.01:1];
 Ta=0.8*Tf;
 T0=0.5;
 T1=0.6;
 T=blending(Tf,Ta,T0,T1);
 plot(Tf,Tf,Tf,Ta,Tf,T);
 legend('Tf','Ta','T')