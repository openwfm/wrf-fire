function make_rain_input(n)
% create moisture_input.txt for a given number of time steps
% should give 1.723 fotr 14-hour wetting time lag
mm=[14*[0:n]'/n,ones(n+1,1)*[300,1e5,0.01],1e3*[0:n]'/n];
save('moisture_input.txt','mm','-ascii')