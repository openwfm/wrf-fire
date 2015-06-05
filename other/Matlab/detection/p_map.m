function p=p_map(T,tign)
% likelihood of detection before cutoff from time of ignition
% usage: p=rad_map(T,time of ignition - time of detection)
% T = time constant (in seconds) for the likelihood decay
p = exp(tign./T);
p(tign>0)=0;
end