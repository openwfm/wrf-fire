function savefile(file)
s=sprintf('%s.save.%i',file,round(now*1e9));
command=['cp ',file,' ',s];
disp(command)
if system(command),
    error('command failed')
end
end
    
