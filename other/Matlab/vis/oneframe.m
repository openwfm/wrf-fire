function oneframe(k)
    ext=sprintf('_%5.5i.txt',k);
    f=read_array_m(['fgrnhfx',ext]);
    u=read_array_m(['uf',ext]);
    v=read_array_m(['vf',ext]);
    l=read_array_m(['lfn',ext]); 
    sf(f,u,v,l)
    title(sprintf('time %i s',k))
end