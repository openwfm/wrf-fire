function mov2avi(M,file)
fprintf('Writing %i frames as movie file %s\n',length(M),file)
v=VideoWriter(file,'MPEG-4');
v.FrameRate=8
open(v);
axis off
set(gca,'nextplot','replacechildren');
for iframe=1:length(M),
    writeVideo(v,M(iframe));
end
close(v)