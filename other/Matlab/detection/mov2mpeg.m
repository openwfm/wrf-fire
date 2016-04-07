function mov2mpeg(M,file,fps)
% mov2mpeg(M,file)
% mov2mpeg(M,file,fps)
fprintf('Writing %i frames as movie file %s\n',length(M),file)
v=VideoWriter(file,'MPEG-4');
if ~exist('fps','var')
    fps=8
end
v.FrameRate=fps;
v.Quality=100;M
v
open(v);
axis off
set(gca,'nextplot','replacechildren');
for iframe=1:length(M),
    writeVideo(v,M(iframe));
end
close(v)