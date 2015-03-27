function s=hdf2struct(f)
% function s=hdf2struct(f)
% read HDF4 file into a structure
i=hdfinfo(f);
s.file=f;
s.info=i;
for k=1:length(i.Attributes)
    field=strrep(i.Attributes(k).Name,' ','_');
    s.attr.(field)=i.Attributes(k).Value;
end
for k=1:length(i.SDS)
    name=i.SDS(k).Name;
    field=strrep(name,' ','_');
    try
        s.data.(field)=hdfread(f,name);
    catch
        s.data.(field)=[];
    end
end