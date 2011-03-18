% readwrfout

t=char(ncread(file,'Times')');
fxlong =ncread(file,'FXLONG');
fxlat  =ncread(file,'FXLAT');
xlong  =ncread(file,'XLONG');
xlat   =ncread(file,'XLAT');
zsf    =ncread(file,'ZSF');
fgrnhfx=ncread(file,'FGRNHFX');
uf     =ncread(file,'UF');
vf     =ncread(file,'VF');
u      =ncread(file,'U');
v      =ncread(file,'V');
w      =ncread(file,'W');
ph     =ncread(file,'PH');
phb    =ncread(file,'PHB');
hgt    =ncread(file,'HGT');
fuel_frac=ncread(file,'FUEL_FRAC');
