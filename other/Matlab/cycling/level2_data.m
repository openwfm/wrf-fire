stash = '/home/jhaley/JPSSdata';


Longitude = hdfread('/home/jhaley/JPSSdata/MOD03.A2013222.0545.006.2013222112442.hdf', 'MODIS_Swath_Type_GEO', 'Fields', 'Longitude');
Latitude = hdfread('/home/jhaley/JPSSdata/MOD03.A2013222.0545.006.2013222112442.hdf', 'MODIS_Swath_Type_GEO', 'Fields', 'Latitude');
fire_mask = hdfread('/home/jhaley/JPSSdata/MOD14.A2013222.0545.006.2015263221706.hdf', '/fire mask', 'Index', {[1  1],[1  1],[2030  1354]});