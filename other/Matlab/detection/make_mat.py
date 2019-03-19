import saveload as sl
import numpy as np
import matplotlib.pyplot as plt
from scipy.interpolate import griddata
from scipy import spatial
import scipy.io as sio
print('Loading data...')
data,fxlon,fxlat,time_num=sl.load('data')
prefixes={'MOD': 'MOD14', 'MYD': 'MYD14', 'VNP': 'NPP_VAF_L2'}

# constants for geotransform
res = 0.01   # resolution
rot = 0.0    # rotation (not currently supported)

# radius for the tree
radius = 0.05

for gran in list(data):
	splitted=gran.split('_')
	prefix=splitted[0]
	date=splitted[1][3:8]+splitted[2]+'00'
	file_name = prefixes[prefix]+'.'+date+'.tif.mat'
	#print(name)
	#G=data[gran]

	bounds = [data[gran].lon.min(),data[gran].lon.max(),data[gran].lat.min(),data[gran].lat.max()]
	lons_interp = np.arange(bounds[0],bounds[1],res)
	lats_interp = np.arange(bounds[2],bounds[3],res)
	lons_interp,lats_interp = np.meshgrid(lons_interp,lats_interp)

	geotransform = [bounds[0],res,rot,bounds[3],rot,res]
	#print(geotransform)

	# flatten to 1d the arrays
	lons = np.reshape(data[gran].lon,np.prod(data[gran].lon.shape))
	lats = np.reshape(data[gran].lat,np.prod(data[gran].lat.shape))
	fires = np.reshape(data[gran].fire,np.prod(data[gran].fire.shape)).astype(np.int8)

	# making tree
	
	tree = spatial.cKDTree(np.column_stack((lons,lats)))
	glons = np.reshape(lons_interp,np.prod(lons_interp.shape))
	glats = np.reshape(lats_interp,np.prod(lats_interp.shape))
	indexes = np.array(tree.query_ball_point(np.column_stack((glons,glats)),radius))
	filtered_indexes = np.array([index[0] if len(index) > 0 else np.nan for index in indexes])
	fire1d = [fires[int(ii)] if not np.isnan(ii) else 0 for ii in filtered_indexes]
	fires_interp = np.reshape(fire1d,lons_interp.shape)

	#coordinates = np.array((lons,lats)).T
	#print(coordinates.shape)
	#print(coordinates)
	#print(lons_interp.shape)
	'''
	print(lons_interp)
	print(lats_interp.shape)
	print(lats_interp)
	'''
	#fires_interp = griddata(coordinates,fires,(lons_interp,lats_interp),method='nearest',fill_value = np.nan)
	print('Done interpolating, now plotting')

	print('Nans in fire: ',np.isnan(fires_interp).sum())

	plot = False
	if plot: 
		#number of elements in arrays to plot
		nums = 100
		nums1 = 50
		fig1, (ax1,ax2) = plt.subplots(nrows = 2, ncols =1)
		ax1.pcolor(lons_interp[0::nums1],lats_interp[0::nums1],fires_interp[0::nums1])
		#plt.colorbar()
		ax2.scatter(lons[0::nums],lats[0::nums],c=fires[0::nums],edgecolors='face')
		plt.show()
	result = {'data': fires_interp.astype(np.int8),'geotransform':geotransform}
	sio.savemat(file_name,mdict = result)

	print('File saved as ',file_name)

	

	
	
		
