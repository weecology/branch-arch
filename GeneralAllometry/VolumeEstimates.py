"""The module estimates volume of a tree canopy and produces a polygon image."""

import numpy as np
import math as m
import csv
import matplotlib as mpl
from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as p

def import_data(file_name, data_type):
    data = np.genfromtxt(file_name, delimiter = ',', 
                         names=True, missing_values='NULL',
                         dtype= data_type)
    return data

def make_plot(x,y,z):
	fig = p.figure()
	ax = fig.gca(projection='3d')
	for i in [0,1,2]: 
	    ax.plot(x[i],y[i],z[i])
	#ax.plot_surface(x,y,z,  rstride=4, cstride=4, color='b')
	ax.set_xlim3d(2,6)
	ax.set_ylim3d(-5,5)
	ax.set_zlim3d(0,7)
	p.show()

def output_data(file_out, data_array):
    output_file = open(file_out, 'w')
    datawriter = csv.writer(output_file)
    datawriter.writerows(data_array)
    output_file.close()    
    
if __name__ == '__main__':    
    data = import_data('CanopyVolume.csv', 
                       ['S6','i8','i8','S5','f8','f8','i8'])
    
    locations = [2,4,6]
    volumes = [['species','tree', 'height', 'triangles', 'center_r', 'avg_r', 'avg_volume']]
    radii = []
    
    for s in set(data['species']):
	spp = data[data['species']==s]
	
	for i in set(spp['tree']):
	    tree = spp[spp['tree']==i]
	    
	    xyz = [[],[],[]]
	    coordinates = []
	    radius = []
	    area = []
	    for j in locations:
		location = tree[tree['location']==j]
		high = round(location[location['position']=='mh'][0][4],3)
		low = round(location[location['position']=='ml'][0][4],3)
		if s == 'cherry':
		    north = round(location[location['position']=='nd'][0][4],3)
		    south = round(location[location['position']=='sd'][0][4],3)
		else:
		    north = round(location[location['position']=='ed'][0][4],3)
		    south = round(location[location['position']=='wd'][0][4],3)
		r = (high+low)/2
		if j == 4:
		    center = [j,0,(r+low),r]
		    height = round(location[location['position']=='mh'][0][4],3)
		xyz[0].append([j,j,j,j,j])
		xyz[1].append([0,-south,0,north,0])
		xyz[2].append([low,(r+low),high,(r+low),low])
		coordinates.extend([[j,0,low],[j,-south,(r+low)],[j,0,high],[j,north,(r+low)]])
		area.extend([(high-low)*north/2 + (high-low*south/2)])
		radius.append(r)
	    
	    euclidians = []
	    for coord in coordinates:
		euclidians.extend([m.sqrt((coord[0]-center[0])**2+
		                         (coord[1]-center[1])**2+
		                         (coord[2]-center[2])**2)])
	    volumes.append([s,i, height, 
		            area[0]+2*area[1]+area[2],                               #triangles
		            4*m.pi*center[3]**3/3,                                   #radius of center
		            4*m.pi*(sum(euclidians)/len(euclidians))**3/3])          #average radius of all points-center
	
	#make_plot(xyz[0], xyz[1], xyz[2])
	
    for v in range(1,(len(volumes))):
	volumes[v].extend([sum(volumes[v][3:5])/3])
	    
        
            
    output_data('VolumeEstimates.csv', volumes)