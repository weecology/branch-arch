"""This module takes spatially explicit branching architecture data in the form of branchID, parent branch id, bearing, declination, length, and diameter. With this data, the module generates a 3D representation of the branching architecture data."""

import numpy as np
import math as m
import csv
import mayavi.mlab as my
    
data = np.genfromtxt('TreeReconstruction.csv', delimiter = ',', 
                         names=True, missing_values='NULL',
                         dtype=['i8','i8','i8','i8','i8','f8','f8','i8','f8'])
    
branches = data[0:265]
twigs = data[266:]

###Tree Model
xyzs = [[[0,0],[0,93],[0,0],180]] #origin and trunk distance
orient = -1

for branch in branches[1:]:
    x_start = xyzs[(branch['parent']-1)][0][1]
    y_start = xyzs[(branch['parent']-1)][1][1]
    z_start = xyzs[(branch['parent']-1)][2][1]
    if branch['bearing'] < 180:
        x_end = x_start - m.cos(m.radians(branch['declination']))*branch['length_cm']
    else:
        x_end = x_start + m.cos(m.radians(branch['declination']))*branch['length_cm']
    y_end = y_start + m.sin(m.radians(branch['declination']))*branch['length_cm']
    z_end = z_start + m.cos(m.radians(branch['bearing']))*branch['length_cm']
    if branch['bearing']== orient:
        xyzs.append([[x_start,x_end],[y_start,y_end],[z_start,z_end],(branch['diameter_mm']*10)])
    else:    
        xyzs.append([[x_start,x_end],[y_start,y_end],[z_start,z_end],branch['diameter_mm']])
        
for xyz in xyzs:
    my.plot3d(xyz[0],xyz[1], xyz[2],tube_radius= xyz[3]/10)
    
my.show()
