"""This module simulates the range of possible branching architectures as defined by the required inputs of bifurcation angle, rotation angle, and probability of a terminal branching event. The simulation is submitted to Ethan White to satissfy the written comprehensive exam requirement. [Developed by: Zack Brym, 15 Nov 2012]"""

import numpy as np
import math as m
import csv
import mayavi.mlab as my

if __name__ == "__main__":
	termination_probability = 0.5 #range [0-1]
	bifurcation_one = 45 #range [0-90]
	bifurcation_two = 45 #range [0-90]
	rotation_angle = 45 #range [0-180]
	length = 1
	termination_value = 0
	diameter = 1

	xyzs = [[[0,0],[0,0],[0,0],1]] #origin and trunk distance

	#for xyz in xyzs:
	if termination_value == 0:
		x_start = xyzs[0][0][1]
		y_start = xyzs[0][1][1]
		z_start = xyzs[0][2][1]
		x_end = length*m.sin(m.radians(bifurcation_one))*m.cos(m.radians(rotation_angle))
		y_end = length*m.sin(m.radians(bifurcation_one))*m.sin(m.radians(rotation_angle))
		z_end = length*m.cos(m.radians(bifurcation_one))
		xyzs.append([[x_start,x_end],[y_start,y_end],[z_start,z_end],diameter])
        
for xyz in xyzs:
    my.plot3d(xyz[0],xyz[1], xyz[2],tube_radius= xyz[3]/10)
    
my.show()
