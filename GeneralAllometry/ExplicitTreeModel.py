import numpy as np
import math as m
import csv
import matplotlib.pyplot as p
import mayavi.mlab as my
    
data = np.genfromtxt('TreeReconstruction.csv', delimiter = ',', 
                         names=True, missing_values='',
                         dtype=['S5','i8','i8','i8','i8','i8','i8','f8','f8','i8','f8','S5','i8'])

#if ind == 15:
#    branches = data[0:265]
#    twigs = data[266:]
#else:
#    for i in [3,4,5,13,14,15]:
#        tree = data[(data['tree']==i)]

spp = data[(data['species']=='apple')] # Choose Species
tree = spp[(spp['tree']==15)]   #Choose Individual
branches = tree[(tree['parent_dist']==0)]
        
    ###Tree Model
xyzs = [[[0,0],[0,(branches['length_cm'][0])],[0,0],(branches['diameter_mm'][0])]] #origin and trunk distance
orient = -1

for branch in branches[1:]:
    x_start = xyzs[(branch['parent']-1)][0][1]
    y_start = xyzs[(branch['parent']-1)][1][1]
    z_start = xyzs[(branch['parent']-1)][2][1]
    if branch['bearing'] < 180:
        x_end = x_start - m.cos(m.radians(branch['declination']))*branch['length_cm']
    else:
        x_end = x_start + m.cos(m.radians(branch['declination']))*branch['length_cm']
    if branch['declination'] == 90:
        y_end = y_start + branch['length_cm']
        z_end = z_start
    else:
        y_end = y_start + m.sin(m.radians(branch['declination']))*branch['length_cm']
        z_end = z_start + m.cos(m.radians(branch['bearing']))*branch['length_cm']
    if branch['bearing']== orient:
        xyzs.append([[x_start,x_end],[y_start,y_end],[z_start,z_end],(branch['diameter_mm']*10)])
    else:    
        xyzs.append([[x_start,x_end],[y_start,y_end],[z_start,z_end],branch['diameter_mm']])
        
for xyz in xyzs:
    my.plot3d(xyz[0],xyz[1], xyz[2],tube_radius= xyz[3]/10)

#Edit Mayavi Scene. Drag to size. Toggle axis indicator. View along the +Z axis. 
#Rotate along x-z plane so x indicator shadows z indicator. Configure the scene. Light 2 & 3 maroon.
    
     ###EXTRA
for xyz in xyzs:
    p.plot(xyz[0],xyz[1], 'k-', lw = xyz[3]/10)

for xyz in xyzs:
    p.plot(xyz[0],xyz[2], 'k-', lw = xyz[3]/10)

output_file = open('TreeReconstructionXYZ.csv', 'w')
datawriter = csv.writer(output_file)
datawriter.writerows(xyzs)
output_file.close()


     ###Data clean-up
#generate declination angle from opp_length
for branch in branches:
    if branch['declination'] >= 0:
        branch['declination'] = branch['declination']
    else:
        if branch['opp_length_cm'] > branch['length_cm']:
            daughters = data[data['parent']==branch['branch']]
            add_daughter = daughters[daughters['angle'] == 0]
            branch['declination'] = round(m.degrees(m.cos(branch['opp_length_cm']/
                                                          (branch['length_cm'] + add_daughter['length_cm']))))
        else:
            if branch['opp_length_cm'] == branch['length_cm']:
                branch['declination'] = 0
            elif branch['opp_length_cm'] > 0:
                branch['declination'] = round(m.degrees(m.cos(branch['opp_length_cm']/branch['length_cm'])))
            elif branch['opp_length_cm'] < 0:
                branch['declination'] = -1*round(m.degrees(m.cos(branch['opp_length_cm']/branch['length_cm'])))
            else:
                branch['declination'] = 90
    
        
        

output_file = open('TreeReconstructionTemp.csv', 'w')
datawriter = csv.writer(output_file)
datawriter.writerows(branches)
output_file.close()



#find extending twigs (should be empty with cleaned data)
ext_twigs = []
for branch in branches:
    if branch['opp_length_cm']:
        if branch['opp_length_cm'] > branch['length_cm']:
            twig_angle = []
            for twig in twigs:
                if twig['angle'] == 0:
                    twig_angle.append(twig['branch']) 
            if not twig_angle:
                ext_twigs.append([branch['branch'],branch['length_cm'],(branch['opp_length_cm']-branch['length_cm'])])
        