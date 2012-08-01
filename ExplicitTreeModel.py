import numpy as np
import matplotlib.pyplot as p
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

for branch in branches[1:100]:
    x_start = xyzs[(branch['parent']-1)][0][1]
    y_start = xyzs[(branch['parent']-1)][1][1]
    z_start = xyzs[(branch['parent']-1)][2][1]
    if branch['bearing'] < 180:
        x_end = x_start + m.cos(m.radians(branch['declination']))*branch['length_cm']
    else:
        x_end = x_start - m.cos(m.radians(branch['declination']))*branch['length_cm']
    y_end = y_start + m.sin(m.radians(branch['declination']))*branch['length_cm']
    z_end = z_start + m.cos(m.radians(branch['bearing']))*branch['length_cm']
    xyzs.append([[x_start,x_end],[y_start,y_end],[z_start,z_end],branch['diameter_mm']]) 
    
output_file = open('TreeReconstructionXYZ.csv', 'w')
datawriter = csv.writer(output_file)
datawriter.writerows(xyzs)
output_file.close()
    
for xyz in xyzs:
        p.plot(xyz[0],xyz[1], 'k-', lw = xyz[3]/10)

for xyz in xyzs:
    p.plot(xyz[0],xyz[2], 'k-', lw = xyz[3]/10)

for xyz in xyzs:
    my.plot3d(xyz[0],xyz[1], xyz[2],tube_radius= xyz[3]/10)

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



#find extending twigs (should be empty with cleaned data
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
        