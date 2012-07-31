import numpy as np
import matplotlib.pyplot as p
import math as m
import csv

datafile = open('TreeReconstruction.csv', 'r')
datalist = []
for row in datafile:
    datalist.append(row.strip().split(','))
    
data = np.genfromtxt('TreeReconstruction.csv', delimiter = ',', 
                         names=True, missing_values='NULL',
                         dtype=['i8','i8','i8','i8','f8','i8','f8','f8','i8','f8'])
    
branches = data[0:265]
twigs = data[266:]

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

p.plot(test[0],test[1], 'k-')

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
        