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

def output_data(file_out, data_array):
    output_file = open(file_out, 'w')
    datawriter = csv.writer(output_file)
    datawriter.writerows(data_array)
    output_file.close()    
    
if __name__ == '__main__':    
    data = import_data('CanopyVolume.csv', 
                       ['i8','i8','S5','f8','f8','i8'])
    
    output_data('CanopyEstimates.csv', [])