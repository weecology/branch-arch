"""This module simulates the range of possible branching architectures as defined by the required inputs of bifurcation angle, rotation angle, and probability of a terminal branching event. The simulation is submitted to Ethan White to satissfy the written comprehensive exam requirement for Ph.D. candidacy at Utah State University. [Developed by: Zack Brym, 15 Nov 2012]"""

import numpy as np
import math as m
import csv
import random as r
import matplotlib as mpl
from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as p

def make_xyzs(start_xyz, length, bifurcation, rotation):
	"""This function returns a list of xyz coordinate pairs generated as a function of radial length, bifurcation angle, and rotation angle. The starting coordinates must be in the general form [[0,0],[0,0],[0,0]]."""
	x_end = round(xyz[0][1] + length*m.sin(m.radians(bifurcation))*
	              m.cos(m.radians(rotation)),3)
	y_end = round(xyz[1][1] + length*m.sin(m.radians(bifurcation))*
	              m.sin(m.radians(rotation)),3)
	z_end = round(xyz[2][1] + length*m.cos(m.radians(bifurcation)),3)
	return [[xyz[0][1],x_end],[xyz[1][1],y_end],[xyz[2][1],z_end]]

def make_all_xyzs(start_xyz, termination_probability, length, bifurcation_set,
                  rotation_angle):
	"""This function is the primary loop of the simulation module. It requires all inputs for make_xyzs as well as the termination probability, and bifurcation parameters (high and low, and constant)."""
	if len(start_xyz) > 10:
		pass
	else:
		term_value = get_termination_value(termination_probability)
		if term_value == 0:
			results = []
			bifurcation = get_bifurcation_angles(bifurcation_set[0],
			                                     bifurcation_set[1],
			                                     bifurcation_set[2])
			rotation = get_rotation_angles(rotation_angle)
			results.append(make_xyzs(start_xyz, length, bifurcation[0],
					         rotation[0]))
			results.append(make_xyzs(start_xyz, length, bifurcation[1],
					         rotation[1]))
			for xyz in results:
				xyzs.append(xyz)	

def get_bifurcation_angles(low_value, high_value, constant=[]):
	"""This function returns two bifurcation angles ordered from with the larger angle first as a list of two numbers. For the simulation to work properly, the low and high values should be within the range [0-90]. A constant set of values can be input by setting bifurcation_set to a list of one or two values."""
	if len(constant) == 2:
		return constant
	if len(constant) == 1:
		one = constant[0]
		two = r.randrange(low_value, high_value)
		if one >= two:
			return [one,two]
		else:
			return [two,one]
	if low_value == high_value:
		return [low_value, low_value]
	else:
		one = r.randrange(low_value, high_value)
		two = r.randrange(low_value, high_value)
		if one >= two:
			return [one,two]
		else:
			return [two,one]

def get_rotation_angles(value=[]):
	"""This function returns two rotation angles ordered with the larger angle first as a list of two numbers. The second angle is exactly opposite or 180 degrees away from the first. For the simulation to work properly, the range of values has been defined as [0-180]. A constant set of values can be input by providing a single positive value."""
	if value:
		one = value
		two = -1*one
		if one <= 90:
			return [one,two]
		else:
			return [two,one]
	else:
		one = r.randrange(0,180)
		two = -1*one
		if one <= 90:
			return [one,two]
		else:
			return [two,one]		

def get_termination_value(termination_probability):
	"""This function returns a binomial response (0 or 1) in a frequency defined by the termination probability with range [0-1]. If the termination_probability is low more branches will be generated and vice versa."""
	value = r.random()
	if value < termination_probability:
		return 1
	else:
		return 0
	
def make_plot(coordinates):
	"""This function contructs a figure from a set of xyz coordinates organized as a list of lists of the form [[xstart, xend],[ystart,yend],[zstart,zend]]. Axes are set to square as per the largest single coordinate value."""
	fig = p.figure()
	ax = fig.gca(projection='3d')
	maximum = [0]
	for xyz in coordinates:
		flat = xyz[0] + xyz[1] + xyz[2] + maximum
		maximum = [max(flat)]
		ax.plot(xyz[0],xyz[1], xyz[2])
	ax.set_xlim3d(-maximum[0],maximum[0])
	ax.set_ylim3d(-maximum[0],maximum[0])
	ax.set_zlim3d(0,maximum[0])
	p.show()	

if __name__ == "__main__":
	
	"""The current parameters set for this simulation are meant to develop an architecture representative of the main structural branches of a free-standing temperate deciduous tree or more specifically a cherry tree. The "branching driven" method is being used to ensure a positive output on the first try. You are welcome to change the parameters and try out the "termination driven" method, as well. Future development will provide loops to vary the existing parameters and generate figures and branching metrics (i.e., canopy volume) so a more rigorous test can be performed to match empirical data. Also, branch length and diameter will be allowed to vary so as to include terminal branching patterns in the analysis. Thanks for taking the time to look over this. I look forward to hearing your thoughts and incorporating an extention of this simulation into my initial data analysis. I hope you have fun playing with the methods and paramters! ~Zack"""
	
	method = "BD" #Choose TD = Termination Driven or BD = Branching Driven
	max_branching = 35 #Maximum loop before abort in TD or number of branching events in BD
	termination_probability = 0.90 #[0-1]
	bifurcation_set = [70,90,[40]] #[min,max,[1 or 2 optional constants]
	rotation_angle = 0 #can provide value in range [0-180] or empty set []
	length = 1
	diameter = 1
	
	xyzs = [[[0, 0], [0, 0], [0, 1]]]
	
	if method == "TD":
		for xyz in xyzs:
			if len(xyzs) < max_branching:
				make_all_xyzs(xyz, termination_probability, length, 
					      bifurcation_set, rotation_angle)
	
		if len(xyzs) >= max_branching:
			print "Ahh! Too much information! Abort! Abort!!!"
	
		elif len(xyzs) == 1:
			print "Nothing doing. Let's try again."
		else:
			print "There were " + str(len(xyzs)) + " branching events!"
			make_plot(xyzs)
		
	if method == "BD":
		while len(xyzs) < max_branching:
			print "Still going: " + str(len(xyzs)) + " coordinates strong..."
			for xyz in xyzs:
				make_all_xyzs(xyz, termination_probability, length, 
					      bifurcation_set, rotation_angle)
			
		make_plot(xyzs)