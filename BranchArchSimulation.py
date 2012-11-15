"""This module simulates the range of possible branching architectures as defined by the required inputs of bifurcation angle, rotation angle, and probability of a terminal branching event. The simulation is submitted to Ethan White to satissfy the written comprehensive exam requirement. [Developed by: Zack Brym, 15 Nov 2012]"""

import numpy as np
import math as m
import csv
import random as r
import matplotlib as mpl
from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as p

def make_xyzs(start_xyz, length, bifurcation, rotation):
	"""This function returns a list of xyz coordinate pairs generated as a function of radial length, bifurcation angle, and rotation angle. The staring coordinates must be in the general form [[0,0],[0,0],[0,0]]."""
	x_end = round(xyz[0][1] + length*m.sin(m.radians(bifurcation))*
	              m.cos(m.radians(rotation)),3)
	y_end = round(xyz[1][1] + length*m.sin(m.radians(bifurcation))*
	              m.sin(m.radians(rotation)),3)
	z_end = round(xyz[2][1] + length*m.cos(m.radians(bifurcation)),3)
	return [[xyz[0][1],x_end],[xyz[1][1],y_end],[xyz[2][1],z_end]]

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

def get_rotation_angles():
	"""This function returns two rotation angles ordered from with the larger angle first as a list of two numbers. The second angle is exactly opposite or 180 degrees away from the first. For the simulation to work properly, the range of values has been defined as [0-180]"""
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

if __name__ == "__main__":
	xyzs = [[[0, 0], [0, 0], [0, 1]]]
	
	termination_probability = 0.6
	bifurcation_set = []
	bifurcation_low = 10
	bifurcation_high = 20
	length = 1
	diameter = 1

	for xyz in xyzs:
		term_value = get_termination_value(termination_probability)
		if term_value == 0:
			bifurcation = get_bifurcation_angles(
			                 bifurcation_low, bifurcation_high,
			                 constant=bifurcation_set)
			rotation = get_rotation_angles()
			xyzs.append(make_xyzs(xyz, length,bifurcation[0],
			                      rotation[0]))
			xyzs.append(make_xyzs(xyz, length,bifurcation[1],
			                      rotation[1]))
		
	fig = p.figure()
	ax = fig.gca(projection='3d')	
	for xyz in xyzs:
		ax.plot(xyz[0],xyz[1], xyz[2])
	p.show()
