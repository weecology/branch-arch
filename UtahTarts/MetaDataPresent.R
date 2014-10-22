block <- read.csv('block.csv', sep = ',', header = T)
scaffold <- read.csv('scaffold.csv', sep = ',', header = T)
volume <- read.csv('volume.csv', sep = ',', header = T)
light <- read.csv('light.csv', sep = ',', header = T)
sugars <- read.csv('sugars.csv', sep = ',', header = T)

branches <- scaffold[scaffold$Scaffold > 0,]

library('smatr')

plot(scaffold$Diameter, scaffold$Length)
plot(log10(scaffold$Diameter), log10(scaffold$Length))

diameter_length <- sma(log10(branches$Length)~log10(branches$Diameter))
plot(log10(branches$Diameter), log10(branches$Length))

### METADATA

# Presented are five data tables in the canopy shape and scaling project (2014) 
# The research was conducted in Utah Country in commercial tart cherry (Prunus cerasus Montmorency, P. mahaleb) orchards.
# Five trees were sampled from 6-9 orchards per 5 growers for morphology. 
# Five trees were sampled from a subset of 3 orchards per grower for light and fruit sugars.

## block.csv contains the block-level data with grower identification and sampling information.
# # # Grower - name of the orchard sampled [character]
# # # Block_Name - name of a block sampled given by the grower [character]
# # # Planting_Year - year grower identifies as first planting [integer]
# # # Block_Code - XX## two letters id grower and two numbers id research block no. [character]
# # # Light_Fruit - 'Y' if additional sampling was condcted for light and fruit sugars [Y, blank]


## scaffold.csv contains the branch-level data, limited to scaffolds or first major branch from trunk, for each individual sampled.
# # # Block - block code [character]
# # # Tree - tree id [1 - 5]
# # # Scaffold - scaffold id, trunk is given scaffold = 0 [0 - 5]
# # # Bearing - compass heading of the direction branch travels from the trunk [0 - 355]
# # # Declination - angle from horizontal of branch [0 (horizontal) - 90 (vertical)]
# # # Length - distance from ground to first branch for trunk or longest distance from trunk to terminal twig (i.e. path length) in centimeters [float]
# # # Diameter - branch diameter at basipetal, or "fattest", end in millimeters [float]


## volume.csv contains data for three-dimensional coordinates that mark the outer shell of a canopy.
# For each major cardinal direction three points were measured; top, bottom, and inflection point. 
# Two more points were measured for the bottom and top of the canopy in line with the trunk.
# # # Block - block code [character]
# # # Tree - tree id [1 - 5]
# # # Direction - cardinal direction from trunk [N, E, S, W] or [NE, SE, SW, NW]
# # # Distance_cm - horizontal distance from trunk in centimeters [integer]
# # # Height_cm - vertical distance from ground in centimeters [integer]


## light.csv contains data for light penetration within the canopy. 
# # # block - block code [character]
# # # date - date sampled [MM/DD/YYYY]
# # # tree - tree id [1 - 5]
# # # position - a number corresponding to the position in the canopy. 1 - 8 and 17 is waste height or low canopy.
# # # # 9 - 16 and 18 is arms reach high or mid canopy. 1 and 9 are due north continuing to east 45 degrees (clockwise).
# # # # 17 and 18 are directly over trunk. [1 - 18]
# # # light_sun - light reading for full sun in micromoles of light photons per metersquared seconds 
# # # # using an Apogee Instruments Quantum Meter 300 series with 6 sensors [integer]
# # # light - light reading at canopy position. From outside of canopy, meter was extended into canopy with first sensor just inside [integer]


## sugars.csv contains data for fruit sugar content within the canopy.
# # # Block - block code [character]
# # # Date - date sampled [MM/DD/YYYY]
# # # Tree - tree id [1 - 5]
# # # Position - a number corresponding to the position in the canopy. 1 is due north continuing to east 45 degrees (clockwise).
# # # # 16 is directly over trunk. 16 should be 17 to match light position  [1-8, 16]
# # # Sugar - average sugar content for 5 representative fruit gathered within arms reach of 'Position' in degrees brix,
# # # #  roughly percent surgar content, using a handheld refractometer. [float]
