# This script builds relationships for vascular structure.

library('smatr')

conduits <- read.csv('VascularStructure.csv', sep = ',', head = T)

apple <- conduits[conduits$species=='gold',]
cherry <- conduits[conduits$species=='sour',]

apple_taper <- sma(log10(apple$hdiam.um)~log10(apple$diam.cm)) #expected 1/3
cherry_taper <- sma(log10(cherry$hdiam.um)~log10(cherry$diam.cm))

apple_packing <- sma(log10(apple$dens.um)~log10(apple$hdiam.um)) #expected -2
cherry_packing <- sma(log10(cherry$dens.um)~log10(cherry$hdiam.um))