### Modifies Chapter1Figs.R to generate panel for all individuals

### Modifies Chapter1Figs.R to remove path level.

branch_size <- read.csv("BranchSegments.csv", sep = ',', header = T)
library('smatr')
library('dplyr')

branch_graph <- function(x, y, labx, laby, point){
  test <- sma(log10(y)~log10(x))  
  plot(log10(x), log10(y), 
       xlim = c((log10(min(x, na.rm=T))-0.05),(log10(max(x, na.rm=T))+0.05)), 
       ylim = c((log10(min(y, na.rm=T))-0.05),(log10(max(y, na.rm=T))+0.05)),
       xlab = labx, ylab = laby, cex.lab = 1.35, cex.axis = 1.15, cex = 1.3,
       pch = as.numeric(point), lwd = 1.5, bg = "grey")
  segments(log10(min(x, na.rm=T))-0.01, 
           (sma(test)$coef[[1]][2,1]*log10(min(x, na.rm=T)))+sma(test)$coef[[1]][1,1]-0.01, 
           log10(max(x, na.rm=T))+0.01,
           (sma(test)$coef[[1]][2,1]*log10(max(x, na.rm=T)))+sma(test)$coef[[1]][1,1]+0.01,
           lwd = 4, lty = 2, col='grey')
  legend('bottomright', paste("a = ", round(sma(test)$coef[[1]][2,1],3)), bty="n")
}

species <- list(list(c("apple", 2, 0),
                     c(2,7,12,3,5,11,6,8,10,1,4,9,13,17,15,18,20,19,14),
                     c("Apple-1 (Bud.9)", "Apple-2 (Bud.9)", "Apple-3 (Bud.9)", "Apple-4 (Bud.9)", 
                       "Apple-5 (G.41)", "Apple-6 (G.41)","Apple-7 (G.41)", "Apple-8 (G.41)", 
                       "Apple-9 (G.210)", "Apple-10 (G.210)", "Apple-11 (G.210)", "Apple-12 (G.210)",
                       "Apple-13 (M.26)", "Apple-14 (JM.8)", "Apple-15 (JM.8)", "Apple-16 (JM.8)", 
                       "Apple-17 (PiAu.5683)", "Apple-18 (PiAu.5683)", "Apple-19 (PiAu.5683)")),
                list(c("cherry", 24, 22),
                     c(7,13,15,1,10),
                     c("Cherry-1", "Cherry-2", "Cherry-3", "Cherry-4", "Cherry-5")))

pdf(file="FigS4.pdf", width=12, height=18, family="Helvetica", pointsize=18)

for (s in species){
  for (t in 1:length(s[[2]])){ 
    ind <- filter(branch_size, species == s[[1]][1], tree == s[[2]][t])
    par(mfrow = c(5,4))
    
    ###Length  ~ Diameter
    length_zero <- filter(ind, length_cm > 0)
    branch_graph(length_zero$diameter_mm, length_zero$length_cm,
                 "log ( Segment Diameter  )", "log ( Segment Length )", s[[1]][2])
    title(main=s[[3]][t])

    branch_graph(length_zero$diameter_mm, length_zero$tot_length,
                 "log ( Proximal Diameter  )","log ( Subtree Length )", s[[1]][3])
    
    ###Surface Area ~ Volume 
    branch_graph(ind$volume, ind$area,
                 "log ( Segment Volume  )","log ( Segment Area )", s[[1]][2])
    
    branch_graph(ind$tot_volume, ind$tot_area,
                 "log ( Subtree Volume  )","log ( Subtree Area )", s[[1]][3])
    
    ### Diameter ~ Volume
    branch_graph(ind$volume, ind$diameter_mm,
                 "log ( Segment Volume  )","log ( Segment Diameter )", s[[1]][2])
    
    branch_graph(ind$tot_volume, ind$diameter_mm,
                 "log ( Subtree Volume  )","log ( Proximal Diameter )", s[[1]][3])
    
    ### Length ~ Volume
    branch_graph(length_zero$volume, length_zero$length_cm,
                 "log ( Segment Volume  )", "log ( Segment Length )", s[[1]][2])
    
    branch_graph(ind$tot_volume, ind$tot_length,
                 "log ( Subtree Volume  )","log ( Subtree Length )", s[[1]][3])
    
    ### Diameter ~ Surface Area
    branch_graph(ind$area, ind$diameter_mm,
                 "log ( Segment Area  )","log ( Segment Diameter )", s[[1]][2])
    
    branch_graph(ind$tot_area, ind$diameter_mm,
                 "log ( Subtree Area  )","log ( Proximal Diameter )", s[[1]][3])
    
    ### Length ~ Surface Area
    branch_graph(length_zero$area, length_zero$length_cm,
                 "log ( Segment Area  )", "log ( Segment Length )", s[[1]][2])
    
    branch_graph(ind$tot_area, ind$tot_length,
                 "log ( Subtree Area  )","log ( Subtree Length )", s[[1]][3])
    
    ### Length ~ Mass
    mass_zero <- filter(length_zero, stem_m > 0)
    branch_graph(mass_zero$stem_m, mass_zero$length_cm,
                 "log ( Segment Mass  )", "log ( Segment Length )", s[[1]][2])
    
    branch_graph(ind$tot_stem_m, ind$tot_length,
                 "log ( Subtree Mass  )", "log ( Subtree Length )", s[[1]][3])
    
    ### Mass ~ Diameter
    branch_graph(mass_zero$diameter_mm, mass_zero$stem_m,
                 "log ( Segment Diameter  )", "log ( Segment Mass )", s[[1]][2])
    
    branch_graph(ind$diameter_mm, ind$tot_stem_m,
                 "log ( Proximal Diameter  )", "log ( Subtree Mass )", s[[1]][3])
    
    ### Mass ~ Volume (Wood Density) 
    branch_graph(mass_zero$volume, mass_zero$stem_m,
                 "log ( Segment Volume  )","log ( Segment Mass )", s[[1]][2])
    
    branch_graph(ind$tot_volume, ind$tot_stem_m,
                 "log ( Subtree Volume  )","log ( Subtree Mass )", s[[1]][3])
  }
}
dev.off()


