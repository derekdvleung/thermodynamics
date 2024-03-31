# Copyright (C) 2023 Derek Leung <derekdvleung@gmail.com>
# This program is free software: you can redistribute it and/or modify it under the terms 
# of the GNU General Public License version 3 as published by the Free Software Foundation. 
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
# without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
# See the GNU General Public License for more details. You should have received a copy of 
# the GNU General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

# This program calculates the reaction path assemblages involved in the hydrothermal alteration and
# carbonation of ultramafic rocks. First, the MSHC system is plotted, similar to Fig. 10 of Menzel et al. 
# (2018). Next, the NKMASHC and CMSHC systems are overlaid on top of this base diagram using the mosaic()
# function in CHNOSZ. Finally, the saturation of quartz is calculated.

#adds the CHNOSZ library for thermodynamic modelling
library (CHNOSZ)

#resets the workspace
reset()
getwd()

###################################################################
# Set T, P, and ranges for log fCO2 and log aSiO2

T <- 300
#325 for Bohlke (1989); 300 for Kishida and Kerrich (1987)
P <- 2600
#2000 for Bohlke (1989); 2000-3200 for Hagemann & Brown (1996) and references therein

pH <- 6

###################################################################
# pre-define logaK+ and logaNa+ activities for stability of muscovite and albite

#according to Kishida and Kerrich (1987), Na/K ~10
# log aK+/aH+ should be around 4.2 based on Bohlke (1989)
K <- -1.8

# log aNa+/aH+ should be around 5.2 based on Bohlke (1989)
#according to Kishida and Kerrich (1987), Na/K ~10
Na <- -0.8

###################################################################
# display settings - use names = FALSE for a diagram that is labeled post-export for publication
names = TRUE

###################################################################
# import minerals from thermoddem database

# switch to kelvin for T
T.units("K")

# add the imported thermoddem database as a .csv file
dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
add.OBIGT("thermoddem-mineral-data.csv")

#swich temperature back to degrees celsius
T.units("C")

###################################################################
# Main space for calculating activity diagrams

for (x in 1:2){ # calculate two diagrams, one zoomed out (Fig. 2a) and one zoomed in (Fig. 2b)
  
  # zoomed out diagram (2a)
  if (x == 1){
    CO2 <- c(-2,3, 600)
    H4SiO4 <- c(-9,0, 600)
  }
  # inset figure (2b)
  if (x == 2){
    CO2 <- c(1,3, 600)
    H4SiO4 <- c(-3.5,-1, 600)
  }

  # define the minerals in MSHC compositional space  
  Mg.cr <- c("Magnesite(Synth)", "Antigorite", "Lizardite", "Chrysotile", "Talc", "Brucite",  "Cummingtonite")
  
  # define the minerals in NKMASHC compositional space  
  Al.cr <- c("Clinochlore",
             "Amesite", 
             "Hydrotalcite(CO3)","Spinel", 
             "Muscovite(ordered)", 
             "Albite(low)", 
             "Diaspore",
             "Celadonite", 
             "Kaolinite",
             "Pyrophyllite",
             "Corundum(alpha)",
             "Corundum(gamma)",
             "Phlogopite"
  )
  
  # colours to fill the NKMASHC fields
  fillColors <- c(
    "#48506130",
    "#A9A66960",
    "#FF000010",
    "#FF00FF20",
    "#99FFE330",
    "#FFF5CE60",
    NA,
    NA,  
    NA,
    NA,
    NA,
    NA,
    NA,
    NA,
    NA,
    NA,
    NA
  )
  
  # define the minerals in CMSHC compositional space    
  Ca.cr <- c("Dolomite(ordered)","Tremolite", "Diopside", "Calcite")

  # define the minerals in NKFASHC compositional space (experimental)    
  #Fe.cr <- c("Magnetite", "Siderite", "Greenalite", "Minnesotaite", "Fe(OH)2")
  #"Hematite"
  
  # define the minerals in CMASHC compositional space (experimental)   
  #CaAl.cr <- c("Clinozoisite", "Edenite(alpha)", "Pyroxene(CaAl)")
  

  
  #sets the compositional space to NCKMASHC
  #Mg is based on conservation of Mg in magnesite
  #Al is based on conservation of Al in muscovite
  #Ca is based on conservation of Ca in dolomite
  
  basis (c("Magnesite(Synth)", "H4SiO4", "H2O", "CO2", "O2", "Muscovite(ordered)", "H+", "Na+","K+", "Dolomite(ordered)", "Magnetite"))
  basis ("CO2", "gas")
  basis ("Na+", Na)
  basis ("K+", K)
  #basis ("Ca+2", Ca)
  basis ("pH", pH)
  #basis ("O2", -37)
  
  ###########################
  # plot the species in MSHC and overlay NKMASHC over the base MSHC  
  
  species (c(Al.cr))
  
  # correct for the activity of spinel because pure spinel is metastable with respect to diaspore
  # (Mg0.52Fe2+0.44Mn0.03Zn0.01)Σ1.00(Cr1.13Al0.79Fe3+0.06V0.01)Σ1.99O4; Menzel et al. (2018)
  species ("Spinel", -1.09)
  
  mAl <- mosaic (Mg.cr, "CO2" = CO2, "H4SiO4" = H4SiO4, T = T, P = P)
  diagram(mAl$A.bases, add = FALSE, col = "#b4b4b4", names = names, col.names = "#b4b4b4", lty =1, italic = TRUE, lwd = 2, fill = NA)
  dAl <- diagram(mAl$A.species, add = TRUE, lwd = 3, fill = fillColors, names = names)
  
  ###########################
  # overlay CMSHC over MSHC      
  species (c(Ca.cr))
  mCa <- mosaic (Mg.cr, "CO2" = CO2, "H4SiO4" = H4SiO4, T = T, P = P)
  dCa <- diagram(mCa$A.species, add = TRUE, col = "#aa0000", names = names, col.names = "#aa0000", lwd = 2)
  
  
  #species (c(CaAl.cr))
  #mCaAl <- mosaic (Mg.cr, "CO2" = CO2, "H4SiO4" = H4SiO4, T = T, P = P)
  #dCaAl <- diagram(mCaAl$A.species, add = TRUE, col = 2, col.names = 2, bold = TRUE)
  #a11 <- mix (dCa, dAl, dCaAl, c(1,1))
  
  #species (c(Fe.cr))
  #species ("Siderite", -0.7)
  
  #mFe <- affinity ("CO2" = CO2, "H4SiO4" = H4SiO4, T = T, P = P)
  #dFe <- diagram (mFe, add = TRUE)
  
  
  ###########################
  # Plot quartz saturation line
  species ("Quartz(alpha)")
  a <- affinity ("CO2" = c(-10,5), "H4SiO4" = c(-15,4), T = T, P = P)
  diagram(a, type = "saturation", add = TRUE, lty = 1, names = names, col = "#4472C4", lwd = 2, fill = NA)
  
  # Add legend and title
  
  TP <- describe.property(c("T", "P", "pH"), c(T, P, pH))
  SP <- describe.basis(c(8,9))
  legend1 <- lex(TP, SP)
  if (x == 1){
    #title(main = syslab(system = c("Na2O","CaO","K2O","MgO", "Al2O3", "SiO2", "H2O","CO2"), dash="-"), )
    legend("bottomright", legend1, bg = "white", cex = 1.0,y.intersp = 1.5, text.width = 0.7)
  }
  
}
