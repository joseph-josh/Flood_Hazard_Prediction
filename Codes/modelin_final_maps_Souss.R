setwd("F:/Mes_Docs/WorkingDIR")

.libPaths("F:/Mes_Docs/WorkingDIR/Library")


# Load packages
library(rgdal)        
library(raster)
library(plyr)         
library(dplyr)        
library(RStoolbox)     
library(RColorBrewer) 
library(sp)           
library(doParallel)   
library(e1071)       
library(pROC)
library(CAST)
library(ggplot2)           
library(caret)        
library(openxlsx)
library(pROC)
library(ROCR)

library(viridis)
library(latticeExtra)
library(gridExtra)
library(grid)
library(mapview)


######################### Rasters
# List of variables

setwd("F:/Mes_Docs/Souss/Floods/Data/Cartes")

f = list.files(, pattern = ".tif$", full.names = T)

# Load variables

ras = lapply(f,raster)

# Stack variables

Rasters = stack(ras)

names(Rasters) = c("KNN","KNN_SE","KNN_TR","KNN_TR_SE",
                   "NNET","NNET_SE","NNET_TR","NNET_TR_SE",
                   "RF" ,"RF_TR","RF_SE","RF_TR_SE",
                   "XGB","XGB_TR","XGB_SE","XGB_TR_SE")

############# Shapefiles

setwd("F:/Mes_Docs/Souss/Floods/Data/Shapefile")

Souss = shapefile("BV_Souss.shp")

RH = shapefile("Reseau.shp")

Villes = shapefile("Villes.shp")


###########################

png("Floods_maps.png",
    width=17,height=22,units="cm",res = 600)
spplot(Rasters,maxpixels=50000,col.regions=palfunc,cuts=80,
       ylab.right=expression ("Flood occurance probability"),
       par.settings = list(layout.widths = list(axis.key.padding = 0,
                                                ylab.right = 2))) + 
  as.layer(spplot(Souss,col="black",col.regions="transparent",lwd=2))

  # as.layer(spplot(RH,col="blue",col.regions="transparent",lwd=2)) + 
  # layer(sp.text(coordinates(Douars), txt = Douars$NOM, pos = 1))

dev.off()
