
setwd("F:/Mes_Docs/WorkingDIR")

.libPaths("F:/Mes_Docs/WorkingDIR/Library")

# Load packages
library(rgdal)        # spatial data processing
library(raster)       # raster processing
library(plyr)         # data manipulation 
library(dplyr)        # data manipulation 
library(RStoolbox)    # Image analysis | plotting spatial data 
library(RColorBrewer) # color
library(ggplot2)      # plotting
library(sp)           # spatial data
library(caret)        # machine learning
library(doParallel)   # Parallel processing
library(e1071)        # Naive Bayes
library(openxlsx)
library(pROC)

# 
# # List of variables
# 
# f = list.files("F:/Mes_Docs/Souss/Floods/Data/Rasters2", pattern = ".tif$", full.names = T)
# 
# #Load variables
# 
# ras = lapply(f,raster)
# 
# # Stack variables
# 
# st = stack(ras)
# 
# 
# # Load flood points
# fp = shapefile("F:/Mes_Docs/Souss/Floods/Data/Shapefile/Flood_points.shp")
# 
# #Extract values to flood points
# 
# # detach(package:tidyr, unload = T)
# #.rs.unloadPackage("tidyr")
# 
# ext = extract(st,fp)
# 
# ext2 = unlist(ext)
# 
# #Convert to dataframe
# 
# ext2 = as.data.frame(ext2)
# 
# # Add ID to flood points
# 
# fp@data$ID <- seq.int(nrow(fp@data))
# 
# # Add ID to extracted data
# library(tidyverse)
# ext2 <- rowid_to_column(ext2, "ID")
# 
# # Merge flood points and extracted data
# 
# extm <- merge(ext2, fp,by.x="ID", by.y="ID")
# 
# 
# # Data partition
# 
# set.seed(100)
# trainids <- createDataPartition(extm$Flood, list=FALSE,p=0.7)
# trainDat <- extm[trainids,]
# testDat <- extm[-trainids,]
# 
# 
# 
# ########################################################
# #Modeling without transformation of categorical variables
# ####################################################
# 
# # Convert categorical variable to factor
# trainDat$Aspect = as.factor(trainDat$Aspect)
# trainDat$Flow_Direction = as.factor(trainDat$Flow_Direction)
# trainDat$Geology  = as.factor(trainDat$Geology )
# trainDat$Landuse = as.factor(trainDat$Landuse)
# trainDat$Soil_type = as.factor(trainDat$Soil_type)
# trainDat$Floods = as.factor(trainDat$Floods)
# 
# testDat$Aspect = as.factor(testDat$Aspect)
# testDat$Flow_Direction = as.factor(testDat$Flow_Direction)
# testDat$Geology  = as.factor(testDat$Geology )
# testDat$Landuse = as.factor(testDat$Landuse)
# testDat$Soil_type = as.factor(testDat$Soil_type)
# testDat$Floods = as.factor(testDat$Floods)
# 
# 
# setwd("F:/Mes_Docs/Souss/Floods/Data")
# 
# trainDat = trainDat[,-c(1,16:17)]
# trainDat$Floods <- ifelse(trainDat$Floods == 1, "yes","no")
# trainDat$Floods <- as.factor(trainDat$Floods)
# 
# testDat = testDat[,-c(1,16:17)]
# testDat$Floods <- ifelse(testDat$Floods == 1, "yes","no")
# testDat$Floods <- as.factor(testDat$Floods)
# 
# 
# maxss <- apply(trainDat[,c(2:6,10:11,13)], 2, max) 
# minss <- apply(trainDat[,c(2:6,10:11,13)], 2, min)
# numvar <- as.data.frame(scale(trainDat[,c(2:6,10:11,13)], center = minss, scale = maxss - minss))
# trainDat <- data.frame(cbind(trainDat[,14], numvar, trainDat[,c(1,7:9,12)]))
# names(trainDat)[1] = "Floods"
# 
# maxss <- apply(testDat[,c(2:6,10:11,13)], 2, max) 
# minss <- apply(testDat[,c(2:6,10:11,13)], 2, min)
# numvar <- as.data.frame(scale(testDat[,c(2:6,10:11,13)], center = minss, scale = maxss - minss))
# testDat<- data.frame(cbind(testDat[,14], numvar, testDat[,c(1,7:9,12)]))
# names(testDat)[1] = "Floods"
# 
# 
# write.xlsx(trainDat,"Train_Data.xlsx")
# write.xlsx(testDat,"Test_Data.xlsx")

##################

trainDat = read.xlsx("Train_Data.xlsx")
testDat = read.xlsx("Test_Data.xlsx")

trainDat[,c("Floods","Aspect","Flow_Direction","Geology","Landuse","Soil_type")] = 
  lapply(trainDat[,c("Floods","Aspect","Flow_Direction","Geology","Landuse","Soil_type")],as.factor)

testDat[,c("Floods","Aspect","Flow_Direction","Geology","Landuse","Soil_type")] = 
  lapply(testDat[,c("Floods","Aspect","Flow_Direction","Geology","Landuse","Soil_type")],as.factor)


# Selected variables: Distance_to_rivers, Soil_type

trainDat = trainDat[,c("Floods","Distance_to_rivers", "Soil_type")]
testDat = testDat[,c("Floods","Distance_to_rivers", "Soil_type")]


# Train rf model

#Random search#####
#Caret can provide for you random parameter if you do not declare for them. 
control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3,
                        search = 'random')    


set.seed(1)
rf_random <- train(Floods~., 
                   data=trainDat,
                   method = 'rf',
                   metric = 'Accuracy',
                   trControl = control,
                   importance = TRUE)

save(rf_random, file = "fit_rf_select.RData")


# Final model

All_incidents <- merge(trainDat, testDat, all=TRUE) #Full outer join: To keep all rows from both data frames, specify all=TRUE.  https://www.dummies.com/programming/r/how-to-use-the-merge-function-with-data-sets-in-r/

set.seed(849)
fit.rfAll<- train(Floods~., 
                  data=All_incidents,
                  method = "rf",
                  metric = "Accuracy",
                  trControl = control,
                  importance = TRUE)

save(fit.rfAll, file = "fit_rfAll_select.Rdata")


#stacked rasters
Rasters=st

# Convert rasters to dataframe with x-y -----------------------

Rasters.df = as.data.frame(Rasters, xy = TRUE, na.rm = TRUE)

# Scale the numeric variables --------------------------------------

maxss <- apply(Rasters.df[,c(2:6,10:11,13)], 2, max) 
minss <- apply(Rasters.df[,c(2:6,10:11,13)], 2, min)
Rasters.df_N_scaled <- as.data.frame(scale(Rasters.df[,c(2:6,10:11,13)], center = minss, scale = maxss - minss)) 

# Now let us add back the (x,y) and to categorical variables
Rasters.df_N_scaled <- data.frame(cbind(Rasters.df[,c(14,15)], Rasters.df_N_scaled, Rasters.df[,c(1,7:9,12)]))


# Omit na

Rasters.df_N_scaled = Rasters.df_N_scaled[complete.cases(Rasters.df_N_scaled),]

# PRODUCE PROBABILITY MAP


unique(Rasters.df_N_scaled$Aspect)
Rasters.df_N_scaled$Aspect[Rasters.df_N_scaled$Aspect==0] = 1
unique(Rasters.df_N_scaled$Flow_Direction)
Rasters.df_N_scaled$Flow_Direction = as.factor(as.integer(Rasters.df_N_scaled$Flow_Direction))
unique(Rasters.df_N_scaled$Geology)
unique(Rasters.df_N_scaled$Landuse)
unique(Rasters.df_N_scaled$Soil_type)
Rasters.df_N_scaled$Soil_type = as.factor(as.integer(Rasters.df_N_scaled$Soil_type))


Rasters.df_N_scaled[,c("Aspect","Flow_Direction","Geology","Landuse","Soil_type")] = 
  lapply(Rasters.df_N_scaled[,c("Aspect","Flow_Direction","Geology","Landuse","Soil_type")],as.factor)

str(Rasters.df_N_scaled)


# prediction

p <-as.data.frame(predict(fit.rfAll, Rasters.df_N_scaled[,-c(1,2)], type = "prob"))

Rasters.df$Levels_yes<-p$yes
Rasters.df$Levels_no<-p$no

x<-SpatialPointsDataFrame(as.data.frame(Rasters.df)[, c("x", "y")], data = Rasters.df)
r_ave_yes <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_yes")])

dem = raster("F:/Mes_Docs/Souss/Floods/Data/Rasters2/Dem.tif")

proj4string(r_ave_yes)=CRS(projection(dem))

r_ave_no <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_no")])
proj4string(r_ave_no)=CRS(projection(dem))


writeRaster(r_ave_yes,filename="Prediction_floods_rf_select.tif", format="GTiff", overwrite=TRUE) 
writeRaster(r_ave_no,filename="Prediction_non_floods_rf_select.tif", format="GTiff", overwrite=TRUE) 

# Plot Maps

library(RColorBrewer)
display.brewer.all()
display.brewer.all(colorblindFriendly = TRUE)

palfunc <- function (n, alpha = 1, begin = 0, end = 1, direction = 1) 
{
  colors <- rev(brewer.pal(11, "RdYlGn"))
  if (direction < 0) colors <- rev(colors)
  colorRampPalette(colors, alpha = alpha)(n)
}

palfunc2 <- function (n, alpha = 1, begin = 0, end = 1, direction = 1) 
{
  colors <- brewer.pal(11, "RdYlGn")
  if (direction < 0) colors <- rev(colors)
  colorRampPalette(colors, alpha = alpha)(n)
}

jpeg("Floods_SM_RF_select.jpg", width = 800, height = 500)
spplot(r_ave_yes, main="Floods Susceptibility Mapping using RF",col.regions=palfunc)
dev.off()

jpeg("Non_Floods_SM_RF_select.jpg", width = 800, height = 500)
spplot(r_ave_no, main="Non Floods RF",col.regions=palfunc2)
dev.off()




#Run XGBoost function ------------------------------------------------

trainDat = read.xlsx("Train_Data.xlsx")
testDat = read.xlsx("Test_Data.xlsx")

trainDat[,c("Floods","Aspect","Flow_Direction","Geology","Landuse","Soil_type")] = 
  lapply(trainDat[,c("Floods","Aspect","Flow_Direction","Geology","Landuse","Soil_type")],as.factor)

testDat[,c("Floods","Aspect","Flow_Direction","Geology","Landuse","Soil_type")] = 
  lapply(testDat[,c("Floods","Aspect","Flow_Direction","Geology","Landuse","Soil_type")],as.factor)


# Selected variables: Dem,Rainfall,Distance_to_rivers (the most important variable)

trainDat = trainDat[,c("Floods","Dem","Rainfall","Distance_to_rivers")]
testDat = testDat[,c("Floods","Dem","Rainfall","Distance_to_rivers")]



control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3,
                        search = 'random') 

set.seed(5)

fit.xgb_train <- train(Floods~., 
                       data=trainDat,
                       method = "xgbTree",
                       metric= "Accuracy",
                       preProc = c("center", "scale"), 
                       trControl = control)


save(fit.xgb_train, file = "fit_xgb_select.Rdata")


All_incidents <- merge(trainDat, testDat, all=TRUE)

#Train xgbTree model USING aLL dependent data

set.seed(849)
fit.xgbAll<- train(Floods~., 
                   data=All_incidents,
                   method = "xgbTree",
                   metric= "Accuracy",
                   preProc = c("center", "scale"), 
                   trControl = myControl,
                   tuneLength = 10,
                   importance = TRUE)

save(fit.xgbAll, file = "fit_xgbAll_select.Rdata")



p<-as.data.frame(predict(fit.xgbAll, Rasters.df_N_scaled[,-c(1,2)], type = "prob"))
summary(p)

Rasters.df$Levels_yes<-p$yes
Rasters.df$Levels_no<-p$no

x<-SpatialPointsDataFrame(as.data.frame(Rasters.df)[, c("x", "y")], data = Rasters.df)
r_ave_yes <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_yes")])
proj4string(r_ave_yes)=CRS(projection(dem))

r_ave_no <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_no")])
proj4string(r_ave_no)=CRS(projection(dem))


writeRaster(r_ave_yes,filename="Prediction_floods_xgb_select.tif", format="GTiff", overwrite=TRUE) 
writeRaster(r_ave_no,filename="Prediction_non_floods_xgb_select.tif", format="GTiff", overwrite=TRUE) 

# Plot Maps


jpeg("Floods_SM_XGB_select.jpg", width = 800, height = 500)
spplot(r_ave_yes, main="Floods Susceptibility Mapping using XGB",col.regions=palfunc)
dev.off()

jpeg("Non_Floods_SM_XGB_select.jpg", width = 800, height = 500)
spplot(r_ave_no, main="Non Floods XGB",col.regions=palfunc2)
dev.off()

############################



#Run KNN function ------------------------------------------------

trainDat = read.xlsx("Train_Data.xlsx")
testDat = read.xlsx("Test_Data.xlsx")

trainDat[,c("Floods","Aspect","Flow_Direction","Geology","Landuse","Soil_type")] = 
  lapply(trainDat[,c("Floods","Aspect","Flow_Direction","Geology","Landuse","Soil_type")],as.factor)

testDat[,c("Floods","Aspect","Flow_Direction","Geology","Landuse","Soil_type")] = 
  lapply(testDat[,c("Floods","Aspect","Flow_Direction","Geology","Landuse","Soil_type")],as.factor)


# Selected variables: "Dem", "Distance_to_rivers","Drainage_density"

trainDat = trainDat[,c("Floods","Dem", "Distance_to_rivers","Drainage_density")]
testDat = testDat[,c("Floods","Dem", "Distance_to_rivers","Drainage_density")]


control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3)

set.seed(1)
knn_default = train(Floods~., 
                    data=trainDat,
                    method = "knn",
                    trControl = control)

save(knn_default, file = "fit_knn_select.Rdata")

All_incidents <- merge(trainDat, testDat, all=TRUE)

#Train KNN model USING aLL dependent data

set.seed(849)
fit.KNNAll<- train(Floods~., 
                   data=All_incidents,
                   method = "knn",
                   trControl = control)

save(fit.KNNAll, file = "fit_knnAll_select.Rdata")


#stacked rasters
Rasters=st

#@Convert rasters to dataframe with x-y -----------------------

Rasters.df = as.data.frame(Rasters, xy = TRUE, na.rm = TRUE)

# Scale the numeric variables --------------------------------------

maxss <- apply(Rasters.df[,c(2:6,10:11,13)], 2, max)
minss <- apply(Rasters.df[,c(2:6,10:11,13)], 2, min)
Rasters.df_N_scaled <- as.data.frame(scale(Rasters.df[,c(2:6,10:11,13)], center = minss, scale = maxss - minss))

# Now let us add back the (x,y) and to categorical variables
Rasters.df_N_scaled <- data.frame(cbind(Rasters.df[,c(14,15)], Rasters.df_N_scaled, Rasters.df[,c(1,7:9,12)]))


# Omit na

Rasters.df_N_scaled = Rasters.df_N_scaled[complete.cases(Rasters.df_N_scaled),]

# PRODUCE PROBABILITY MAP


unique(Rasters.df_N_scaled$Aspect)
Rasters.df_N_scaled$Aspect[Rasters.df_N_scaled$Aspect==0] = 1
unique(Rasters.df_N_scaled$Flow_Direction)
Rasters.df_N_scaled$Flow_Direction = as.factor(as.integer(Rasters.df_N_scaled$Flow_Direction))
unique(Rasters.df_N_scaled$Geology)
unique(Rasters.df_N_scaled$Landuse)
unique(Rasters.df_N_scaled$Soil_type)
Rasters.df_N_scaled$Soil_type = as.factor(as.integer(Rasters.df_N_scaled$Soil_type))


Rasters.df_N_scaled[,c("Aspect","Flow_Direction","Geology","Landuse","Soil_type")] = 
  lapply(Rasters.df_N_scaled[,c("Aspect","Flow_Direction","Geology","Landuse","Soil_type")],as.factor)


save(Rasters.df_N_scaled, file = "Rasters.df_N_scaled_OV.RData")

Rasters.df_N_scaled = get(load(file = "Rasters.df_N_scaled_OV.RData"))

p<-as.data.frame(predict(fit.KNNAll, Rasters.df_N_scaled[,-c(1,2)], type = "prob"))

Rasters.df$Levels_yes<-p$yes
Rasters.df$Levels_no<-p$no

x<-SpatialPointsDataFrame(as.data.frame(Rasters.df)[, c("x", "y")], data = Rasters.df)
r_ave_yes <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_yes")])
proj4string(r_ave_yes)=CRS(projection(dem))

r_ave_no <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_no")])
proj4string(r_ave_no)=CRS(projection(dem))


writeRaster(r_ave_yes,filename="Prediction_floods_knn_select.tif", format="GTiff", overwrite=TRUE) 
writeRaster(r_ave_no,filename="Prediction_non_floods_knn_select.tif", format="GTiff", overwrite=TRUE) 

# Plot Maps


jpeg("Floods_SM_KNN_select.jpg", width = 800, height = 500)
spplot(r_ave_yes, main="Floods Susceptibility Mapping using KNN",col.regions=palfunc)
dev.off()

jpeg("Non_Floods_SM_KNN_select.jpg", width = 800, height = 500)
spplot(r_ave_no, main="Non Floods KNN",col.regions=palfunc2)
dev.off()



#######NNET

trainDat = read.xlsx("Train_Data.xlsx")
testDat = read.xlsx("Test_Data.xlsx")

trainDat[,c("Floods","Aspect","Flow_Direction","Geology","Landuse","Soil_type")] = 
  lapply(trainDat[,c("Floods","Aspect","Flow_Direction","Geology","Landuse","Soil_type")],as.factor)

testDat[,c("Floods","Aspect","Flow_Direction","Geology","Landuse","Soil_type")] = 
  lapply(testDat[,c("Floods","Aspect","Flow_Direction","Geology","Landuse","Soil_type")],as.factor)


# Selected variables: Dem,Distance_to_rivers

trainDat = trainDat[,c("Floods","Dem", "Distance_to_rivers")]
testDat = testDat[,c("Floods","Dem", "Distance_to_rivers")]

# ######################
#Run nnet function ------------------------------------------------

control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3)

set.seed(1)
nnet_default = train(Floods~., 
                     data=trainDat,
                     method = "nnet",
                     trControl = control)

save(nnet_default,file = "fit_nnet_select.RData")


All_incidents <- merge(trainDat, testDat, all=TRUE)

#Train nnet model USING aLL dependent data

set.seed(849)
fit.nnetAll<- train(Floods~., 
                    data=All_incidents,
                    method = "nnet",
                    trControl = control)

save(fit.nnetAll,file = "fit_nnetAll_select.RData")

Rasters.df_N_scaled = read.xlsx("Rasters.df_N_scaled.xlsx")

Rasters.df_N_scaled [,c("Aspect","FlowDirection","Geology","Landuse","Lithofacies")] = 
  lapply(Rasters.df_N_scaled [,c("Aspect","FlowDirection","Geology","Landuse","Lithofacies")],as.factor)


p<-as.data.frame(predict(fit.nnetAll, Rasters.df_N_scaled[,-c(1,2)], type = "prob"))

Rasters.df$Levels_yes<-p$yes
Rasters.df$Levels_no<-p$no

x<-SpatialPointsDataFrame(as.data.frame(Rasters.df)[, c("x", "y")], data = Rasters.df)
r_ave_yes <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_yes")])
proj4string(r_ave_yes)=CRS(projection(dem))

r_ave_no <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_no")])
proj4string(r_ave_no)=CRS(projection(dem))


writeRaster(r_ave_yes,filename="Prediction_floods_nnet_select.tif", format="GTiff", overwrite=TRUE) 
writeRaster(r_ave_no,filename="Prediction_non_floods_nnet_select.tif", format="GTiff", overwrite=TRUE) 

# Plot Maps


jpeg("Floods_SM_nnet_select.jpg", width = 800, height = 500)
spplot(r_ave_yes, main="Floods Susceptibility Mapping using nnet",col.regions=palfunc)
dev.off()

jpeg("Non_Floods_SM_nnet_select.jpg", width = 800, height = 500)
spplot(r_ave_no, main="Non Floods nnet",col.regions=palfunc2)
dev.off()
