
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

setwd("F:/Mes_Docs/Souss/Floods/Data")


################################################################################
#Modeling with transformation of categorical variables
################################################################################
# trainDat = read.xlsx("Train_Data.xlsx")
# testDat = read.xlsx("Test_Data.xlsx")
# 
# #########
# data_train = trainDat
# data_train[,c("Aspect","Flow_Direction","Geology","Landuse","Soil_type")] = 
#   lapply(data_train[,c("Aspect","Flow_Direction","Geology","Landuse","Soil_type")],as.numeric)
# ###########
# 
# # Aspect setting
# 
# AspectTr<-cut(data_train$Aspect, seq(0,9,1), right=FALSE, labels=c("a0","a1","a2","a3","a4","a5","a6","a7","a8"))
# table(AspectTr) 
# class(AspectTr) 
# 
# 
# flags = data.frame(Reduce(cbind,lapply(levels(AspectTr),function(x){(AspectTr == x)*1})
# ))
# names(flags) = levels(AspectTr)
# data_train = cbind(data_train, flags) 
# 
# # Remove the original Aspect 
# data_train <- data_train[,-c(10,15)] 
# 
# 
# # Flow_Direction setting
# 
# Flow_DirectionTr<-cut(data_train$Flow_Direction, seq(0,9,1), right=FALSE, labels=c("fd0","fd1","fd2","fd3","fd4","fd5","fd6","fd7","fd8"))
# table(Flow_DirectionTr) 
# class(Flow_DirectionTr) 
# 
# 
# flags = data.frame(Reduce(cbind,lapply(levels(Flow_DirectionTr),function(x){(Flow_DirectionTr == x)*1})
# ))
# names(flags) = levels(Flow_DirectionTr)
# data_train = cbind(data_train, flags) 
# 
# # Remove the original Flow_Direction 
# data_train <- data_train[,-c(10,22)] 
# 
# 
# # Geology setting
# 
# GeologyTr<-cut(data_train$Geology, seq(0,8,1), right=FALSE, labels=c("g0","g1","g2","g3","g4","g5","g6","g7"))
# table(GeologyTr) 
# class(GeologyTr) 
# 
# 
# flags = data.frame(Reduce(cbind,lapply(levels(GeologyTr),function(x){(GeologyTr == x)*1})
# ))
# names(flags) = levels(GeologyTr)
# data_train = cbind(data_train, flags)
# 
# # Remove the original Geology 
# data_train <- data_train[,-c(10,29)]
# 
# 
# 
# # Landuse setting
# 
# LanduseTr<-cut(data_train$Landuse, seq(0,6,1), right=FALSE, labels=c("l0","l1","l2","l3","l4","l5"))
# table(LanduseTr) 
# class(LanduseTr) 
# 
# 
# flags = data.frame(Reduce(cbind,lapply(levels(LanduseTr),function(x){(LanduseTr == x)*1})
# ))
# names(flags) = levels(LanduseTr)
# data_train = cbind(data_train, flags) 
# 
# # Remove the original Landuse 
# data_train <- data_train[,-c(10,35)] 
# 
# 
# # Soil_type setting
# 
# Soil_typeTr<-cut(data_train$Soil_type, seq(0,7,1), right=FALSE, labels=c("st0","st1","st2","st3","st4","st5","st6"))
# table(Soil_typeTr) 
# class(Soil_typeTr) 
# 
# 
# flags = data.frame(Reduce(cbind,lapply(levels(Soil_typeTr),function(x){(Soil_typeTr == x)*1})
# ))
# names(flags) = levels(Soil_typeTr)
# data_train = cbind(data_train, flags)
# 
# # Remove the original Soil_type 
# data_train <- data_train[,-c(10,39)]
# 
# 
# # Dealing with testing data
# data_test = testDat
# data_test[,c("Aspect","Flow_Direction","Geology","Landuse","Soil_type")] = 
#   lapply(data_test[,c("Aspect","Flow_Direction","Geology","Landuse","Soil_type")],as.numeric)
# 
# # Aspect setting
# 
# AspectTr<-cut(data_test$Aspect, seq(0,9,1), right=FALSE, labels=c("a0","a1","a2","a3","a4","a5","a6","a7","a8"))
# table(AspectTr) 
# class(AspectTr) 
# 
# 
# flags = data.frame(Reduce(cbind,lapply(levels(AspectTr),function(x){(AspectTr == x)*1})
# ))
# names(flags) = levels(AspectTr)
# data_test = cbind(data_test, flags) 
# 
# # Remove the original Aspect 
# data_test <- data_test[,-c(10,15)] 
# 
# 
# # Flow_Direction setting
# 
# Flow_DirectionTr<-cut(data_test$Flow_Direction, seq(0,9,1), right=FALSE, labels=c("fd0","fd1","fd2","fd3","fd4","fd5","fd6","fd7","fd8"))
# table(Flow_DirectionTr) 
# class(Flow_DirectionTr) 
# 
# 
# flags = data.frame(Reduce(cbind,lapply(levels(Flow_DirectionTr),function(x){(Flow_DirectionTr == x)*1})
# ))
# names(flags) = levels(Flow_DirectionTr)
# data_test = cbind(data_test, flags) 
# 
# # Remove the original Flow_Direction 
# data_test <- data_test[,-c(10,22)] 
# 
# 
# # Geology setting
# 
# GeologyTr<-cut(data_test$Geology, seq(0,8,1), right=FALSE, labels=c("g0","g1","g2","g3","g4","g5","g6","g7"))
# table(GeologyTr) 
# class(GeologyTr) 
# 
# 
# flags = data.frame(Reduce(cbind,lapply(levels(GeologyTr),function(x){(GeologyTr == x)*1})
# ))
# names(flags) = levels(GeologyTr)
# data_test = cbind(data_test, flags)
# 
# # Remove the original Geology 
# data_test <- data_test[,-c(10,29)]
# 
# 
# 
# # Landuse setting
# 
# LanduseTr<-cut(data_test$Landuse, seq(0,6,1), right=FALSE, labels=c("l0","l1","l2","l3","l4","l5"))
# table(LanduseTr) 
# class(LanduseTr) 
# 
# 
# flags = data.frame(Reduce(cbind,lapply(levels(LanduseTr),function(x){(LanduseTr == x)*1})
# ))
# names(flags) = levels(LanduseTr)
# data_test = cbind(data_test, flags) 
# 
# # Remove the original Landuse 
# data_test <- data_test[,-c(10,35)] 
# 
# 
# # Soil_type setting
# 
# Soil_typeTr<-cut(data_test$Soil_type, seq(0,7,1), right=FALSE, labels=c("st0","st1","st2","st3","st4","st5","st6"))
# table(Soil_typeTr) 
# class(Soil_typeTr) 
# 
# 
# flags = data.frame(Reduce(cbind,lapply(levels(Soil_typeTr),function(x){(Soil_typeTr == x)*1})
# ))
# names(flags) = levels(Soil_typeTr)
# data_test = cbind(data_test, flags)
# 
# # Remove the original Soil_type 
# data_test <- data_test[,-c(10,39)]
# 
# 
# # Create one file contain all data
# scaled_t = data_train
# scaled_tst = data_test
# 
# All_incidents <- merge(scaled_tst, scaled_t, all=TRUE) #Full outer join: To keep all rows from both data frames, specify all=TRUE.  https://www.dummies.com/programming/r/how-to-use-the-merge-function-with-data-sets-in-r/
# str(All_incidents)
# All_incidents$Floods = as.factor(All_incidents$Floods)
# 
# names(scaled_t)[1] = "Training"
# names(scaled_tst)[1] = "Testing"
# 
# write.xlsx(scaled_t,"Train_Data_tr.xlsx")
# write.xlsx(scaled_tst,"Test_Data_tr.xlsx")

trainDat = read.xlsx("Train_Data_tr.xlsx")
testDat = read.xlsx("Test_Data_tr.xlsx")

trainDat$Training = as.factor(trainDat$Training)

# Modeling ---------------------------------------------------------

# Train rf model

# Define the control
trControl <- trainControl(method='repeatedcv', 
                          repeats=3,
                          number = 10,
                          search = "grid")

set.seed(1)
rf_random <- train(Training~., 
                   data=trainDat,
                   method = 'rf',
                   metric = 'Accuracy',
                   trControl = control,
                   importance = TRUE)

save(rf_random, file = "fit_rf_tr.RData")


# Final model

names(trainDat)[1] = "Floods"
names(testDat)[1] = "Floods"

All_incidents <- merge(trainDat, testDat, all=TRUE) #Full outer join: To keep all rows from both data frames, specify all=TRUE.  https://www.dummies.com/programming/r/how-to-use-the-merge-function-with-data-sets-in-r/

names(All_incidents)[1] = "Training"

All_incidents$Training = as.factor(All_incidents$Training)


set.seed(849)
fit.rfAll<- train(Training~., 
                  data=All_incidents,
                  method = "rf",
                  metric = "Accuracy",
                  trControl = control,
                  importance = TRUE)

save(fit.rfAll, file = "fit_rfAll_tr.Rdata")


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


# Transform categorical data 

Rasters.df_N_scaled[,c("Aspect","Flow_Direction","Geology","Landuse","Soil_type")] = 
  lapply(Rasters.df_N_scaled[,c("Aspect","Flow_Direction","Geology","Landuse","Soil_type")],as.numeric)

# Aspect setting

AspectTr<-cut(Rasters.df_N_scaled$Aspect, seq(0,9,1), right=FALSE, labels=c("a0","a1","a2","a3","a4","a5","a6","a7","a8"))
table(AspectTr) 
class(AspectTr) 


flags = data.frame(Reduce(cbind,lapply(levels(AspectTr),function(x){(AspectTr == x)*1})
))
names(flags) = levels(AspectTr)
Rasters.df_N_scaled = cbind(Rasters.df_N_scaled, flags) 

# Remove the original Aspect 
Rasters.df_N_scaled <- Rasters.df_N_scaled[,-c(11,16)] 


# Flow_Direction setting

Flow_DirectionTr<-cut(Rasters.df_N_scaled$Flow_Direction, seq(0,9,1), right=FALSE, labels=c("fd0","fd1","fd2","fd3","fd4","fd5","fd6","fd7","fd8"))
table(Flow_DirectionTr) 
class(Flow_DirectionTr) 


flags = data.frame(Reduce(cbind,lapply(levels(Flow_DirectionTr),function(x){(Flow_DirectionTr == x)*1})
))
names(flags) = levels(Flow_DirectionTr)
Rasters.df_N_scaled = cbind(Rasters.df_N_scaled, flags) 

# Remove the original Flow_Direction 
Rasters.df_N_scaled <- Rasters.df_N_scaled[,-c(11,23)] 


# Geology setting

GeologyTr<-cut(Rasters.df_N_scaled$Geology, seq(0,8,1), right=FALSE, labels=c("g0","g1","g2","g3","g4","g5","g6","g7"))
table(GeologyTr) 
class(GeologyTr) 


flags = data.frame(Reduce(cbind,lapply(levels(GeologyTr),function(x){(GeologyTr == x)*1})
))
names(flags) = levels(GeologyTr)
Rasters.df_N_scaled = cbind(Rasters.df_N_scaled, flags)

# Remove the original Geology 
Rasters.df_N_scaled <- Rasters.df_N_scaled[,-c(11,30)]



# Landuse setting

LanduseTr<-cut(Rasters.df_N_scaled$Landuse, seq(0,6,1), right=FALSE, labels=c("l0","l1","l2","l3","l4","l5"))
table(LanduseTr) 
class(LanduseTr) 


flags = data.frame(Reduce(cbind,lapply(levels(LanduseTr),function(x){(LanduseTr == x)*1})
))
names(flags) = levels(LanduseTr)
Rasters.df_N_scaled = cbind(Rasters.df_N_scaled, flags) 

# Remove the original Landuse 
Rasters.df_N_scaled <- Rasters.df_N_scaled[,-c(11,36)] 


# Soil_type setting

Soil_typeTr<-cut(Rasters.df_N_scaled$Soil_type, seq(0,7,1), right=FALSE, labels=c("st0","st1","st2","st3","st4","st5","st6"))
table(Soil_typeTr) 
class(Soil_typeTr) 


flags = data.frame(Reduce(cbind,lapply(levels(Soil_typeTr),function(x){(Soil_typeTr == x)*1})
))
names(flags) = levels(Soil_typeTr)
Rasters.df_N_scaled = cbind(Rasters.df_N_scaled, flags)

# Remove the original Soil_type 
Rasters.df_N_scaled <- Rasters.df_N_scaled[,-c(11,40)]


# Omit na

Rasters.df_N_scaled = Rasters.df_N_scaled[complete.cases(Rasters.df_N_scaled),]

str(Rasters.df_N_scaled)

save(Rasters.df_N_scaled, file = "Rasters.df_N_scaled_TV.RData")

# PRODUCE PROBABILITY MAP

# prediction

p<-as.data.frame(predict(fit.rfAll, Rasters.df_N_scaled[,-c(1,2)], type = "prob"))
summary(p)

Rasters.df$Levels_yes<-p$yes
Rasters.df$Levels_no<-p$no

x<-SpatialPointsDataFrame(as.data.frame(Rasters.df)[, c("x", "y")], data = Rasters.df)
r_ave_yes <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_yes")])

dem = raster("F:/Mes_Docs/Souss/Floods/Data/Rasters2/Dem.tif")

proj4string(r_ave_yes)=CRS(projection(dem))

r_ave_no <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_no")])
proj4string(r_ave_no)=CRS(projection(dem))


writeRaster(r_ave_yes,filename="Prediction_floods_rf_tr.tif", format="GTiff", overwrite=TRUE) 
writeRaster(r_ave_no,filename="Prediction_non_floods_rf_tr.tif", format="GTiff", overwrite=TRUE) 

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

jpeg("Floods_SM_RF_tr.jpg", width = 800, height = 500)
spplot(r_ave_yes, main="Floods Susceptibility Mapping using RF",col.regions=palfunc)
dev.off()

jpeg("Non_Floods_SM_RF_tr.jpg", width = 800, height = 500)
spplot(r_ave_no, main="Non Floods RF",col.regions=palfunc2)
dev.off()




#Run XGBoost function ------------------------------------------------

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


save(fit.xgb_train, file = "fit_xgb_tr.Rdata")


#Train xgbTree model USING aLL dependent data

set.seed(849)
fit.xgbAll<- train(Training~., 
                   data=All_incidents,
                   method = "xgbTree",
                   metric= "Accuracy",
                   preProc = c("center", "scale"), 
                   trControl = myControl,
                   tuneLength = 10,
                   importance = TRUE)

save(fit.xgbAll, file = "fit_xgbAll_tr.Rdata")



p<-as.data.frame(predict(fit.xgbAll, Rasters.df_N_scaled[,-c(1,2)], type = "prob"))
summary(p)

Rasters.df$Levels_yes<-p$yes
Rasters.df$Levels_no<-p$no

x<-SpatialPointsDataFrame(as.data.frame(Rasters.df)[, c("x", "y")], data = Rasters.df)
r_ave_yes <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_yes")])
proj4string(r_ave_yes)=CRS(projection(dem))

r_ave_no <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_no")])
proj4string(r_ave_no)=CRS(projection(dem))


writeRaster(r_ave_yes,filename="Prediction_floods_xgb_tr.tif", format="GTiff", overwrite=TRUE) 
writeRaster(r_ave_no,filename="Prediction_non_floods_xgb_tr.tif", format="GTiff", overwrite=TRUE) 

# Plot Maps

jpeg("Floods_SM_XGB_tr.jpg", width = 800, height = 500)
spplot(r_ave_yes, main="Floods Susceptibility Mapping using XGB",col.regions=palfunc)
dev.off()

jpeg("Non_Floods_SM_XGB_tr.jpg", width = 800, height = 500)
spplot(r_ave_no, main="Non Floods XGB",col.regions=palfunc2)
dev.off()

############################



#Run KNN function ------------------------------------------------

control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3)

set.seed(1)
knn_default = train(Training~., 
                    data=scaled_t,
                    method = "knn",
                    trControl = control)

save(knn_default, file = "fit_knn_tr.Rdata")

#Train KNN model USING aLL dependent data

set.seed(849)
fit.KNNAll<- train(Training~., 
                   data=All_incidents,
                   method = "knn",
                   trControl = control)

save(fit.KNNAll, file = "fit_knnAll_tr.Rdata")



p<-as.data.frame(predict(fit.KNNAll, Rasters.df_N_scaled[,-c(1,2)], type = "prob"))
summary(p)

Rasters.df$Levels_yes<-p$yes
Rasters.df$Levels_no<-p$no

x<-SpatialPointsDataFrame(as.data.frame(Rasters.df)[, c("x", "y")], data = Rasters.df)
r_ave_yes <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_yes")])
proj4string(r_ave_yes)=CRS(projection(dem))

r_ave_no <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_no")])
proj4string(r_ave_no)=CRS(projection(dem))


writeRaster(r_ave_yes,filename="Prediction_floods_knn_tr.tif", format="GTiff", overwrite=TRUE) 
writeRaster(r_ave_no,filename="Prediction_non_floods_knn_tr.tif", format="GTiff", overwrite=TRUE) 

# Plot Maps

jpeg("Floods_SM_KNN_tr.jpg", width = 800, height = 500)
spplot(r_ave_yes, main="Floods Susceptibility Mapping using KNN",col.regions=palfunc)
dev.off()

jpeg("Non_Floods_SM_KNN_tr.jpg", width = 800, height = 500)
spplot(r_ave_no, main="Non Floods KNN",col.regions=palfunc2)
dev.off()


######################

trainDat = read.xlsx("Train_Data_tr.xlsx")
testDat = read.xlsx("Test_Data_tr.xlsx")

names(trainDat)[1] = "Floods"
names(testDat)[1] = "Floods"

trainDat$Floods = as.factor(trainDat$Floods)
testDat$Floods = as.factor(testDat$Floods)

#Run nnet function ------------------------------------------------

control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3)

set.seed(1)
nnet_default = train(Floods~., 
                     data=trainDat,
                     method = "nnet",
                     trControl = control)

save(nnet_default,file = "fit_nnet_tr.RData")

All_incidents <- merge(trainDat, testDat, all=TRUE)
#Train nnet model USING aLL dependent data

set.seed(849)
fit.nnetAll<- train(Floods~., 
                    data=All_incidents,
                    method = "nnet",
                    trControl = control)

save(fit.nnetAll,file = "fit_nnetAll_tr.RData")

Rasters.df_N_scaled = get(load(file = "Rasters.df_N_scaled_TV.RData"))

p<-as.data.frame(predict(fit.nnetAll, Rasters.df_N_scaled[,-c(1,2)], type = "prob"))

Rasters.df$Levels_yes<-p$yes
Rasters.df$Levels_no<-p$no

x<-SpatialPointsDataFrame(as.data.frame(Rasters.df)[, c("x", "y")], data = Rasters.df)
r_ave_yes <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_yes")])
proj4string(r_ave_yes)=CRS(projection(dem))

r_ave_no <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_no")])
proj4string(r_ave_no)=CRS(projection(dem))


writeRaster(r_ave_yes,filename="Prediction_floods_nnet_tr.tif", format="GTiff", overwrite=TRUE) 
writeRaster(r_ave_no,filename="Prediction_non_floods_nnet_tr.tif", format="GTiff", overwrite=TRUE) 

# Plot Maps


jpeg("Floods_SM_nnet_tr.jpg", width = 800, height = 500)
spplot(r_ave_yes, main="Floods Susceptibility Mapping using nnet",col.regions=palfunc)
dev.off()

jpeg("Non_Floods_SM_nnet_tr.jpg", width = 800, height = 500)
spplot(r_ave_no, main="Non Floods nnet",col.regions=palfunc2)
dev.off()
