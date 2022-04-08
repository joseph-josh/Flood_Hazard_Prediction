
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


#Dealing with Landuse and soil_type variables (all levels are not represented in the extracted data)

# Landuse = raster("F:/Mes_Docs/Souss/Floods/Data/Rasters/Landuse.tif")
# Soil_type = raster("F:/Mes_Docs/Souss/Floods/Data/Rasters/Soil_type.tif")
# 
# # Combining Landuse type 1 with 5
# Landuse[Landuse==5] <- 1
# 
# #Landuse type 6 become 5
# 
# Landuse[Landuse==6] <- 5
# 
# writeRaster(Landuse, "F:/Mes_Docs/Souss/Floods/Data/Rasters/Landuse.tif", overwrite=T)
# 
# # Combine soil type 3 and 5
# Soil_type[Soil_type==5] <- 3
# 
# #Soil type 6 and 7 become 5 and 6 respectively
# 
# Soil_type[Soil_type==6] <- 5
# Soil_type[Soil_type==7] <- 6
# 
# writeRaster(Soil_type, "F:/Mes_Docs/Souss/Floods/Data/Rasters/Soil_type.tif", overwrite=T)

# dem = raster("F:/Mes_Docs/Souss/Floods/Data/Rasters/Dem.tif")
# 
# # Extract aspect from the DEM
# 
# aspect <- terrain(dem, opt = 'aspect', unit = 'degrees')
# 
# #  classification of aspect
# reclass_df <- c(0, 22.5, 1,
#                 22.5, 67.5, 2,
#                 67.5, 112.5, 3,
#                 112.5, 157.5, 4,
#                 157.5, 202.5, 5,
#                 202.5, 247.5, 6,
#                 247.5, 292.5, 7,
#                 292.5, 337.5, 8,
#                 337.5, 360, 1)
# 
# reclass_m <- matrix(reclass_df,
#                     ncol = 3,
#                     byrow = TRUE)
# 
# aspect_rcl = reclassify(aspect,
#                         reclass_m)
# 
# writeRaster(aspect_rcl,"F:/Mes_Docs/Souss/Floods/Data/Rasters/Aspect.tif", overwrite = T)

# List of variables

f = list.files("F:/Mes_Docs/Souss/Floods/Data/Rasters2", pattern = ".tif$", full.names = T)

#Load variables

ras = lapply(f,raster)

# Stack variables

st = stack(ras)


# Load flood points
fp = shapefile("F:/Mes_Docs/Souss/Floods/Data/Shapefile/Flood_points.shp")

#Extract values to flood points

# detach(package:tidyr, unload = T)
#.rs.unloadPackage("tidyr")

ext = extract(st,fp)

ext2 = unlist(ext)

#Convert to dataframe

ext2 = as.data.frame(ext2)

# Add ID to flood points

fp@data$ID <- seq.int(nrow(fp@data))

# Add ID to extracted data
library(tidyverse)
ext2 <- rowid_to_column(ext2, "ID")

# Merge flood points and extracted data

extm <- merge(ext2, fp,by.x="ID", by.y="ID")


# Data partition

set.seed(100)
trainids <- createDataPartition(extm$Flood, list=FALSE,p=0.7)
trainDat <- extm[trainids,]
testDat <- extm[-trainids,]



########################################################
#Modeling without transformation of categorical variables
####################################################

# Convert categorical variable to factor
trainDat$Aspect = as.factor(trainDat$Aspect)
trainDat$Flow_Direction = as.factor(trainDat$Flow_Direction)
trainDat$Geology  = as.factor(trainDat$Geology )
trainDat$Landuse = as.factor(trainDat$Landuse)
trainDat$Soil_type = as.factor(trainDat$Soil_type)
trainDat$Floods = as.factor(trainDat$Floods)

testDat$Aspect = as.factor(testDat$Aspect)
testDat$Flow_Direction = as.factor(testDat$Flow_Direction)
testDat$Geology  = as.factor(testDat$Geology )
testDat$Landuse = as.factor(testDat$Landuse)
testDat$Soil_type = as.factor(testDat$Soil_type)
testDat$Floods = as.factor(testDat$Floods)

# Train rf model

# Define the control
trControl <- trainControl(method='repeatedcv', 
                          repeats=3,
                          number = 10,
                          search = "grid")

#Building the model with the default values.

trainDat = trainDat[,-c(1,16:17)]
trainDat$Floods <- ifelse(trainDat$Floods == 1, "yes","no")
trainDat$Floods <- as.factor(trainDat$Floods)


library(openxlsx)

write.xlsx(trainDat,"Train_Data.xlsx")


# Run the model
set.seed(100)
rf_defaultN <- train(Floods~., 
                     data=trainDat,
                     method = "rf",
                     metric = "Accuracy",
                     trControl = trControl)
# Print the results
print(rf_defaultN)     
plot(rf_defaultN)
rf_defaultN$finalModel

# Step 2) Search best mtry

best_mtry = 2

tuneGrid <- expand.grid(.mtry = best_mtry)


fit_rf_final <- train(Floods~., 
                      data=trainDat,
                      method = "rf",
                      metric = "Accuracy",
                      tuneGrid = tuneGrid,
                      trControl = trControl,
                      importance = TRUE
)

fit_rf_final
print(fit_rf_final)
varImp(fit_rf_final)
plot(varImp(fit_rf_final), main="RF tuned model")


# Step 5) Evaluate the model
testDat = testDat[,-c(1,16:17)]
testDat$Floods <- ifelse(testDat$Floods == 1, "yes","no")
testDat$Floods <- as.factor(testDat$Floods)


write.xlsx(testDat,"Test_Data.xlsx")


p_final<-predict(fit_rf_final, testDat, type = "raw")
confusionMatrix(p_final, as.factor(testDat$Floods))  


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
print(rf_random)
varImp(rf_random)
plot(varImp(rf_random))
plot(rf_random)

# Evaluate the model

#Confusion Matrix - train data

p1<-predict(rf_random, traintDat[,-14], type = "raw")
confusionMatrix(p1, as.factor(traintDa$Floods))  

#Confusion Matrix - test data

p1<-predict(rf_random, testDat[,-14], type = "raw")
confusionMatrix(p1, as.factor(testDat$Floods))  


# Step 3 Plot ROC curves

library(pROC)

# the model is used to predict the test data. However, you should ask for type="prob" here
pred <- as.data.frame(predict(rf_random, testDat, type = "prob"))

##  Since you have probabilities, use them to get the most-likely class.
# predict class and then attach test class
pred$predict <- names(pred)[1:2][apply(pred[,1:2], 1, which.max)]
pred$observed <- as.factor(testDat$Floods)
head(pred)

#  ROC curves

roc.yes <- roc(ifelse(pred$observed=="yes","no-yes","yes"), as.numeric(pred$yes))
roc.no <- roc(ifelse(pred$observed=="no","no-no", "no"), as.numeric(pred$no))

plot(roc.yes, col = "green", main="RF best tune prediction ROC plot using testing data", xlim=c(0.44,0.1))
lines(roc.yes, col = "red")

plot(roc.no, col = "green", main="RF best tune prediction ROC plot using testing data", xlim=c(0.44,0.1))
lines(roc.no, col = "red")

# calculating the values of AUC for ROC curve
results= c("Yes AUC" = roc.yes$auc) #,"No AUC" = roc.no$auc)
print(results)
legend("topleft",c("AUC = 0.95 "),fill=c("red"),inset = (0.42))


# Final model

All_incidents <- merge(trainDat, testDat, all=TRUE) #Full outer join: To keep all rows from both data frames, specify all=TRUE.  https://www.dummies.com/programming/r/how-to-use-the-merge-function-with-data-sets-in-r/
str(All_incidents)
All_incidents <- All_incidents[,c(14,1:13)] # re-order columns

write.xlsx(All_incidents,"All_incidents.xlsx")

trainDat$Floods= NULL  # remove flood column
testDat$Floods=NULL  # remove flood column

set.seed(849)
fit.rfAll<- train(Floods~., 
                  data=All_incidents,
                  method = "rf",
                  metric = "Accuracy",
                  trControl = control,
                  importance = TRUE)

save(fit.rfAll, file = "F:/Mes_Docs/Souss/Floods/Data/fit.rfAll.Rdata")

X.rfAll = varImp(fit.rfAll)
plot(X.rfAll)

# Plot graph
# 1. Open jpeg file
jpeg("Variable_Importance_RF.jpg", width = 800, height = 500)
# 2. Create the plot
plot(X.rfAll,main="Variable Importance RF" )
# 3. Close the file
dev.off()

#stacked rasters
Rasters=st

# Convert rasters to dataframe with x-y -----------------------

Rasters.df = as.data.frame(Rasters, xy = TRUE, na.rm = TRUE)
head(Rasters.df,1)
Rasters.df_N <- Rasters.df[,-c(14,15)] # remove x, y

#######
# library(foreach)
# library(doSNOW)
# library(data.table)
# cl <- makeCluster(14) # using 14 cores
# registerDoSNOW(cl) 
# 
# 
# # library(doParallel)
# # #Find out how many cores are available (if you don't already know)
# # cores<-detectCores()
# # #Create cluster with desired number of cores, leave one open for the machine
# # #core processes
# # cl <- makeCluster(cores[1]-1)
# # #Register cluster
# # registerDoParallel(cl)
# # clusterEvalQ(cl, .libPaths("F:/Mes_Docs/WorkingDIR/Library"))
# 
# st6 = subset(st,1:6)
# 
# layer_list <- 
#   foreach(i = 1:nlayers(st6), .packages = c('raster', 'data.table') ) %dopar% {
#     #convert layer of rasterStack to data.table
#     layer_pts <- as.data.table(rasterToPoints(st6[[i]]))
#     setkey(layer_pts, x, y) # data.table can key on x and y, no synthetic key needed
#     layer_pts
#   }
# 
# tbl_out6 <- Reduce(merge, layer_list) # uses keys from setkey
# 
# 
# st7 = subset(st,7:13)
# layer_list <- 
#   foreach(i = 1:nlayers(st7), .packages = c('raster', 'data.table') ) %dopar% {
#     #convert layer of rasterStack to data.table
#     layer_pts <- as.data.table(rasterToPoints(st7[[i]]))
#     setkey(layer_pts, x, y) # data.table can key on x and y, no synthetic key needed
#     layer_pts
#   }
# 
# tbl_out7 <- Reduce(merge, layer_list) # uses keys from setkey
# 
# # Combine all data
# 
# Rasters.df = cbind(tbl_out6,tbl_out7)
# 
# memory.size(max = 500000)
# Rasters.df_N <- Rasters.df[,-c("x","y")] # remove x, y
# 

# Scale the numeric variables --------------------------------------

maxss <- apply(Rasters.df[,c(2:6,10:11,13)], 2, max) 
minss <- apply(Rasters.df[,c(2:6,10:11,13)], 2, min)
Rasters.df_N_scaled <- as.data.frame(scale(Rasters.df[,c(2:6,10:11,13)], center = minss, scale = maxss - minss)) 

# Now let us add back the (x,y) and to categorical variables
Rasters.df_N_scaled <- data.frame(cbind(Rasters.df[,c(14,15)], Rasters.df_N_scaled, Rasters.df[,c(1,7:9,12)]))

# save(Rasters.df_N_scaled, file = "F:/Mes_Docs/Souss/Floods/Data/Rasters.df_N_scaled.Rdata")
# 
# library(openxlsx)
# write.xlsx(Rasters.df_N_scaled, "F:/Mes_Docs/Souss/Floods/Data/Rasters.df_N_scaled.xlsx")

# Omit na

Rasters.df_N_scaled = Rasters.df_N_scaled[complete.cases(Rasters.df_N_scaled),]

# PRODUCE PROBABILITY MAP


unique(Rasters.df_N_scaled$Aspect)
Rasters.df_N_scaled$Aspect[Rasters.df_N_scaled$Aspect==0] = 1
unique(Rasters.df_N_scaled$Flow_Direction)
Rasters.df_N_scaled$Flow_Direction = as.factor(as.integer(Rasters.df_N_scaled$Flow_Direction))
# Rasters.df_N_scaled$Flow_Direction[Rasters.df_N_scaled$Flow_Direction==9 |
#                                      Rasters.df_N_scaled$Flow_Direction==10 |
#                                      Rasters.df_N_scaled$Flow_Direction==11 |
#                                      Rasters.df_N_scaled$Flow_Direction==12 |
#                                      Rasters.df_N_scaled$Flow_Direction==13 |
#                                      Rasters.df_N_scaled$Flow_Direction==14 |
#                                      Rasters.df_N_scaled$Flow_Direction==15] = 8
# levels(Rasters.df_N_scaled$Flow_Direction) <- c("1", "2", "3", "4", "5", "6", "7", "8")
# library(data.table)
# setattr(Rasters.df_N_scaled$Flow_Direction,"levels",c("1", "2", "3", "4", "5", "6", "7", "8"))

unique(Rasters.df_N_scaled$Geology)
unique(Rasters.df_N_scaled$Landuse)
unique(Rasters.df_N_scaled$Soil_type)
Rasters.df_N_scaled$Soil_type = as.factor(as.integer(Rasters.df_N_scaled$Soil_type))

# library(dplyr)
# 
# exceptions <- c("1", "2", "3", "4", "5", "6")
# 
# Rasters.df_N_scaled = Rasters.df_N_scaled %>%
#   mutate(Soil_type = if_else(!(Soil_type %in% exceptions), 
#                        "6",Soil_type))
# 
# setattr(Rasters.df_N_scaled$Soil_type,"levels",c("1", "2", "3", "4", "5", "6"))


Rasters.df_N_scaled[,c("Aspect","Flow_Direction","Geology","Landuse","Soil_type")] = 
  lapply(Rasters.df_N_scaled[,c("Aspect","Flow_Direction","Geology","Landuse","Soil_type")],as.factor)

str(Rasters.df_N_scaled)

Rasters.df_N_scaled = Rasters.df_N_scaled[,-c(3:4,18:22)]

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


setwd("F:/Mes_Docs/Souss/Floods/Data")

writeRaster(r_ave_yes,filename="Prediction_floods_rf.tif", format="GTiff", overwrite=TRUE) 
writeRaster(r_ave_no,filename="Prediction_non_floods_rf.tif", format="GTiff", overwrite=TRUE) 

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

jpeg("Floods_SM_RF.jpg", width = 800, height = 500)
spplot(r_ave_yes, main="Floods Susceptibility Mapping using RF",col.regions=palfunc)
dev.off()

jpeg("Non_Floods_SM_RF.jpg", width = 800, height = 500)
spplot(r_ave_no, main="Non Floods RF",col.regions=palfunc2)
dev.off()





##################################Other way to make the prediction
#prediction

catvar = fit.rfAll$trainingData[,c("Aspect","Flow_Direction","Geology","Landuse","Soil_type")]
f <- lapply(catvar,levels)
names(f) <- c("Aspect","Flow_Direction","Geology","Landuse","Soil_type")

prediction <- predict(st, fit.rfAll,factors=f)

spplot(prediction, main="Floods Susceptibility Mapping using RF",col.regions=palfunc)

############################





#Run XGBoost function ------------------------------------------------

#Tunning prameters
myControl <- trainControl(method="repeatedcv", 
                          number=10, 
                          repeats=5,
                          returnResamp='all', 
                          allowParallel=TRUE)

tune_grid <- expand.grid(nrounds = c(200),           # the max number of iterations INCREASE THE PROCESSING TIME COST
                          max_depth = c(6),            # depth of a tree EFFECTIVE OPTIMIZATION
                          eta = c(0.05),               # control the learning rate
                          gamma = c(0.1),             # minimum loss reduction required
                          colsample_bytree = c(0.75),  # subsample ratio of columns when constructing each tree
                          min_child_weight = c(1),     # minimum sum of instance weight (hessian) needed in a child 
                          subsample = c(1))         
# Step 5 modeling
set.seed(849)

fit.xgb_train<- train(Floods~., 
                      data=trainDat,
                      method = "xgbTree",
                      metric= "Accuracy",
                      preProc = c("center", "scale"), 
                      trControl = myControl,
                      tuneGrid = tune_grid,
                      tuneLength = 10)
fit.xgb_train$results
fit.xgb_train$resample$Accuracy
X.xgb = varImp(fit.xgb_train)
plot(X.xgb)


#Confusion Matrix - train data

p1<-predict(fit.xgb_train, traintDat[,-14], type = "raw")
confusionMatrix(p1, as.factor(traintDa$Floods))  

#Confusion Matrix - test data

p1<-predict(fit.xgb_train, testDat[,-14], type = "raw")
confusionMatrix(p1, as.factor(testDat$Floods))  



## Plot ROC curves

library(pROC)

# the model is used to predict the test data. However, you should ask for type="prob" here
predictions1 <- as.data.frame(predict(fit.xgb_train, testDat, type = "prob"))

##  Since you have probabilities, use them to get the most-likely class.
# predict class and then attach test class
predictions1$predict <- names(predictions1)[1:2][apply(predictions1[,1:2], 1, which.max)]
predictions1$observed <- as.factor(testDat$Floods)
head(predictions1)

#ROC curve
roc.yes <- roc(ifelse(predictions1$observed=="yes","no-yes","yes"), as.numeric(predictions1$yes))
roc.no <- roc(ifelse(predictions1$observed=="no","no-no", "no"), as.numeric(predictions1$no))

plot(roc.no, col = "green", main="XGBoost best tune prediction ROC plot using testing data", xlim=c(0.44,0.1))
lines(roc.yes, col = "red")


# calculating the values of AUC for ROC curve
results= c("Yes AUC" = roc.yes$auc) #,"No AUC" = roc.no$auc)
print(results)
legend("topleft",c("AUC = 0.96 "),fill=c("red"),inset = (0.42))


#Train xgbTree model USING aLL dependent data
#We will use the train() function from the of caret package with the "method" parameter "xgbTree" wrapped from the XGBoost package.

set.seed(849)
fit.xgbAll<- train(Floods~., 
                   data=All_incidents,
                   method = "xgbTree",
                   metric= "Accuracy",
                   preProc = c("center", "scale"), 
                   trControl = myControl,
                   tuneGrid = tune_grid,
                   tuneLength = 10,
                   importance = TRUE)

X.xgbAll = varImp(fit.xgbAll)
plot(X.xgbAll, main="Variable Importance XGB All tunned")

# Plot graph
# 1. Open jpeg file
jpeg("varImportance_XGB_All.jpg", width = 1000, height = 700)
# 2. Create the plot
plot(X.xgbAll,main="Variable Importance XGB" )
# 3. Close the file
dev.off()


p<-as.data.frame(predict(fit.xgbAll, Rasters.df_N_scaled[,-c(1,2)], type = "prob"))
summary(p)

Rasters.df$Levels_yes<-p$yes
Rasters.df$Levels_no<-p$no

x<-SpatialPointsDataFrame(as.data.frame(Rasters.df)[, c("x", "y")], data = Rasters.df)
r_ave_yes <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_yes")])
proj4string(r_ave_yes)=CRS(projection(dem))

r_ave_no <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_no")])
proj4string(r_ave_no)=CRS(projection(dem))


writeRaster(r_ave_yes,filename="Prediction_floods_xgb.tif", format="GTiff", overwrite=TRUE) 
writeRaster(r_ave_no,filename="Prediction_non_floods_xgb.tif", format="GTiff", overwrite=TRUE) 

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



jpeg("Floods_SM_XGB.jpg", width = 800, height = 500)
spplot(r_ave_yes, main="Floods Susceptibility Mapping using XGB",col.regions=palfunc)
dev.off()

jpeg("Non_Floods_SM_XGB.jpg", width = 800, height = 500)
spplot(r_ave_no, main="Non Floods XGB",col.regions=palfunc2)
dev.off()

############################



#Run KNN function ------------------------------------------------

control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3)


set.seed(1)
knn_grid1 = train(Floods~., 
                  data=trainDat,
                  method = "knn",
                  trControl = control,
                  tuneGrid = expand.grid(k = seq(1, 31, by = 2))
)
plot(knn_grid1, main="KNN with different K values")
plot(varImp(knn_grid1))

# Evaluate the model
p1_knn_grid<-predict(knn_grid1, testDat[,c(-14)], type = "raw")
confusionMatrix(p1_knn_grid, as.factor(testDat$Floods))  

set.seed(1)
knn_default = train(Floods~., 
                    data=trainDat,
                    method = "knn",
                    trControl = control)

knn_default
plot(knn_default)
plot(varImp(knn_default), main="KNN DEFAULT")

#Confusion Matrix - train data

p1<-predict(knn_default, traintDat[,-14], type = "raw")
confusionMatrix(p1, as.factor(traintDa$Floods))  


#Confusion Matrix - test data

p1<-predict(knn_default, testDat[,-14], type = "raw")
confusionMatrix(p1, as.factor(testDat$Floods))  


## Plot ROC curves

library(pROC)

# the model is used to predict the test data. However, you should ask for type="prob" here
predictions1 <- as.data.frame(predict(knn_default, testDat, type = "prob"))

##  Since you have probabilities, use them to get the most-likely class.
# predict class and then attach test class
predictions1$predict <- names(predictions1)[1:2][apply(predictions1[,1:2], 1, which.max)]
predictions1$observed <- as.factor(testDat$Floods)
head(predictions1)

#ROC curve
roc.yes <- roc(ifelse(predictions1$observed=="yes","no-yes","yes"), as.numeric(predictions1$yes))
roc.no <- roc(ifelse(predictions1$observed=="no","no-no", "no"), as.numeric(predictions1$no))

plot(roc.no, col = "green", main="KNN best tune prediction ROC plot using testing data", xlim=c(0.44,0.1))
lines(roc.yes, col = "red")


# calculating the values of AUC for ROC curve
results= c("Yes AUC" = roc.yes$auc) #,"No AUC" = roc.no$auc)
print(results)
legend("topleft",c("AUC = 0.94 "),fill=c("red"),inset = (0.42))


#Train KNN model USING aLL dependent data
#We will use the train() function from the of caret package with the "method" parameter "xgbTree" wrapped from the XGBoost package.

set.seed(849)
fit.KNNAll<- train(Floods~., 
                   data=All_incidents,
                   method = "knn",
                   trControl = control)

plot(varImp(fit.KNNAll))

# Plot graph
# 1. Open jpeg file
jpeg("varImportance_KNNAll.jpg", width = 800, height = 500)
# 2. Create the plot
plot(X.rfAll,main="Variable Importance KNN" )
# 3. Close the file
dev.off()



p<-as.data.frame(predict(fit.KNNAll, Rasters.df_N_scaled[,-c(1,2)], type = "prob"))
summary(p)

Rasters.df$Levels_yes<-p$yes
Rasters.df$Levels_no<-p$no

x<-SpatialPointsDataFrame(as.data.frame(Rasters.df)[, c("x", "y")], data = Rasters.df)
r_ave_yes <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_yes")])
proj4string(r_ave_yes)=CRS(projection(dem))

r_ave_no <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_no")])
proj4string(r_ave_no)=CRS(projection(dem))


writeRaster(r_ave_yes,filename="Prediction_floods_knn.tif", format="GTiff", overwrite=TRUE) 
writeRaster(r_ave_no,filename="Prediction_non_floods_knn.tif", format="GTiff", overwrite=TRUE) 

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


jpeg("Floods_SM_KNN.jpg", width = 800, height = 500)
spplot(r_ave_yes, main="Floods Susceptibility Mapping using KNN",col.regions=palfunc)
dev.off()

jpeg("Non_Floods_SM_KNN.jpg", width = 800, height = 500)
spplot(r_ave_no, main="Non Floods KNN",col.regions=palfunc2)
dev.off()


######################


# Naive Bayes algorithm

myControl <- trainControl(method="repeatedcv", 
                          number=10, 
                          repeats=3)#


#Train Naive Bayes model
#We will use the train() function of the caret package with the "method" parameter "nb" wrapped from the e1071 package.
set.seed(849)
fit.nb_def <- train(Floods~., 
                    data=trainDat,
                    method = "nb",
                    metric= "Accuracy",
                    preProc = c("center", "scale"),
                    trControl = myControl)

fit.nb_def$resample 
X.nb  = varImp(fit.nb_def)
plot(X.nb )


# Plot graph
# 1. Open jpeg file
jpeg("varImportance_NB.jpg", width = 800, height = 500)
# 2. Create the plot
plot(X.nb,main="Variable Importance NB" )
# 3. Close the file
dev.off()

p1<-predict(fit.nb_def, testDat[,c(-14)], type = "raw")
confusionMatrix(p1, as.factor(testDat$Floods))   # using more deep tree, the accuracy linearly increases! 


#Step 2: Tuning parameters: 

tune_gridNaive <- expand.grid(fL= c(0,0.5,1.0) ,             # (Laplace Correction)
                              usekernel= T ,                     #(Distribution Type)
                              adjust= c(0,0.5,1.0)              #(Bandwidth Adjustment)
)

#Train Na?ve Bayes model
#We will use the train() function of the caret package with the "method" parameter "nb" wrapped from the e1071 package.
set.seed(849)
fit.nb <- train(Floods~., 
                data=trainDat,
                method = "nb",
                tuneGrid=tune_gridNaive,
                metric= "Accuracy",
                preProc = c("center", "scale"), 
                trControl = myControl,
                importance = TRUE)
fit.nb$results 
summaryRes=fit.nb$results # nrounds was fixed = 210
head(summaryRes)
summary(summaryRes)
head(summaryRes[order(summaryRes$Accuracy, decreasing = TRUE),],n=6)  # sort max to min for first 5 values based on Accuracy


## using best tunned hyperparameters
tune_gridNaive2 <- expand.grid(fL= c(0) ,             # (Laplace Correction)
                               usekernel= T ,                     #(Distribution Type)
                               adjust= c(1.0)              #(Bandwidth Adjustment)
)

#Train Na?ve Bayes model
#We will use the train() function of the caret package with the "method" parameter "nb" wrapped from the e1071 package.
set.seed(849)
fit.nb2 <- train(Floods~., 
                 data=trainDat,
                 method = "nb",
                 tuneGrid=tune_gridNaive2,
                 metric= "Accuracy",
                 preProc = c("center", "scale"), 
                 trControl = myControl,
                 importance = TRUE)
fit.nb2$results 


#Confusion Matrix - train data

p1<-predict(fit.nb2, traintDat[,-14], type = "raw")
confusionMatrix(p1, as.factor(traintDa$Floods))  


#Confusion Matrix - test data

p1<-predict(fit.nb2, testDat[,-14], type = "raw")
confusionMatrix(p1, as.factor(testDat$Floods))  


## Plot ROC curves

library(pROC)

# the model is used to predict the test data. However, you should ask for type="prob" here
predictions1 <- as.data.frame(predict(fit.nb2, testDat, type = "prob"))

##  Since you have probabilities, use them to get the most-likely class.
# predict class and then attach test class
predictions1$predict <- names(predictions1)[1:2][apply(predictions1[,1:2], 1, which.max)]
predictions1$observed <- as.factor(testDat$Floods)
head(predictions1)

#ROC curve
roc.yes <- roc(ifelse(predictions1$observed=="yes","no-yes","yes"), as.numeric(predictions1$yes))
roc.no <- roc(ifelse(predictions1$observed=="no","no-no", "no"), as.numeric(predictions1$no))

plot(roc.no, col = "green", main="NB best tune prediction ROC plot using testing data", xlim=c(0.44,0.1))
lines(roc.yes, col = "red")


# calculating the values of AUC for ROC curve
results= c("Yes AUC" = roc.yes$auc) #,"No AUC" = roc.no$auc)
print(results)
legend("topleft",c("AUC = 0.97 "),fill=c("red"),inset = (0.42))

#Train NB model USING aLL dependent data
set.seed(849)
fit.nbAll<- train(Floods~., 
                  data=All_incidents,
                  method = "nb",
                  metric= "Accuracy",
                  tuneGrid=tune_gridNaive2,
                  preProc = c("center", "scale"), 
                  trControl = myControl)

fit.nbAll$results
X.nbAll = varImp(fit.nbAll)
plot(X.nbAll, main="Variable Importance All NB tuned")

# Plot graph
# 1. Open jpeg file
jpeg("varImportance_All_NB.jpg", width = 800, height = 500)
# 2. Create the plot
plot(X.nbAll,main="Variable Importance All NB tuned" )
# 3. Close the file
dev.off()



p<-as.data.frame(predict(fit.nbAll, Rasters.df_N_scaled[,-c(1,2)], type = "prob"))
summary(p)

Rasters.df$Levels_yes<-p$yes
Rasters.df$Levels_no<-p$no

x<-SpatialPointsDataFrame(as.data.frame(Rasters.df)[, c("x", "y")], data = Rasters.df)
r_ave_yes <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_yes")])
proj4string(r_ave_yes)=CRS(projection(dem))

r_ave_no <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_no")])
proj4string(r_ave_no)=CRS(projection(dem))


writeRaster(r_ave_yes,filename="Prediction_floods_nb.tif", format="GTiff", overwrite=TRUE) 
writeRaster(r_ave_no,filename="Prediction_non_floods_nb.tif", format="GTiff", overwrite=TRUE) 

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




jpeg("Floods_SM_NB.jpg", width = 800, height = 500)
spplot(r_ave_yes, main="Floods Susceptibility Mapping using NB",col.regions=palfunc)
dev.off()

jpeg("Non_Floods_SM_NB.jpg", width = 800, height = 500)
spplot(r_ave_no, main="Non Floods NB",col.regions=palfunc2)
dev.off()



################################################################################
#Modeling with transformation of categorical variables
################################################################################

# Dealing with training data
# Dealing with Categorical data
#  "Aspect", "Flow_Direction" "Geology" "Landuse" "Soil_type"
unique(trainDat$Aspect)
unique(trainDat$Flow_Direction)
unique(trainDat$Geology)
unique(trainDat$Landuse)
unique(trainDat$Soil_type)

#########
data_train = trainDat
data_train[,c("Aspect","Flow_Direction","Geology","Landuse","Soil_type")] = 
  lapply(data_train[,c("Aspect","Flow_Direction","Geology","Landuse","Soil_type")],as.numeric)
###########

# Aspect setting

AspectTr<-cut(data_train$Aspect, seq(0,9,1), right=FALSE, labels=c("a0","a1","a2","a3","a4","a5","a6","a7","a8"))
table(AspectTr) 
class(AspectTr) 


flags = data.frame(Reduce(cbind,lapply(levels(AspectTr),function(x){(AspectTr == x)*1})
))
names(flags) = levels(AspectTr)
data_train = cbind(data_train, flags) 

# Remove the original Aspect 
data_train <- data_train[,-c(1,15)] 


# Flow_Direction setting

Flow_DirectionTr<-cut(data_train$Flow_Direction, seq(0,9,1), right=FALSE, labels=c("fd0","fd1","fd2","fd3","fd4","fd5","fd6","fd7","fd8"))
table(Flow_DirectionTr) 
class(Flow_DirectionTr) 


flags = data.frame(Reduce(cbind,lapply(levels(Flow_DirectionTr),function(x){(Flow_DirectionTr == x)*1})
))
names(flags) = levels(Flow_DirectionTr)
data_train = cbind(data_train, flags) 

# Remove the original Flow_Direction 
data_train <- data_train[,-c(6,22)] 


# Geology setting

GeologyTr<-cut(data_train$Geology, seq(0,8,1), right=FALSE, labels=c("g0","g1","g2","g3","g4","g5","g6","g7"))
table(GeologyTr) 
class(GeologyTr) 


flags = data.frame(Reduce(cbind,lapply(levels(GeologyTr),function(x){(GeologyTr == x)*1})
))
names(flags) = levels(GeologyTr)
data_train = cbind(data_train, flags)

# Remove the original Geology 
data_train <- data_train[,-c(6,29)]



# Landuse setting

LanduseTr<-cut(data_train$Landuse, seq(0,6,1), right=FALSE, labels=c("l0","l1","l2","l3","l4","l5"))
table(LanduseTr) 
class(LanduseTr) 


flags = data.frame(Reduce(cbind,lapply(levels(LanduseTr),function(x){(LanduseTr == x)*1})
))
names(flags) = levels(LanduseTr)
data_train = cbind(data_train, flags) 

# Remove the original Landuse 
data_train <- data_train[,-c(6,35)] 


# Soil_type setting

Soil_typeTr<-cut(data_train$Soil_type, seq(0,7,1), right=FALSE, labels=c("st0","st1","st2","st3","st4","st5","st6"))
table(Soil_typeTr) 
class(Soil_typeTr) 


flags = data.frame(Reduce(cbind,lapply(levels(Soil_typeTr),function(x){(Soil_typeTr == x)*1})
))
names(flags) = levels(Soil_typeTr)
data_train = cbind(data_train, flags)

# Remove the original Soil_type 
data_train <- data_train[,-c(8,39)]

data_train <- data_train[,c(9,1:8,10:43)] # re-order columns

# Normalization

maxs <- apply(data_train[,-1], 2, max) 
mins <- apply(data_train[,-1], 2, min)
scaled_train <- as.data.frame(scale(data_train[,-1], center = mins, scale = maxs - mins))
scaled_t <-scaled_train
Training = as.data.frame(trainDat[,"Floods"])
names(Training) = "Training"
scaled_t = cbind(Training,scaled_t)
scaled_t$Training <- ifelse(scaled_t$Training == 1, "yes","no")



# Dealing with testing data
data_test = testDat
data_test[,c("Aspect","Flow_Direction","Geology","Landuse","Soil_type")] = 
  lapply(data_test[,c("Aspect","Flow_Direction","Geology","Landuse","Soil_type")],as.numeric)

# Aspect setting

AspectTr<-cut(data_test$Aspect, seq(0,9,1), right=FALSE, labels=c("a0","a1","a2","a3","a4","a5","a6","a7","a8"))
table(AspectTr) 
class(AspectTr) 


flags = data.frame(Reduce(cbind,lapply(levels(AspectTr),function(x){(AspectTr == x)*1})
))
names(flags) = levels(AspectTr)
data_test = cbind(data_test, flags) 

# Remove the original Aspect 
data_test <- data_test[,-c(1,15)] 


# Flow_Direction setting

Flow_DirectionTr<-cut(data_test$Flow_Direction, seq(0,9,1), right=FALSE, labels=c("fd0","fd1","fd2","fd3","fd4","fd5","fd6","fd7","fd8"))
table(Flow_DirectionTr) 
class(Flow_DirectionTr) 


flags = data.frame(Reduce(cbind,lapply(levels(Flow_DirectionTr),function(x){(Flow_DirectionTr == x)*1})
))
names(flags) = levels(Flow_DirectionTr)
data_test = cbind(data_test, flags) 

# Remove the original Flow_Direction 
data_test <- data_test[,-c(6,22)] 


# Geology setting

GeologyTr<-cut(data_test$Geology, seq(0,8,1), right=FALSE, labels=c("g0","g1","g2","g3","g4","g5","g6","g7"))
table(GeologyTr) 
class(GeologyTr) 


flags = data.frame(Reduce(cbind,lapply(levels(GeologyTr),function(x){(GeologyTr == x)*1})
))
names(flags) = levels(GeologyTr)
data_test = cbind(data_test, flags)

# Remove the original Geology 
data_test <- data_test[,-c(6,29)]



# Landuse setting

LanduseTr<-cut(data_test$Landuse, seq(0,6,1), right=FALSE, labels=c("l0","l1","l2","l3","l4","l5"))
table(LanduseTr) 
class(LanduseTr) 


flags = data.frame(Reduce(cbind,lapply(levels(LanduseTr),function(x){(LanduseTr == x)*1})
))
names(flags) = levels(LanduseTr)
data_test = cbind(data_test, flags) 

# Remove the original Landuse 
data_test <- data_test[,-c(6,35)] 


# Soil_type setting

Soil_typeTr<-cut(data_test$Soil_type, seq(0,7,1), right=FALSE, labels=c("st0","st1","st2","st3","st4","st5","st6"))
table(Soil_typeTr) 
class(Soil_typeTr) 


flags = data.frame(Reduce(cbind,lapply(levels(Soil_typeTr),function(x){(Soil_typeTr == x)*1})
))
names(flags) = levels(Soil_typeTr)
data_test = cbind(data_test, flags)

# Remove the original Soil_type 
data_test <- data_test[,-c(8,39)]

data_test <- data_test[,c(9,1:8,10:43)] # re-order columns


# Scale the data
numvar = data_test[,2:9]
maxs <- apply(numvar, 2, max) 
mins <- apply(numvar, 2, min)
scaled_numvar <- as.data.frame(scale(numvar, center = mins, scale = maxs - mins))
catvar = data_test[,10:43]
scaled_test = cbind(scaled_numvar,catvar)
scaled_tst <-scaled_test
Testing = as.data.frame(testDat[,"Floods"])
names(Testing) = "Testing"
scaled_tst = cbind(Testing,scaled_tst)
scaled_tst$Testing <- ifelse(scaled_tst$Testing == 1, "yes","no")


# Create one file contain all data

scaled_tst$Floods=scaled_tst$Testing

scaled_t$Floods=scaled_t$Training

All_incidents <- merge(scaled_tst[,-1], scaled_t[,-1], all=TRUE) #Full outer join: To keep all rows from both data frames, specify all=TRUE.  https://www.dummies.com/programming/r/how-to-use-the-merge-function-with-data-sets-in-r/
All_incidents <- All_incidents[,c(43,1:42)] # re-order columns
str(All_incidents)
All_incidents$Floods = as.factor(All_incidents$Floods)


scaled_tst$Floods= NULL  # remove flood column
scaled_t$Floods=NULL  # remove flood column




# Modeling ---------------------------------------------------------

# Data partition


# Train rf model

# Define the control
trControl <- trainControl(method='repeatedcv', 
                          repeats=3,
                          number = 10,
                          search = "grid")

#Building the model with the default values.


library(openxlsx)

scaled_t$Training = as.factor(scaled_t$Training)
write.xlsx(scaled_t,"Train_Data_tr.xlsx")


# Run the model
set.seed(100)
rf_defaultN <- train(Training~., 
                     data=scaled_t,
                     method = "rf",
                     metric = "Accuracy",
                     trControl = trControl)
# Print the results
print(rf_defaultN)     
plot(rf_defaultN)
rf_defaultN$finalModel

# Step 2) Search best mtry

best_mtry = 2

tuneGrid <- expand.grid(.mtry = best_mtry)


fit_rf_final <- train(Training~., 
                      data=scaled_t,
                      method = "rf",
                      metric = "Accuracy",
                      tuneGrid = tuneGrid,
                      trControl = trControl,
                      importance = TRUE
)

fit_rf_final
print(fit_rf_final)
varImp(fit_rf_final)
plot(varImp(fit_rf_final), main="RF tuned model")


# Step 5) Evaluate the model

scaled_tst$Testing = as.factor(scaled_tst$Testing)

write.xlsx(scaled_tst,"Test_Data_tr.xlsx")


p_final<-predict(fit_rf_final, scaled_tst, type = "raw")
confusionMatrix(p_final, as.factor(scaled_tst$Testing))  


#Random search#####
#Caret can provide for you random parameter if you do not declare for them. 
control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3,
                        search = 'random')    


set.seed(1)
rf_random <- train(Training~., 
                   data=scaled_t,
                   method = 'rf',
                   metric = 'Accuracy',
                   trControl = control,
                   importance = TRUE)
print(rf_random)
varImp(rf_random)
plot(varImp(rf_random))
plot(rf_random)

# Evaluate the model

#Confusion Matrix 

p1<-predict(rf_random, scaled_tst[,-1], type = "raw")
confusionMatrix(p1, as.factor(scaled_tst$Testing))  


# Step 3 Plot ROC curves

library(pROC)

# the model is used to predict the test data. However, you should ask for type="prob" here
pred <- as.data.frame(predict(rf_random, scaled_tst, type = "prob"))

##  Since you have probabilities, use them to get the most-likely class.
# predict class and then attach test class
pred$predict <- names(pred)[1:2][apply(pred[,1:2], 1, which.max)]
pred$observed <- as.factor(scaled_tst$Testing)
head(pred)

#  ROC curves

roc.yes <- roc(ifelse(pred$observed=="yes","no-yes","yes"), as.numeric(pred$yes))
roc.no <- roc(ifelse(pred$observed=="no","no-no", "no"), as.numeric(pred$no))

plot(roc.yes, col = "green", main="RF best tune prediction ROC plot using testing data", xlim=c(0.44,0.1))
lines(roc.yes, col = "red")

# plot(roc.no, col = "green", main="RF best tune prediction ROC plot using testing data", xlim=c(0.44,0.1))
# lines(roc.no, col = "red")

# calculating the values of AUC for ROC curve
results= c("Yes AUC" = roc.yes$auc) #,"No AUC" = roc.no$auc)
print(results)
legend("topleft",c("AUC = 0.97 "),fill=c("red"),inset = (0.42))


# Final model

names(All_incidents)[1] = "Training"

set.seed(849)
fit.rfAll<- train(Training~., 
                  data=All_incidents,
                  method = "rf",
                  metric = "Accuracy",
                  trControl = control,
                  importance = TRUE)

save(fit.rfAll, file = "F:/Mes_Docs/Souss/Training/Data/fit.rfAll_tr.Rdata")

X.rfAll = varImp(fit.rfAll)
plot(X.rfAll)

# Plot graph
# 1. Open jpeg file
jpeg("Variable_Importance_RF_tr.jpg", width = 800, height = 500)
# 2. Create the plot
plot(X.rfAll,main="Variable Importance RF" )
# 3. Close the file
dev.off()


#stacked rasters
Rasters=st

# Convert rasters to dataframe with x-y -----------------------

Rasters.df = as.data.frame(Rasters, xy = TRUE, na.rm = TRUE)
head(Rasters.df,1)
Rasters.df_N <- Rasters.df[,c(1,7:9,12)] # remove x, y


# Scale the numeric variables --------------------------------------

maxss <- apply(Rasters.df[,c(2:6,10:11,13)], 2, max) 
minss <- apply(Rasters.df[,c(2:6,10:11,13)], 2, min)
Rasters.df_N_scaled <- as.data.frame(scale(Rasters.df[,c(2:6,10:11,13)], center = minss, scale = maxss - minss)) 


# Transform categorical data 
# Aspect setting

AspectTr<-cut(Rasters.df_N$Aspect, seq(0,9,1), right=FALSE, labels=c("a0","a1","a2","a3","a4","a5","a6","a7","a8"))
table(AspectTr) 
class(AspectTr) 


flags = data.frame(Reduce(cbind,lapply(levels(AspectTr),function(x){(AspectTr == x)*1})
))
names(flags) = levels(AspectTr)
Rasters.df_N = cbind(Rasters.df_N, flags) 

# Remove the original Aspect 
Rasters.df_N <- Rasters.df_N[,-c(1,6)] 


# Flow_Direction setting

Flow_DirectionTr<-cut(Rasters.df_N$Flow_Direction, seq(0,9,1), right=FALSE, labels=c("fd0","fd1","fd2","fd3","fd4","fd5","fd6","fd7","fd8"))
table(Flow_DirectionTr) 
class(Flow_DirectionTr) 


flags = data.frame(Reduce(cbind,lapply(levels(Flow_DirectionTr),function(x){(Flow_DirectionTr == x)*1})
))
names(flags) = levels(Flow_DirectionTr)
Rasters.df_N = cbind(Rasters.df_N, flags) 

# Remove the original Flow_Direction 
Rasters.df_N <- Rasters.df_N[,-c(1,13)] 


# Geology setting

GeologyTr<-cut(Rasters.df_N$Geology, seq(0,8,1), right=FALSE, labels=c("g0","g1","g2","g3","g4","g5","g6","g7"))
table(GeologyTr) 
class(GeologyTr) 


flags = data.frame(Reduce(cbind,lapply(levels(GeologyTr),function(x){(GeologyTr == x)*1})
))
names(flags) = levels(GeologyTr)
Rasters.df_N = cbind(Rasters.df_N, flags)

# Remove the original Geology 
Rasters.df_N <- Rasters.df_N[,-c(1,20)]



# Landuse setting

LanduseTr<-cut(Rasters.df_N$Landuse, seq(0,6,1), right=FALSE, labels=c("l0","l1","l2","l3","l4","l5"))
table(LanduseTr) 
class(LanduseTr) 


flags = data.frame(Reduce(cbind,lapply(levels(LanduseTr),function(x){(LanduseTr == x)*1})
))
names(flags) = levels(LanduseTr)
Rasters.df_N = cbind(Rasters.df_N, flags) 

# Remove the original Landuse 
Rasters.df_N <- Rasters.df_N[,-c(1,26)] 


# Soil_type setting

Soil_typeTr<-cut(Rasters.df_N$Soil_type, seq(0,7,1), right=FALSE, labels=c("st0","st1","st2","st3","st4","st5","st6"))
table(Soil_typeTr) 
class(Soil_typeTr) 


flags = data.frame(Reduce(cbind,lapply(levels(Soil_typeTr),function(x){(Soil_typeTr == x)*1})
))
names(flags) = levels(Soil_typeTr)
Rasters.df_N = cbind(Rasters.df_N, flags)

# Remove the original Soil_type 
Rasters.df_N <- Rasters.df_N[,-c(1,30)]


# Now let us add back the (x,y) and to categorical variables
Rasters.df_N_scaled <- data.frame(cbind(Rasters.df[,c(14,15)], Rasters.df_N_scaled, Rasters.df_N))


# Omit na

Rasters.df_N_scaled = Rasters.df_N_scaled[complete.cases(Rasters.df_N_scaled),]

str(Rasters.df_N_scaled)

#Rasters.df_N_scaled[,c(11:44)] = lapply(Rasters.df_N_scaled[,c(11:44)], as.numeric)

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


setwd("F:/Mes_Docs/Souss/Floods/Data")

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





##################################Other way to make the prediction
#prediction

catvar = fit.rfAll$trainingData[,c("Aspect","Flow_Direction","Geology","Landuse","Soil_type")]
f <- lapply(catvar,levels)
names(f) <- c("Aspect","Flow_Direction","Geology","Landuse","Soil_type")

prediction <- predict(st, fit.rfAll,factors=f)

spplot(prediction, main="Floods Susceptibility Mapping using RF",col.regions=palfunc)

############################





#Run XGBoost function ------------------------------------------------

#Tunning prameters
myControl <- trainControl(method="repeatedcv", 
                          number=10, 
                          repeats=5,
                          returnResamp='all', 
                          allowParallel=TRUE)

tune_grid <- expand.grid(nrounds = c(200),           # the max number of iterations INCREASE THE PROCESSING TIME COST
                         max_depth = c(6),            # depth of a tree EFFECTIVE OPTIMIZATION
                         eta = c(0.05),               # control the learning rate
                         gamma = c(0.1),             # minimum loss reduction required
                         colsample_bytree = c(0.75),  # subsample ratio of columns when constructing each tree
                         min_child_weight = c(1),     # minimum sum of instance weight (hessian) needed in a child 
                         subsample = c(1))         
# Step 5 modeling
set.seed(849)

fit.xgb_train<- train(Training~., 
                      data=scaled_t,
                      method = "xgbTree",
                      metric= "Accuracy",
                      preProc = c("center", "scale"), 
                      trControl = myControl,
                      tuneGrid = tune_grid,
                      tuneLength = 10)
fit.xgb_train$results
fit.xgb_train$resample$Accuracy
X.xgb = varImp(fit.xgb_train)
plot(X.xgb)


p1<-predict(fit.xgb_train, scaled_tst[,-1], type = "raw")
confusionMatrix(p1, as.factor(scaled_tst$Testing))  


## Plot ROC curves

library(pROC)

# the model is used to predict the test data. However, you should ask for type="prob" here
predictions1 <- as.data.frame(predict(fit.xgb_train, scaled_tst[,-1], type = "prob"))

##  Since you have probabilities, use them to get the most-likely class.
# predict class and then attach test class
predictions1$predict <- names(predictions1)[1:2][apply(predictions1[,1:2], 1, which.max)]
predictions1$observed <- as.factor(testDat$Floods)
head(predictions1)

#ROC curve
roc.yes <- roc(ifelse(predictions1$observed=="yes","no-yes","yes"), as.numeric(predictions1$yes))
roc.no <- roc(ifelse(predictions1$observed=="no","no-no", "no"), as.numeric(predictions1$no))

plot(roc.no, col = "green", main="XGBoost best tune prediction ROC plot using testing data", xlim=c(0.44,0.1))
lines(roc.yes, col = "red")


# calculating the values of AUC for ROC curve
results= c("Yes AUC" = roc.yes$auc) #,"No AUC" = roc.no$auc)
print(results)
legend("topleft",c("AUC = 0.97 "),fill=c("red"),inset = (0.42))


#Train xgbTree model USING aLL dependent data
#We will use the train() function from the of caret package with the "method" parameter "xgbTree" wrapped from the XGBoost package.

set.seed(849)
fit.xgbAll<- train(Training~., 
                   data=All_incidents,
                   method = "xgbTree",
                   metric= "Accuracy",
                   preProc = c("center", "scale"), 
                   trControl = myControl,
                   tuneGrid = tune_grid,
                   tuneLength = 10,
                   importance = TRUE)

X.xgbAll = varImp(fit.xgbAll)
plot(X.xgbAll, main="Variable Importance XGB All tunned")

# Plot graph
# 1. Open jpeg file
jpeg("varImportance_XGB_All_tr.jpg", width = 1000, height = 700)
# 2. Create the plot
plot(X.xgbAll,main="Variable Importance XGB" )
# 3. Close the file
dev.off()


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
knn_grid1 = train(Training~., 
                  data=scaled_t,
                  method = "knn",
                  trControl = control,
                  tuneGrid = expand.grid(k = seq(1, 31, by = 2))
)
plot(knn_grid1, main="KNN with different K values")
plot(varImp(knn_grid1))

# Evaluate the model
p1_knn_grid<-predict(knn_grid1,scaled_tst, type = "raw")
confusionMatrix(p1_knn_grid, as.factor(scaled_tst$Testing))  

set.seed(1)
knn_default = train(Training~., 
                    data=scaled_t,
                    method = "knn",
                    trControl = control)

knn_default
plot(knn_default)
plot(varImp(knn_default), main="KNN DEFAULT")


#Confusion Matrix 

p1<-predict(knn_default, scaled_tst, type = "raw")
confusionMatrix(p1, as.factor(scaled_tst$Testing))  


## Plot ROC curves

library(pROC)

# the model is used to predict the test data. However, you should ask for type="prob" here
predictions1 <- as.data.frame(predict(knn_default, scaled_tst, type = "prob"))

##  Since you have probabilities, use them to get the most-likely class.
# predict class and then attach test class
predictions1$predict <- names(predictions1)[1:2][apply(predictions1[,1:2], 1, which.max)]
predictions1$observed <- as.factor(testDat$Floods)
head(predictions1)

#ROC curve
roc.yes <- roc(ifelse(predictions1$observed=="yes","no-yes","yes"), as.numeric(predictions1$yes))
roc.no <- roc(ifelse(predictions1$observed=="no","no-no", "no"), as.numeric(predictions1$no))

plot(roc.no, col = "green", main="KNN best tune prediction ROC plot using testing data", xlim=c(0.44,0.1))
lines(roc.yes, col = "red")


# calculating the values of AUC for ROC curve
results= c("Yes AUC" = roc.yes$auc) #,"No AUC" = roc.no$auc)
print(results)
legend("topleft",c("AUC = 0.97 "),fill=c("red"),inset = (0.42))


#Train KNN model USING aLL dependent data
#We will use the train() function from the of caret package with the "method" parameter "xgbTree" wrapped from the XGBoost package.

set.seed(849)
fit.KNNAll<- train(Training~., 
                   data=All_incidents,
                   method = "knn",
                   trControl = control)

plot(varImp(fit.KNNAll))

# Plot graph
# 1. Open jpeg file
jpeg("varImportance_KNNAll_tr.jpg", width = 800, height = 500)
# 2. Create the plot
plot(varImp(fit.KNNAll),main="Variable Importance KNN")
# 3. Close the file
dev.off()



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


jpeg("Floods_SM_KNN_tr.jpg", width = 800, height = 500)
spplot(r_ave_yes, main="Floods Susceptibility Mapping using KNN",col.regions=palfunc)
dev.off()

jpeg("Non_Floods_SM_KNN_tr.jpg", width = 800, height = 500)
spplot(r_ave_no, main="Non Floods KNN",col.regions=palfunc2)
dev.off()


######################


# Naive Bayes algorithm

myControl <- trainControl(method="repeatedcv", 
                          number=10, 
                          repeats=3)#


#Train Naive Bayes model
#We will use the train() function of the caret package with the "method" parameter "nb" wrapped from the e1071 package.
set.seed(849)
fit.nb_def <- train(Training~., 
                    data=scaled_t,
                    method = "nb",
                    metric= "Accuracy",
                    preProc = c("center", "scale"),
                    trControl = myControl)

fit.nb_def$resample 
X.nb  = varImp(fit.nb_def)
plot(X.nb )


# Plot graph
# 1. Open jpeg file
jpeg("varImportance_NB_tr.jpg", width = 800, height = 500)
# 2. Create the plot
plot(X.nb,main="Variable Importance NB" )
# 3. Close the file
dev.off()

p1<-predict(fit.nb_def, scaled_tst, type = "raw")
confusionMatrix(p1, as.factor(scaled_tst$Testing))   # using more deep tree, the accuracy linearly increases! 


#Step 2: Tuning parameters: 

tune_gridNaive <- expand.grid(fL= c(0,0.5,1.0) ,             # (Laplace Correction)
                              usekernel= T ,                     #(Distribution Type)
                              adjust= c(0,0.5,1.0)              #(Bandwidth Adjustment)
)

#Train Na?ve Bayes model
#We will use the train() function of the caret package with the "method" parameter "nb" wrapped from the e1071 package.
set.seed(849)
fit.nb <- train(Training~., 
                data=scaled_t,
                method = "nb",
                tuneGrid=tune_gridNaive,
                metric= "Accuracy",
                preProc = c("center", "scale"), 
                trControl = myControl,
                importance = TRUE)
fit.nb$results 
summaryRes=fit.nb$results # nrounds was fixed = 210
head(summaryRes)
summary(summaryRes)
head(summaryRes[order(summaryRes$Accuracy, decreasing = TRUE),],n=6)  # sort max to min for first 5 values based on Accuracy


## using best tunned hyperparameters
tune_gridNaive2 <- expand.grid(fL= c(0) ,             # (Laplace Correction)
                               usekernel= T ,                     #(Distribution Type)
                               adjust= c(1.0)              #(Bandwidth Adjustment)
)

#Train Na?ve Bayes model
#We will use the train() function of the caret package with the "method" parameter "nb" wrapped from the e1071 package.
set.seed(849)
fit.nb2 <- train(Training~., 
                 data=scaled_t,
                 method = "nb",
                 tuneGrid=tune_gridNaive2,
                 metric= "Accuracy",
                 preProc = c("center", "scale"), 
                 trControl = myControl,
                 importance = TRUE)
fit.nb2$results 


#Confusion Matrix - train data

p1<-predict(fit.nb2, scaled_tst, type = "raw")
confusionMatrix(p1, as.factor(scaled_tst$Testing))  


#Confusion Matrix - test data

p1<-predict(fit.nb2, scaled_tst, type = "raw")
confusionMatrix(p1, as.factor(scaled_tst$Testing))  


## Plot ROC curves

library(pROC)

# the model is used to predict the test data. However, you should ask for type="prob" here
predictions1 <- as.data.frame(predict(fit.nb2, scaled_tst, type = "prob"))

##  Since you have probabilities, use them to get the most-likely class.
# predict class and then attach test class
predictions1$predict <- names(predictions1)[1:2][apply(predictions1[,1:2], 1, which.max)]
predictions1$observed <- as.factor(testDat$Floods)
head(predictions1)

#ROC curve
roc.yes <- roc(ifelse(predictions1$observed=="yes","no-yes","yes"), as.numeric(predictions1$yes))
roc.no <- roc(ifelse(predictions1$observed=="no","no-no", "no"), as.numeric(predictions1$no))

plot(roc.no, col = "green", main="NB best tune prediction ROC plot using testing data", xlim=c(0.44,0.1))
lines(roc.yes, col = "red")


# calculating the values of AUC for ROC curve
results= c("Yes AUC" = roc.yes$auc) #,"No AUC" = roc.no$auc)
print(results)
legend("topleft",c("AUC = 0.97 "),fill=c("red"),inset = (0.42))

#Train NB model USING aLL dependent data
set.seed(849)
fit.nbAll<- train(Training~., 
                  data=All_incidents,
                  method = "nb",
                  metric= "Accuracy",
                  tuneGrid=tune_gridNaive2,
                  preProc = c("center", "scale"), 
                  trControl = myControl)

fit.nbAll$results
X.nbAll = varImp(fit.nbAll)
plot(X.nbAll, main="Variable Importance All NB tuned")

# Plot graph
# 1. Open jpeg file
jpeg("varImportance_All_NB_tr.jpg", width = 800, height = 500)
# 2. Create the plot
plot(X.nbAll,main="Variable Importance All NB tuned" )
# 3. Close the file
dev.off()


memory.size(max = 1000000)
p<-as.data.frame(predict(fit.nbAll, Rasters.df_N_scaled[,-c(1,2)], type = "prob"))
summary(p)

Rasters.df$Levels_yes<-p$yes
Rasters.df$Levels_no<-p$no

x<-SpatialPointsDataFrame(as.data.frame(Rasters.df)[, c("x", "y")], data = Rasters.df)
r_ave_yes <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_yes")])
proj4string(r_ave_yes)=CRS(projection(dem))

r_ave_no <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_no")])
proj4string(r_ave_no)=CRS(projection(dem))


writeRaster(r_ave_yes,filename="Prediction_floods_nb_tr.tif", format="GTiff", overwrite=TRUE) 
writeRaster(r_ave_no,filename="Prediction_non_floods_nb_tr.tif", format="GTiff", overwrite=TRUE) 

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




jpeg("Floods_SM_NB_tr.jpg", width = 800, height = 500)
spplot(r_ave_yes, main="Floods Susceptibility Mapping using NB",col.regions=palfunc)
dev.off()

jpeg("Non_Floods_SM_NB_tr.jpg", width = 800, height = 500)
spplot(r_ave_no, main="Non Floods NB",col.regions=palfunc2)
dev.off()


