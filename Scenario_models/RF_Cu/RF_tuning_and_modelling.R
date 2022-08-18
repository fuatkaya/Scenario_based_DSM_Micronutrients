library(sp)
library(raster)
library(rgdal)
library(ithir)
library(MASS)
library(randomForest)
library(caret)
library(ggplot2)
library(rasterVis)
library(lattice)
library(latticeExtra)
RF_Micro_Scenario_models_dataset <-read.csv(file.choose(),header =T, sep = ",") 
RF_Micro_Scenario_models_dataset <-  as.data.frame(RF_Micro_Scenario_models_dataset)
View(RF_Micro_Scenario_models_dataset)
str(RF_Micro_Scenario_models_dataset)
set.seed(2021)
Training_RF_Micro_scenario <- sample(nrow(RF_Micro_Scenario_models_dataset), 0.7 * nrow(RF_Micro_Scenario_models_dataset))
View(Training_RF_Micro_scenario)
str(Training_RF_Micro_scenario)



###tuning_Cu
ctrl_Micro_rf <- trainControl(method= "repeatedcv", number = 5, repeats = 3, search = "random")

###Landsat
KhavrRF_Micro_modeltune_Cu_RS_Landsat <- train(Cu ~ b2_reflectance + b3_reflectance + b4_reflectance + b5_reflectance + b6_reflectance + b7_reflectance + 
                                                 ci_mean_kh + gndvi_mean_kh + ndvi_mean_kh + satind_mean_kh + sgsi_mean_kh, method ="rf", data = RF_Micro_Scenario_models_dataset[Training_RF_Micro_scenario, ], 
                                               trainControl=ctrl_Micro_rf)

KhavrRF_Micro_modeltune_Cu_RS_Landsat

plot(KhavrRF_Micro_modeltune_Cu_RS_Landsat)
library(randomForest)
KhavrRF_Micro_Cu_RS_Landsat <- randomForest(Cu ~ b2_reflectance + b3_reflectance + b4_reflectance + b5_reflectance + b6_reflectance + b7_reflectance + 
                                              ci_mean_kh + gndvi_mean_kh + ndvi_mean_kh + satind_mean_kh + sgsi_mean_kh, data = RF_Micro_Scenario_models_dataset[Training_RF_Micro_scenario, ], mtry = 2, ntree = 500)


KhavrRF_Micro_Cu_RS_Landsat
plot(KhavrRF_Micro_Cu_RS_Landsat)

par(mfrow = c(2,1))
# Internal validation
KhavrRF_Micro_Cu_RS_Landsat_Internal <- predict(KhavrRF_Micro_Cu_RS_Landsat, newdata = RF_Micro_Scenario_models_dataset[Training_RF_Micro_scenario, ])
KhavrRF_Micro_Cu_RS_Landsat_Internal
goof(observed = RF_Micro_Scenario_models_dataset$Cu[Training_RF_Micro_scenario], predicted = KhavrRF_Micro_Cu_RS_Landsat_Internal ,  plot.it = TRUE)
# externalvalidation
KhavrRF_Micro_Cu_RS_Landsat_External <- predict(KhavrRF_Micro_Cu_RS_Landsat, newdata = RF_Micro_Scenario_models_dataset[-Training_RF_Micro_scenario, ])

goof(observed =  RF_Micro_Scenario_models_dataset$Cu[-Training_RF_Micro_scenario], predicted = KhavrRF_Micro_Cu_RS_Landsat_External ,  plot.it = TRUE)


library(hydroGOF)
library(PerformanceAnalytics)
library(MLmetrics)
#training
nrmse(KhavrRF_Micro_Cu_RS_Landsat_Internal, RF_Micro_Scenario_models_dataset$Cu[Training_RF_Micro_scenario])
MAPE(KhavrRF_Micro_Cu_RS_Landsat_Internal, RF_Micro_Scenario_models_dataset$Cu[Training_RF_Micro_scenario])

##testing
nrmse(KhavrRF_Micro_Cu_RS_Landsat_External, RF_Micro_Scenario_models_dataset$Cu[-Training_RF_Micro_scenario])
MAPE(KhavrRF_Micro_Cu_RS_Landsat_External, RF_Micro_Scenario_models_dataset$Cu[-Training_RF_Micro_scenario])

