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
SVR_Micro_Scenario_models_dataset <-read.csv(file.choose(),header =T, sep = ",") 
SVR_Micro_Scenario_models_dataset <-  as.data.frame(SVR_Micro_Scenario_models_dataset)
View(SVR_Micro_Scenario_models_dataset)
str(SVR_Micro_Scenario_models_dataset)
set.seed(2021)
Training_SVR_Micro_scenario <- sample(nrow(SVR_Micro_Scenario_models_dataset), 0.7 * nrow(SVR_Micro_Scenario_models_dataset))
View(Training_SVR_Micro_scenario)
str(Training_SVR_Micro_scenario)

############################SVR#################################SVR########################################################

###tuning_Cu
ctrl_Micro_svr <- trainControl(method= "repeatedcv", number = 5, repeats = 3, search = "random") ###ctrl=control
library(e1071)
###Landsat
KhavrSVR_Micro_modeltune_Cu_RS_Landsat <- tune(svm, Cu ~ b2_reflectance + b3_reflectance + b4_reflectance + b5_reflectance + b6_reflectance + 
                                                 b7_reflectance + ci_mean_kh + gndvi_mean_kh + ndvi_mean_kh + satind_mean_kh + sgsi_mean_kh, data = SVR_Micro_Scenario_models_dataset, 
                                               kernel = "radial", 
                                               ranges = list(gamma= 2^(-2:2), cost = 2^(-4:10)),
                                               tune.control=tune.control(cross = 5))



KhavrSVR_Micro_modeltune_Cu_RS_Landsat


KhavrSVR_Micro_Cu_RS_Landsat <- svm(Cu ~ b2_reflectance + b3_reflectance + b4_reflectance + b5_reflectance + b6_reflectance + b7_reflectance + 
                                      ci_mean_kh + gndvi_mean_kh + ndvi_mean_kh + satind_mean_kh + sgsi_mean_kh, 
                                    data = SVR_Micro_Scenario_models_dataset[Training_SVR_Micro_scenario, ], kernel="radial", cost=0.5, gamma=4)

KhavrSVR_Micro_Cu_RS_Landsat


par(mfrow = c(2,1))
# Internal validation
KhavrSVR_Micro_Cu_RS_Landsat_Internal <- predict(KhavrSVR_Micro_Cu_RS_Landsat, newdata = SVR_Micro_Scenario_models_dataset[Training_SVR_Micro_scenario, ])
KhavrSVR_Micro_Cu_RS_Landsat_Internal
goof(observed = SVR_Micro_Scenario_models_dataset$Cu[Training_SVR_Micro_scenario], predicted = KhavrSVR_Micro_Cu_RS_Landsat_Internal ,  plot.it = TRUE)
# externalvalidation
KhavrSVR_Micro_Cu_RS_Landsat_External <- predict(KhavrSVR_Micro_Cu_RS_Landsat, newdata = SVR_Micro_Scenario_models_dataset[-Training_SVR_Micro_scenario, ])

goof(observed =  SVR_Micro_Scenario_models_dataset$Cu[-Training_SVR_Micro_scenario], predicted = KhavrSVR_Micro_Cu_RS_Landsat_External ,  plot.it = TRUE)


library(hydroGOF)
library(PerformanceAnalytics)
library(MLmetrics)
#training
nrmse(KhavrSVR_Micro_Cu_RS_Landsat_Internal, SVR_Micro_Scenario_models_dataset$Cu[Training_SVR_Micro_scenario])
MAPE(KhavrSVR_Micro_Cu_RS_Landsat_Internal, SVR_Micro_Scenario_models_dataset$Cu[Training_SVR_Micro_scenario])

##testing
nrmse(KhavrSVR_Micro_Cu_RS_Landsat_External, SVR_Micro_Scenario_models_dataset$Cu[-Training_SVR_Micro_scenario])
MAPE(KhavrSVR_Micro_Cu_RS_Landsat_External, SVR_Micro_Scenario_models_dataset$Cu[-Training_SVR_Micro_scenario])













