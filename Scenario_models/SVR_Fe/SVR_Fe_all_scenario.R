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

###tuning_Fe
ctrl_Micro_svr <- trainControl(method= "repeatedcv", number = 5, repeats = 3, search = "random")
library(e1071)
###Landsat
KhavrSVR_Micro_modeltune_Fe_RS_Landsat <- tune(svm, Fe ~ b2_reflectance + b3_reflectance + b4_reflectance + b5_reflectance + b6_reflectance + 
                                                 b7_reflectance + ci_mean_kh + gndvi_mean_kh + ndvi_mean_kh + satind_mean_kh + sgsi_mean_kh, data = SVR_Micro_Scenario_models_dataset, 
                                               kernel = "radial", 
                                               ranges = list(gamma= 2^(-2:2), cost = 2^(-4:10)),
                                               tune.control=tune.control(cross = 5))



KhavrSVR_Micro_modeltune_Fe_RS_Landsat


KhavrSVR_Micro_Fe_RS_Landsat <- svm(Fe ~ b2_reflectance + b3_reflectance + b4_reflectance + b5_reflectance + b6_reflectance + b7_reflectance + 
                                              ci_mean_kh + gndvi_mean_kh + ndvi_mean_kh + satind_mean_kh + sgsi_mean_kh, 
                                            data = SVR_Micro_Scenario_models_dataset[Training_SVR_Micro_scenario, ], kernel="radial", cost=2, gamma=0.25)

KhavrSVR_Micro_Fe_RS_Landsat


par(mfrow = c(2,1))
# Internal validation
KhavrSVR_Micro_Fe_RS_Landsat_Internal <- predict(KhavrSVR_Micro_Fe_RS_Landsat, newdata = SVR_Micro_Scenario_models_dataset[Training_SVR_Micro_scenario, ])
KhavrSVR_Micro_Fe_RS_Landsat_Internal
goof(observed = SVR_Micro_Scenario_models_dataset$Fe[Training_SVR_Micro_scenario], predicted = KhavrSVR_Micro_Fe_RS_Landsat_Internal ,  plot.it = TRUE)
# externalvalidation
KhavrSVR_Micro_Fe_RS_Landsat_External <- predict(KhavrSVR_Micro_Fe_RS_Landsat, newdata = SVR_Micro_Scenario_models_dataset[-Training_SVR_Micro_scenario, ])

goof(observed =  SVR_Micro_Scenario_models_dataset$Fe[-Training_SVR_Micro_scenario], predicted = KhavrSVR_Micro_Fe_RS_Landsat_External ,  plot.it = TRUE)


library(hydroGOF)
library(PerformanceAnalytics)
library(MLmetrics)
#training
nrmse(KhavrSVR_Micro_Fe_RS_Landsat_Internal, SVR_Micro_Scenario_models_dataset$Fe[Training_SVR_Micro_scenario])
MAPE(KhavrSVR_Micro_Fe_RS_Landsat_Internal, SVR_Micro_Scenario_models_dataset$Fe[Training_SVR_Micro_scenario])

##testing
nrmse(KhavrSVR_Micro_Fe_RS_Landsat_External, SVR_Micro_Scenario_models_dataset$Fe[-Training_SVR_Micro_scenario])
MAPE(KhavrSVR_Micro_Fe_RS_Landsat_External, SVR_Micro_Scenario_models_dataset$Fe[-Training_SVR_Micro_scenario])





##Sentinel
KhavrSVR_Micro_modeltune_Fe_RS_Sentinel <- tune(svm, Fe ~ Sen_B02_30m_aoi + Sen_B03_Mean_30m_aoi + Sen_B04_Mean_30m_aoi + Sen_B05_Mean_30m_aoi + Sen_B06_Mean_30m_aoi + Sen_B07_Mean_30m_aoi + Sen_B08_Mean_30m_aoi + Sen_B08A_Mean_30m_aoi + Sen_B11_Mean_30m_aoi + Sen_B12_Mean_30m_aoi + 
                                                  ClayInd_Mean_Sentinel_khavr + GNDVI_Mean_Sentinel_khavr + MSAVI2_Mean_Sentinel_khavr + NDVI_Mean_Sentinel_khavr + Saturation_Mean_Sentinel_khavr + SGSI_Mean_Sentinel_khavr, 
                                                data = SVR_Micro_Scenario_models_dataset,
                                                kernel = "radial", 
                                                ranges = list(gamma= 2^(-2:2), cost = 2^(-4:10)),
                                                tune.control=tune.control(cross = 5))


KhavrSVR_Micro_modeltune_Fe_RS_Sentinel


KhavrSVR_Micro_Fe_RS_Sentinel <- svm(Fe ~ Sen_B02_30m_aoi + Sen_B03_Mean_30m_aoi + Sen_B04_Mean_30m_aoi + Sen_B05_Mean_30m_aoi + Sen_B06_Mean_30m_aoi + Sen_B07_Mean_30m_aoi + Sen_B08_Mean_30m_aoi + Sen_B08A_Mean_30m_aoi + Sen_B11_Mean_30m_aoi + Sen_B12_Mean_30m_aoi + 
                                               ClayInd_Mean_Sentinel_khavr + GNDVI_Mean_Sentinel_khavr + MSAVI2_Mean_Sentinel_khavr + NDVI_Mean_Sentinel_khavr + Saturation_Mean_Sentinel_khavr + SGSI_Mean_Sentinel_khavr, 
                                             data = SVR_Micro_Scenario_models_dataset[Training_SVR_Micro_scenario, ], kernel="radial", cost=0.25, gamma=0.25)

KhavrSVR_Micro_Fe_RS_Sentinel

par(mfrow = c(2,1))
# Internal validation
KhavrSVR_Micro_Fe_RS_Sentinel_Internal <- predict(KhavrSVR_Micro_Fe_RS_Sentinel, newdata = SVR_Micro_Scenario_models_dataset[Training_SVR_Micro_scenario, ])
KhavrSVR_Micro_Fe_RS_Sentinel_Internal
goof(observed = SVR_Micro_Scenario_models_dataset$Fe[Training_SVR_Micro_scenario], predicted = KhavrSVR_Micro_Fe_RS_Sentinel_Internal ,  plot.it = TRUE)
# externalvalidation
KhavrSVR_Micro_Fe_RS_Sentinel_External <- predict(KhavrSVR_Micro_Fe_RS_Sentinel, newdata = SVR_Micro_Scenario_models_dataset[-Training_SVR_Micro_scenario, ])

goof(observed =  SVR_Micro_Scenario_models_dataset$Fe[-Training_SVR_Micro_scenario], predicted = KhavrSVR_Micro_Fe_RS_Sentinel_External,  plot.it = TRUE)
#training
nrmse(KhavrSVR_Micro_Fe_RS_Sentinel_Internal, SVR_Micro_Scenario_models_dataset$Fe[Training_SVR_Micro_scenario])
MAPE(KhavrSVR_Micro_Fe_RS_Sentinel_Internal, SVR_Micro_Scenario_models_dataset$Fe[Training_SVR_Micro_scenario])

##testing
nrmse(KhavrSVR_Micro_Fe_RS_Sentinel_External, SVR_Micro_Scenario_models_dataset$Fe[-Training_SVR_Micro_scenario])
MAPE(KhavrSVR_Micro_Fe_RS_Sentinel_External, SVR_Micro_Scenario_models_dataset$Fe[-Training_SVR_Micro_scenario])




##Landsat+Sentinel
KhavrSVR_Micro_modeltune_Fe_RS_Landsat_Sentinel <- tune(svm, Fe ~ b2_reflectance + b3_reflectance + b4_reflectance + b5_reflectance + b6_reflectance + b7_reflectance + 
                                                          ci_mean_kh + gndvi_mean_kh + ndvi_mean_kh + satind_mean_kh + sgsi_mean_kh +  
                                                          Sen_B02_30m_aoi + Sen_B03_Mean_30m_aoi + Sen_B04_Mean_30m_aoi + Sen_B05_Mean_30m_aoi + Sen_B06_Mean_30m_aoi + Sen_B07_Mean_30m_aoi + Sen_B08_Mean_30m_aoi + Sen_B08A_Mean_30m_aoi + Sen_B11_Mean_30m_aoi + Sen_B12_Mean_30m_aoi + 
                                                          ClayInd_Mean_Sentinel_khavr + GNDVI_Mean_Sentinel_khavr + MSAVI2_Mean_Sentinel_khavr + NDVI_Mean_Sentinel_khavr + Saturation_Mean_Sentinel_khavr + SGSI_Mean_Sentinel_khavr,    
                                                       data = SVR_Micro_Scenario_models_dataset,
                                                       kernel = "radial", 
                                                       ranges = list(gamma= 2^(-2:2), cost = 2^(-4:10)),
                                                       tune.control=tune.control(cross = 5))

KhavrSVR_Micro_modeltune_Fe_RS_Landsat_Sentinel



KhavrSVR_Micro_Fe_RS_Landsat_Sentinel <- svm(Fe ~ b2_reflectance + b3_reflectance + b4_reflectance + b5_reflectance + b6_reflectance + b7_reflectance + 
                                                       ci_mean_kh + gndvi_mean_kh + ndvi_mean_kh + satind_mean_kh + sgsi_mean_kh +  
                                                       Sen_B02_30m_aoi + Sen_B03_Mean_30m_aoi + Sen_B04_Mean_30m_aoi + Sen_B05_Mean_30m_aoi + Sen_B06_Mean_30m_aoi + Sen_B07_Mean_30m_aoi + Sen_B08_Mean_30m_aoi + Sen_B08A_Mean_30m_aoi + Sen_B11_Mean_30m_aoi + Sen_B12_Mean_30m_aoi + 
                                                       ClayInd_Mean_Sentinel_khavr + GNDVI_Mean_Sentinel_khavr + MSAVI2_Mean_Sentinel_khavr + NDVI_Mean_Sentinel_khavr + Saturation_Mean_Sentinel_khavr + SGSI_Mean_Sentinel_khavr   
                                                     ,data = SVR_Micro_Scenario_models_dataset[Training_SVR_Micro_scenario, ], kernel="radial", cost=0.25, gamma=0.25)

KhavrSVR_Micro_Fe_RS_Landsat_Sentinel


par(mfrow = c(2,1))
# Internal validation
KhavrSVR_Micro_Fe_RS_Landsat_Sentinel_Internal <- predict(KhavrSVR_Micro_Fe_RS_Landsat_Sentinel, newdata = SVR_Micro_Scenario_models_dataset[Training_SVR_Micro_scenario, ])
KhavrSVR_Micro_Fe_RS_Landsat_Sentinel_Internal
goof(observed = SVR_Micro_Scenario_models_dataset$Fe[Training_SVR_Micro_scenario], predicted = KhavrSVR_Micro_Fe_RS_Landsat_Sentinel_Internal ,  plot.it = TRUE)
# externalvalidation
KhavrSVR_Micro_Fe_RS_Landsat_Sentinel_External <- predict(KhavrSVR_Micro_Fe_RS_Landsat_Sentinel, newdata = SVR_Micro_Scenario_models_dataset[-Training_SVR_Micro_scenario, ])

goof(observed =  SVR_Micro_Scenario_models_dataset$Fe[-Training_SVR_Micro_scenario], predicted = KhavrSVR_Micro_Fe_RS_Landsat_Sentinel_External,  plot.it = TRUE)
#training
nrmse(KhavrSVR_Micro_Fe_RS_Landsat_Sentinel_Internal, SVR_Micro_Scenario_models_dataset$Fe[Training_SVR_Micro_scenario])
MAPE(KhavrSVR_Micro_Fe_RS_Landsat_Sentinel_Internal, SVR_Micro_Scenario_models_dataset$Fe[Training_SVR_Micro_scenario])

##testing
nrmse(KhavrSVR_Micro_Fe_RS_Landsat_Sentinel_External, SVR_Micro_Scenario_models_dataset$Fe[-Training_SVR_Micro_scenario])
MAPE(KhavrSVR_Micro_Fe_RS_Landsat_Sentinel_External, SVR_Micro_Scenario_models_dataset$Fe[-Training_SVR_Micro_scenario])




##RS+Topographic
KhavrSVR_Micro_modeltune_Fe_RS_Topographic <- tune(svm, Fe ~ b2_reflectance + b3_reflectance + b4_reflectance + b5_reflectance + b6_reflectance + b7_reflectance + 
                                                     ci_mean_kh + gndvi_mean_kh + ndvi_mean_kh + satind_mean_kh + sgsi_mean_kh +  
                                                     Sen_B02_30m_aoi + Sen_B03_Mean_30m_aoi + Sen_B04_Mean_30m_aoi + Sen_B05_Mean_30m_aoi + Sen_B06_Mean_30m_aoi + Sen_B07_Mean_30m_aoi + Sen_B08_Mean_30m_aoi + Sen_B08A_Mean_30m_aoi + Sen_B11_Mean_30m_aoi + Sen_B12_Mean_30m_aoi +
                                                     ClayInd_Mean_Sentinel_khavr + GNDVI_Mean_Sentinel_khavr + MSAVI2_Mean_Sentinel_khavr + NDVI_Mean_Sentinel_khavr + Saturation_Mean_Sentinel_khavr + SGSI_Mean_Sentinel_khavr +    
                                                     FlowAcc_kh + PlanCur_kh + ProflCur_kh + slope_kh + spi_kh + Twi_kh + aoi_dem_clip_khavr, data = SVR_Micro_Scenario_models_dataset,
                                                  kernel = "radial", 
                                                  ranges = list(gamma= 2^(-2:2), cost = 2^(-4:10)),
                                                  tune.control=tune.control(cross = 5))
KhavrSVR_Micro_modeltune_Fe_RS_Topographic


KhavrSVR_Micro_Fe_RS_Topographic <- svm(Fe ~ b2_reflectance + b3_reflectance + b4_reflectance + b5_reflectance + b6_reflectance + b7_reflectance + 
                                                  ci_mean_kh + gndvi_mean_kh + ndvi_mean_kh + satind_mean_kh + sgsi_mean_kh +  
                                                  Sen_B02_30m_aoi + Sen_B03_Mean_30m_aoi + Sen_B04_Mean_30m_aoi + Sen_B05_Mean_30m_aoi + Sen_B06_Mean_30m_aoi + Sen_B07_Mean_30m_aoi + Sen_B08_Mean_30m_aoi + Sen_B08A_Mean_30m_aoi + Sen_B11_Mean_30m_aoi + Sen_B12_Mean_30m_aoi +
                                                  ClayInd_Mean_Sentinel_khavr + GNDVI_Mean_Sentinel_khavr + MSAVI2_Mean_Sentinel_khavr + NDVI_Mean_Sentinel_khavr + Saturation_Mean_Sentinel_khavr + SGSI_Mean_Sentinel_khavr +    
                                                  FlowAcc_kh + PlanCur_kh + ProflCur_kh + slope_kh + spi_kh + Twi_kh + aoi_dem_clip_khavr, data = SVR_Micro_Scenario_models_dataset[Training_SVR_Micro_scenario, ], kernel="radial", cost=0.125, gamma=0.25)

KhavrSVR_Micro_Fe_RS_Topographic


# Internal validation
KhavrSVR_Micro_Fe_RS_Topographic_Internal <- predict(KhavrSVR_Micro_Fe_RS_Topographic, newdata = SVR_Micro_Scenario_models_dataset[Training_SVR_Micro_scenario, ])
KhavrSVR_Micro_Fe_RS_Topographic_Internal

goof(observed = SVR_Micro_Scenario_models_dataset$Fe[Training_SVR_Micro_scenario], predicted = KhavrSVR_Micro_Fe_RS_Topographic_Internal ,  plot.it = TRUE)

# externalvalidation
KhavrSVR_Micro_Fe_RS_Topographic_External <- predict(KhavrSVR_Micro_Fe_RS_Topographic, newdata = SVR_Micro_Scenario_models_dataset[-Training_SVR_Micro_scenario, ])

goof(observed =  SVR_Micro_Scenario_models_dataset$Fe[-Training_SVR_Micro_scenario], predicted = KhavrSVR_Micro_Fe_RS_Topographic_External,  plot.it = TRUE)
#training
nrmse(KhavrSVR_Micro_Fe_RS_Topographic_Internal, SVR_Micro_Scenario_models_dataset$Fe[Training_SVR_Micro_scenario])
MAPE(KhavrSVR_Micro_Fe_RS_Topographic_Internal, SVR_Micro_Scenario_models_dataset$Fe[Training_SVR_Micro_scenario])

##testing
nrmse(KhavrSVR_Micro_Fe_RS_Topographic_External, SVR_Micro_Scenario_models_dataset$Fe[-Training_SVR_Micro_scenario])
MAPE(KhavrSVR_Micro_Fe_RS_Topographic_External, SVR_Micro_Scenario_models_dataset$Fe[-Training_SVR_Micro_scenario])


##RS+Topographic+Climate
KhavrSVR_Micro_modeltune_Fe_RS_Topographic_Climate <- tune(svm, Fe ~ b2_reflectance + b3_reflectance + b4_reflectance + b5_reflectance + b6_reflectance + b7_reflectance + ci_mean_kh + gndvi_mean_kh + ndvi_mean_kh + satind_mean_kh + sgsi_mean_kh +  
                                                             Sen_B02_30m_aoi + Sen_B03_Mean_30m_aoi + Sen_B04_Mean_30m_aoi + Sen_B05_Mean_30m_aoi + Sen_B06_Mean_30m_aoi + Sen_B07_Mean_30m_aoi + Sen_B08_Mean_30m_aoi + Sen_B08A_Mean_30m_aoi + Sen_B11_Mean_30m_aoi + Sen_B12_Mean_30m_aoi + ClayInd_Mean_Sentinel_khavr + GNDVI_Mean_Sentinel_khavr + MSAVI2_Mean_Sentinel_khavr + NDVI_Mean_Sentinel_khavr + Saturation_Mean_Sentinel_khavr + SGSI_Mean_Sentinel_khavr +    
                                                             FlowAcc_kh + PlanCur_kh + ProflCur_kh + slope_kh + spi_kh + Twi_kh + aoi_dem_clip_khavr + 
                                                             srad_khavr + bio_15_khavr + bio_12_khavr + bio_1_khavr, data = SVR_Micro_Scenario_models_dataset,
                                                           kernel = "radial", 
                                                           ranges = list(gamma= 2^(-2:2), cost = 2^(-4:10)),
                                                           tune.control=tune.control(cross = 5))



KhavrSVR_Micro_modeltune_Fe_RS_Topographic_Climate


KhavrSVR_Micro_Fe_RS_Topographic_Climate <- svm(Fe ~ b2_reflectance + b3_reflectance + b4_reflectance + b5_reflectance + b6_reflectance + b7_reflectance + ci_mean_kh + gndvi_mean_kh + ndvi_mean_kh + satind_mean_kh + sgsi_mean_kh +  
                                                          Sen_B02_30m_aoi + Sen_B03_Mean_30m_aoi + Sen_B04_Mean_30m_aoi + Sen_B05_Mean_30m_aoi + Sen_B06_Mean_30m_aoi + Sen_B07_Mean_30m_aoi + Sen_B08_Mean_30m_aoi + Sen_B08A_Mean_30m_aoi + Sen_B11_Mean_30m_aoi + Sen_B12_Mean_30m_aoi + ClayInd_Mean_Sentinel_khavr + GNDVI_Mean_Sentinel_khavr + MSAVI2_Mean_Sentinel_khavr + NDVI_Mean_Sentinel_khavr + Saturation_Mean_Sentinel_khavr + SGSI_Mean_Sentinel_khavr +    
                                                          FlowAcc_kh + PlanCur_kh + ProflCur_kh + slope_kh + spi_kh + Twi_kh + aoi_dem_clip_khavr + 
                                                          srad_khavr + bio_15_khavr + bio_12_khavr + bio_1_khavr, data = SVR_Micro_Scenario_models_dataset[Training_SVR_Micro_scenario, ], kernel="radial", cost=4, gamma=0.25)


KhavrSVR_Micro_Fe_RS_Topographic_Climate


# Internal validation
KhavrSVR_Micro_Fe_RS_Topographic_Climate_Internal <- predict(KhavrSVR_Micro_Fe_RS_Topographic_Climate, newdata = SVR_Micro_Scenario_models_dataset[Training_SVR_Micro_scenario, ])
KhavrSVR_Micro_Fe_RS_Topographic_Climate_Internal
goof(observed = SVR_Micro_Scenario_models_dataset$Fe[Training_SVR_Micro_scenario], predicted = KhavrSVR_Micro_Fe_RS_Topographic_Climate_Internal ,  plot.it = TRUE)
# externalvalidation
KhavrSVR_Micro_Fe_RS_Topographic_Climate_External <- predict(KhavrSVR_Micro_Fe_RS_Topographic_Climate, newdata = SVR_Micro_Scenario_models_dataset[-Training_SVR_Micro_scenario, ])

goof(observed =  SVR_Micro_Scenario_models_dataset$Fe[-Training_SVR_Micro_scenario], predicted = KhavrSVR_Micro_Fe_RS_Topographic_Climate_External,  plot.it = TRUE)
#training
nrmse(KhavrSVR_Micro_Fe_RS_Topographic_Climate_Internal, SVR_Micro_Scenario_models_dataset$Fe[Training_SVR_Micro_scenario])
MAPE(KhavrSVR_Micro_Fe_RS_Topographic_Climate_Internal, SVR_Micro_Scenario_models_dataset$Fe[Training_SVR_Micro_scenario])

##testing
nrmse(KhavrSVR_Micro_Fe_RS_Topographic_Climate_External, SVR_Micro_Scenario_models_dataset$Fe[-Training_SVR_Micro_scenario])
MAPE(KhavrSVR_Micro_Fe_RS_Topographic_Climate_External, SVR_Micro_Scenario_models_dataset$Fe[-Training_SVR_Micro_scenario])




###RS+Topographic+Climate+Soil
KhavrSVR_Micro_modeltune_Fe_RS_Topographic_Climate_Soil <- tune(svm, Fe ~ b2_reflectance + b3_reflectance + b4_reflectance + b5_reflectance + b6_reflectance + b7_reflectance + ci_mean_kh + gndvi_mean_kh + ndvi_mean_kh + satind_mean_kh + sgsi_mean_kh +  
                                                                  Sen_B02_30m_aoi + Sen_B03_Mean_30m_aoi + Sen_B04_Mean_30m_aoi + Sen_B05_Mean_30m_aoi + Sen_B06_Mean_30m_aoi + Sen_B07_Mean_30m_aoi + Sen_B08_Mean_30m_aoi + Sen_B08A_Mean_30m_aoi + Sen_B11_Mean_30m_aoi + Sen_B12_Mean_30m_aoi + ClayInd_Mean_Sentinel_khavr + GNDVI_Mean_Sentinel_khavr + MSAVI2_Mean_Sentinel_khavr + NDVI_Mean_Sentinel_khavr + Saturation_Mean_Sentinel_khavr + SGSI_Mean_Sentinel_khavr +    
                                                                  FlowAcc_kh + PlanCur_kh + ProflCur_kh + slope_kh + spi_kh + Twi_kh + aoi_dem_clip_khavr + 
                                                                  srad_khavr + bio_15_khavr + bio_12_khavr + bio_1_khavr + 
                                                                  Clay_khavr + pH_khavr + Sand_khavr, data = SVR_Micro_Scenario_models_dataset,
                                                                kernel = "radial", 
                                                                ranges = list(gamma= 2^(-2:2), cost = 2^(-4:10)),
                                                                tune.control=tune.control(cross = 5))

KhavrSVR_Micro_modeltune_Fe_RS_Topographic_Climate_Soil

KhavrSVR_Micro_Fe_RS_Topographic_Climate_Soil <- svm(Fe ~ b2_reflectance + b3_reflectance + b4_reflectance + b5_reflectance + b6_reflectance + b7_reflectance + ci_mean_kh + gndvi_mean_kh + ndvi_mean_kh + satind_mean_kh + sgsi_mean_kh +  
                                                               Sen_B02_30m_aoi + Sen_B03_Mean_30m_aoi + Sen_B04_Mean_30m_aoi + Sen_B05_Mean_30m_aoi + Sen_B06_Mean_30m_aoi + Sen_B07_Mean_30m_aoi + Sen_B08_Mean_30m_aoi + Sen_B08A_Mean_30m_aoi + Sen_B11_Mean_30m_aoi + Sen_B12_Mean_30m_aoi + ClayInd_Mean_Sentinel_khavr + GNDVI_Mean_Sentinel_khavr + MSAVI2_Mean_Sentinel_khavr + NDVI_Mean_Sentinel_khavr + Saturation_Mean_Sentinel_khavr + SGSI_Mean_Sentinel_khavr +    
                                                               FlowAcc_kh + PlanCur_kh + ProflCur_kh + slope_kh + spi_kh + Twi_kh + aoi_dem_clip_khavr + 
                                                               srad_khavr + bio_15_khavr + bio_12_khavr + bio_1_khavr + 
                                                               Clay_khavr + pH_khavr + Sand_khavr, data = SVR_Micro_Scenario_models_dataset[Training_SVR_Micro_scenario, ], kernel="radial", cost=4, gamma=0.25)

KhavrSVR_Micro_Fe_RS_Topographic_Climate_Soil

# Internal validation
KhavrSVR_Micro_Fe_RS_Topographic_Climate_Soil_Internal <- predict(KhavrSVR_Micro_Fe_RS_Topographic_Climate_Soil, newdata = SVR_Micro_Scenario_models_dataset[Training_SVR_Micro_scenario, ])
KhavrSVR_Micro_Fe_RS_Topographic_Climate_Soil_Internal
goof(observed = SVR_Micro_Scenario_models_dataset$Fe[Training_SVR_Micro_scenario], predicted = KhavrSVR_Micro_Fe_RS_Topographic_Climate_Soil_Internal ,  plot.it = TRUE)
# externalvalidation
KhavrSVR_Micro_Fe_RS_Topographic_Climate_Soil_External <- predict(KhavrSVR_Micro_Fe_RS_Topographic_Climate_Soil, newdata = SVR_Micro_Scenario_models_dataset[-Training_SVR_Micro_scenario, ])

goof(observed =  SVR_Micro_Scenario_models_dataset$Fe[-Training_SVR_Micro_scenario], predicted = KhavrSVR_Micro_Fe_RS_Topographic_Climate_Soil_External,  plot.it = TRUE)
#training
nrmse(KhavrSVR_Micro_Fe_RS_Topographic_Climate_Soil_Internal, SVR_Micro_Scenario_models_dataset$Fe[Training_SVR_Micro_scenario])
MAPE(KhavrSVR_Micro_Fe_RS_Topographic_Climate_Soil_Internal, SVR_Micro_Scenario_models_dataset$Fe[Training_SVR_Micro_scenario])

##testing
nrmse(KhavrSVR_Micro_Fe_RS_Topographic_Climate_Soil_External, SVR_Micro_Scenario_models_dataset$Fe[-Training_SVR_Micro_scenario])
MAPE(KhavrSVR_Micro_Fe_RS_Topographic_Climate_Soil_External, SVR_Micro_Scenario_models_dataset$Fe[-Training_SVR_Micro_scenario])


###Topographic
KhavrSVR_Micro_modeltune_Fe_Topographic <- tune(svm, Fe ~ FlowAcc_kh + PlanCur_kh + ProflCur_kh + slope_kh + spi_kh + Twi_kh + aoi_dem_clip_khavr 
                                                , data = SVR_Micro_Scenario_models_dataset,
                                                kernel = "radial", 
                                                ranges = list(gamma= 2^(-2:2), cost = 2^(-4:10)),
                                                tune.control=tune.control(cross = 5))
KhavrSVR_Micro_modeltune_Fe_Topographic

KhavrSVR_Micro_Fe_Topographic <- svm(Fe ~ FlowAcc_kh + PlanCur_kh + ProflCur_kh + slope_kh + spi_kh + Twi_kh + aoi_dem_clip_khavr 
                                             , data = SVR_Micro_Scenario_models_dataset[Training_SVR_Micro_scenario, ], kernel="radial", cost=0.25, gamma=0.25)

KhavrSVR_Micro_Fe_Topographic


# Internal validation
KhavrSVR_Micro_Fe_Topographic_Internal <- predict(KhavrSVR_Micro_Fe_Topographic, newdata = SVR_Micro_Scenario_models_dataset[Training_SVR_Micro_scenario, ])

goof(observed = SVR_Micro_Scenario_models_dataset$Fe[Training_SVR_Micro_scenario], predicted = KhavrSVR_Micro_Fe_Topographic_Internal ,  plot.it = TRUE)
# externalvalidation
KhavrSVR_Micro_Fe_Topographic_External <- predict(KhavrSVR_Micro_Fe_Topographic, newdata = SVR_Micro_Scenario_models_dataset[-Training_SVR_Micro_scenario, ])

goof(observed =  SVR_Micro_Scenario_models_dataset$Fe[-Training_SVR_Micro_scenario], predicted = KhavrSVR_Micro_Fe_Topographic_External,  plot.it = TRUE)
#training
nrmse(KhavrSVR_Micro_Fe_Topographic_Internal, SVR_Micro_Scenario_models_dataset$Fe[Training_SVR_Micro_scenario])
MAPE(KhavrSVR_Micro_Fe_Topographic_Internal, SVR_Micro_Scenario_models_dataset$Fe[Training_SVR_Micro_scenario])

##testing
nrmse(KhavrSVR_Micro_Fe_Topographic_External, SVR_Micro_Scenario_models_dataset$Fe[-Training_SVR_Micro_scenario])
MAPE(KhavrSVR_Micro_Fe_Topographic_External, SVR_Micro_Scenario_models_dataset$Fe[-Training_SVR_Micro_scenario])


###Climate
KhavrSVR_Micro_modeltune_Fe_Climate <- tune(svm, Fe ~  srad_khavr + bio_15_khavr + bio_12_khavr + bio_1_khavr, data = SVR_Micro_Scenario_models_dataset,
                                            kernel = "radial", 
                                            ranges = list(gamma= 2^(-2:2), cost = 2^(-4:10)),
                                            tune.control=tune.control(cross = 5))

KhavrSVR_Micro_modeltune_Fe_Climate

KhavrSVR_Micro_Fe_Climate <- svm(Fe ~  srad_khavr + bio_15_khavr + bio_12_khavr + bio_1_khavr, data = SVR_Micro_Scenario_models_dataset[Training_SVR_Micro_scenario, ], kernel="radial", cost=16, gamma=0.5)
KhavrSVR_Micro_Fe_Climate

# Internal validation
KhavrSVR_Micro_Fe_Climate_Internal <- predict(KhavrSVR_Micro_Fe_Climate, newdata = SVR_Micro_Scenario_models_dataset[Training_SVR_Micro_scenario, ])

goof(observed = SVR_Micro_Scenario_models_dataset$Fe[Training_SVR_Micro_scenario], predicted = KhavrSVR_Micro_Fe_Climate_Internal ,  plot.it = TRUE)
# externalvalidation
KhavrSVR_Micro_Fe_Climate_External <- predict(KhavrSVR_Micro_Fe_Climate, newdata = SVR_Micro_Scenario_models_dataset[-Training_SVR_Micro_scenario, ])

goof(observed =  SVR_Micro_Scenario_models_dataset$Fe[-Training_SVR_Micro_scenario], predicted = KhavrSVR_Micro_Fe_Climate_External,  plot.it = TRUE)
#training
nrmse(KhavrSVR_Micro_Fe_Climate_Internal, SVR_Micro_Scenario_models_dataset$Fe[Training_SVR_Micro_scenario])
MAPE(KhavrSVR_Micro_Fe_Climate_Internal, SVR_Micro_Scenario_models_dataset$Fe[Training_SVR_Micro_scenario])

##testing
nrmse(KhavrSVR_Micro_Fe_Climate_External, SVR_Micro_Scenario_models_dataset$Fe[-Training_SVR_Micro_scenario])
MAPE(KhavrSVR_Micro_Fe_Climate_External, SVR_Micro_Scenario_models_dataset$Fe[-Training_SVR_Micro_scenario])

###Soil
KhavrSVR_Micro_modeltune_Fe_Soil <- tune(svm, Fe ~ b2_reflectance + Clay_khavr + pH_khavr + Sand_khavr,data = SVR_Micro_Scenario_models_dataset,
                                        kernel = "radial", 
                                        ranges = list(gamma= 2^(-2:2), cost = 2^(-4:10)),
                                        tune.control=tune.control(cross = 5))
KhavrSVR_Micro_modeltune_Fe_Soil

KhavrSVR_Micro_Fe_Soil <- svm(Fe ~ b2_reflectance + Clay_khavr + pH_khavr + Sand_khavr, data = SVR_Micro_Scenario_models_dataset[Training_SVR_Micro_scenario, ], kernel="radial", cost=2, gamma=1)
KhavrSVR_Micro_Fe_Soil

# Internal validation
KhavrSVR_Micro_Fe_Soil_Internal <- predict(KhavrSVR_Micro_Fe_Soil, newdata = SVR_Micro_Scenario_models_dataset[Training_SVR_Micro_scenario, ])

goof(observed = SVR_Micro_Scenario_models_dataset$Fe[Training_SVR_Micro_scenario], predicted = KhavrSVR_Micro_Fe_Soil_Internal ,  plot.it = TRUE)
# externalvalidation
KhavrSVR_Micro_Fe_Soil_External <- predict(KhavrSVR_Micro_Fe_Soil, newdata = SVR_Micro_Scenario_models_dataset[-Training_SVR_Micro_scenario, ])

goof(observed =  SVR_Micro_Scenario_models_dataset$Fe[-Training_SVR_Micro_scenario], predicted = KhavrSVR_Micro_Fe_Soil_External,  plot.it = TRUE)

#training
nrmse(KhavrSVR_Micro_Fe_Soil_Internal, SVR_Micro_Scenario_models_dataset$Fe[Training_SVR_Micro_scenario])
MAPE(KhavrSVR_Micro_Fe_Soil_Internal, SVR_Micro_Scenario_models_dataset$Fe[Training_SVR_Micro_scenario])

##testing
nrmse(KhavrSVR_Micro_Fe_Soil_External, SVR_Micro_Scenario_models_dataset$Fe[-Training_SVR_Micro_scenario])
MAPE(KhavrSVR_Micro_Fe_Soil_External, SVR_Micro_Scenario_models_dataset$Fe[-Training_SVR_Micro_scenario])

#####Recursive Feature Elimination
KhavrSVR_Micro_modeltune_Fe_Rec_Fea_Elimination <- tune(svm, Fe ~ b2_reflectance + b3_reflectance + b4_reflectance + b5_reflectance + b6_reflectance + b7_reflectance + ci_mean_kh + gndvi_mean_kh + ndvi_mean_kh + satind_mean_kh + sgsi_mean_kh +  
                                                          Sen_B02_30m_aoi + Sen_B03_Mean_30m_aoi + Sen_B04_Mean_30m_aoi + Sen_B05_Mean_30m_aoi + Sen_B06_Mean_30m_aoi + Sen_B07_Mean_30m_aoi + Sen_B08_Mean_30m_aoi + Sen_B11_Mean_30m_aoi + Sen_B12_Mean_30m_aoi + ClayInd_Mean_Sentinel_khavr + GNDVI_Mean_Sentinel_khavr + MSAVI2_Mean_Sentinel_khavr + NDVI_Mean_Sentinel_khavr + Saturation_Mean_Sentinel_khavr + SGSI_Mean_Sentinel_khavr +    
                                                          FlowAcc_kh + PlanCur_kh + ProflCur_kh + slope_kh + spi_kh + Twi_kh + aoi_dem_clip_khavr + 
                                                          srad_khavr + bio_15_khavr + bio_12_khavr + bio_1_khavr + 
                                                          Clay_khavr + pH_khavr, data = SVR_Micro_Scenario_models_dataset,
                                                       kernel = "radial", 
                                                       ranges = list(gamma= 2^(-2:2), cost = 2^(-4:10)),
                                                       tune.control=tune.control(cross = 5))

KhavrSVR_Micro_modeltune_Fe_Rec_Fea_Elimination


KhavrSVR_Micro_Fe_Rec_Fea_Elimination <- svm(Fe ~ b2_reflectance + b3_reflectance + b4_reflectance + b5_reflectance + b6_reflectance + b7_reflectance + ci_mean_kh + gndvi_mean_kh + ndvi_mean_kh + satind_mean_kh + sgsi_mean_kh +  
                                                       Sen_B02_30m_aoi + Sen_B03_Mean_30m_aoi + Sen_B04_Mean_30m_aoi + Sen_B05_Mean_30m_aoi + Sen_B06_Mean_30m_aoi + Sen_B07_Mean_30m_aoi + Sen_B08_Mean_30m_aoi + Sen_B11_Mean_30m_aoi + Sen_B12_Mean_30m_aoi + ClayInd_Mean_Sentinel_khavr + GNDVI_Mean_Sentinel_khavr + MSAVI2_Mean_Sentinel_khavr + NDVI_Mean_Sentinel_khavr + Saturation_Mean_Sentinel_khavr + SGSI_Mean_Sentinel_khavr +    
                                                       FlowAcc_kh + PlanCur_kh + ProflCur_kh + slope_kh + spi_kh + Twi_kh + aoi_dem_clip_khavr + 
                                                       srad_khavr + bio_15_khavr + bio_12_khavr + bio_1_khavr + 
                                                       Clay_khavr + pH_khavr, data = SVR_Micro_Scenario_models_dataset[Training_SVR_Micro_scenario, ], kernel="radial", cost=1, gamma=0.25)
KhavrSVR_Micro_Fe_Rec_Fea_Elimination
# Internal validation
KhavrSVR_Micro_Fe_Rec_Fea_Elimination_Internal <- predict(KhavrSVR_Micro_Fe_Rec_Fea_Elimination, newdata = SVR_Micro_Scenario_models_dataset[Training_SVR_Micro_scenario, ])

goof(observed = SVR_Micro_Scenario_models_dataset$Fe[Training_SVR_Micro_scenario], predicted = KhavrSVR_Micro_Fe_Rec_Fea_Elimination_Internal ,  plot.it = TRUE)
# externalvalidation
KhavrSVR_Micro_Fe_Rec_Fea_Elimination_External <- predict(KhavrSVR_Micro_Fe_Rec_Fea_Elimination, newdata = SVR_Micro_Scenario_models_dataset[-Training_SVR_Micro_scenario, ])

goof(observed =  SVR_Micro_Scenario_models_dataset$Fe[-Training_SVR_Micro_scenario], predicted = KhavrSVR_Micro_Fe_Rec_Fea_Elimination_External,  plot.it = TRUE)
#training
nrmse(KhavrSVR_Micro_Fe_Rec_Fea_Elimination_Internal, SVR_Micro_Scenario_models_dataset$Fe[Training_SVR_Micro_scenario])
MAPE(KhavrSVR_Micro_Fe_Rec_Fea_Elimination_Internal, SVR_Micro_Scenario_models_dataset$Fe[Training_SVR_Micro_scenario])

##testing
nrmse(KhavrSVR_Micro_Fe_Rec_Fea_Elimination_External, SVR_Micro_Scenario_models_dataset$Fe[-Training_SVR_Micro_scenario])
MAPE(KhavrSVR_Micro_Fe_Rec_Fea_Elimination_External, SVR_Micro_Scenario_models_dataset$Fe[-Training_SVR_Micro_scenario])



###########svr#########
#################EXPERT OPINION AND VIF ############################
library(e1071)
###eXPERT OPINION AND vif COVARIATE analysis
KhavrSVR_Micro_modeltune_Fe_expert <- tune(svm, Fe ~ b2_reflectance + b5_reflectance  + ci_mean_kh +  
                                             Sen_B08_Mean_30m_aoi + GNDVI_Mean_Sentinel_khavr +  Saturation_Mean_Sentinel_khavr +   
                                             FlowAcc_kh + PlanCur_kh + ProflCur_kh + slope_kh + spi_kh + Twi_kh + aoi_dem_clip_khavr + 
                                             srad_khavr + bio_15_khavr + bio_12_khavr + bio_1_khavr, data = RF_Micro_Scenario_models_dataset,
                                            kernel = "radial", 
                                            ranges = list(gamma= 2^(-2:2), cost = 2^(-4:10)),
                                            tune.control=tune.control(cross = 5))

KhavrSVR_Micro_modeltune_Fe_expert
plot(KhavrSVR_Micro_modeltune_Fe_expert)

KhavrSVR_Micro_Fe_expert <- svm(Fe ~ b5_reflectance  + ci_mean_kh +  
                                          Sen_B08_Mean_30m_aoi + GNDVI_Mean_Sentinel_khavr +  Saturation_Mean_Sentinel_khavr +   
                                          FlowAcc_kh + PlanCur_kh + ProflCur_kh + slope_kh + spi_kh + Twi_kh + aoi_dem_clip_khavr + 
                                          srad_khavr + bio_15_khavr + bio_12_khavr + bio_1_khavr,
                                         data = RF_Micro_Scenario_models_dataset[Training_RF_Micro_scenario, ], kernel="radial", cost=4, gamma=0.25)

KhavrSVR_Micro_Fe_expert
plot(KhavrSVR_Micro_Fe_expert)
par(mfrow = c(2,1))
# Internal validation
KhavrSVR_Micro_Fe_expert_Internal <- predict(KhavrSVR_Micro_Fe_expert, newdata = RF_Micro_Scenario_models_dataset[Training_RF_Micro_scenario, ])
KhavrSVR_Micro_Fe_expert_Internal
goof(observed = RF_Micro_Scenario_models_dataset$Fe[Training_RF_Micro_scenario], predicted = KhavrSVR_Micro_Fe_expert_Internal ,  plot.it = TRUE)
# externalvalidation
KhavrSVR_Micro_Fe_expert_External <- predict(KhavrSVR_Micro_Fe_expert, newdata = RF_Micro_Scenario_models_dataset[-Training_RF_Micro_scenario, ])

goof(observed =  RF_Micro_Scenario_models_dataset$Fe[-Training_RF_Micro_scenario], predicted = KhavrSVR_Micro_Fe_expert_External,  plot.it = TRUE)
#training
nrmse(KhavrSVR_Micro_Fe_expert_Internal, RF_Micro_Scenario_models_dataset$Fe[Training_RF_Micro_scenario])
MAPE(KhavrSVR_Micro_Fe_expert_Internal, RF_Micro_Scenario_models_dataset$Fe[Training_RF_Micro_scenario])

##testing
nrmse(KhavrSVR_Micro_Fe_expert_External, RF_Micro_Scenario_models_dataset$Fe[-Training_RF_Micro_scenario])
MAPE(KhavrSVR_Micro_Fe_expert_External, RF_Micro_Scenario_models_dataset$Fe[-Training_RF_Micro_scenario])


#########importance###


w <- t(KhavrSVR_Micro_Fe_expert$coefs) %*% KhavrSVR_Micro_Fe_expert$SV
w <- apply(w, 2, function(v){sqrt(sum(v^2))})
w <- sort(w, decreasing = T)
print(w)
plot(w)

View(KHAVR_SVR_Fe_Expert_Op_IMP)
KHAVR_SVR_Fe_Expert_Op_IMP <- as.data.frame(w)
write.table(KHAVR_SVR_Fe_Expert_Op_IMP, "KHAVR_SVR_Fe_Expert_Op_IMP.txt", sep = ",")

############################MAPPING###############
KhavrSVR_Micro_Fe_expert

##Landsat
b5_reflectance <- raster("D:/Ali keshavarzi/Data1/alan_iran/Covariates/DEM_4_YEARS_SATELLITE_MEANS/b5_reflectance.tif")
ci_mean_kh <- raster("D:/Ali keshavarzi/Data1/alan_iran/Covariates/DEM_4_YEARS_SATELLITE_MEANS/ci_mean_kh.tif")

###DEM
aoi_dem_clip_khavr <- raster("D:/Ali keshavarzi/Data1/alan_iran/Covariates/DEM_4_YEARS_SATELLITE_MEANS/aoi_dem_clip_khavr.tif")
ProflCur_kh <- raster("D:/Ali keshavarzi/Data1/alan_iran/Covariates/DEM_4_YEARS_SATELLITE_MEANS/ProflCur_kh.tif")
PlanCur_kh <- raster("D:/Ali keshavarzi/Data1/alan_iran/Covariates/DEM_4_YEARS_SATELLITE_MEANS/PlanCur_kh.tif")
slope_kh <- raster("D:/Ali keshavarzi/Data1/alan_iran/Covariates/DEM_4_YEARS_SATELLITE_MEANS/slope_kh.tif")
Twi_kh <- raster("D:/Ali keshavarzi/Data1/alan_iran/Covariates/DEM_4_YEARS_SATELLITE_MEANS/Twi_kh.tif")
FlowAcc_kh <- raster("D:/Ali keshavarzi/Data1/alan_iran/Covariates/DEM_4_YEARS_SATELLITE_MEANS/FlowAcc_kh.tif")
spi_kh <- raster("D:/Ali keshavarzi/Data1/alan_iran/Covariates/DEM_4_YEARS_SATELLITE_MEANS/spi_kh.tif")

##10meters
GNDVI_Mean_Sentinel_khavr <- raster("D:/Ali keshavarzi/Data1/Phosphorous_Modelling/Sentinel_Aoi_khavr_clip/GNDVI_Mean_Sentinel_khavr.tif")
Saturation_Mean_Sentinel_khavr <- raster("D:/Ali keshavarzi/Data1/Phosphorous_Modelling/Sentinel_Aoi_khavr_clip/Saturation_Mean_Sentinel_khavr.tif")
Sen_B08_Mean_30m_aoi <- raster("D:/Ali keshavarzi/Data1/Phosphorous_Modelling/Sentinel_10_band_thee_year_means/Sen_B08_Mean_30m_aoi.tif")

##Wordclimdata
bio_1_khavr <-  raster("D:/Ali keshavarzi/Data1/Phosphorous_Modelling/khavr-wordclim_aoi_clip/bio_1_khavr.tif")
srad_khavr <-  raster("D:/Ali keshavarzi/Data1/Phosphorous_Modelling/khavr-wordclim_aoi_clip/srad_khavr.tif")
bio_12_khavr <-  raster("D:/Ali keshavarzi/Data1/Phosphorous_Modelling/khavr-wordclim_aoi_clip/bio_12_khavr.tif")
bio_15_khavr <-  raster("D:/Ali keshavarzi/Data1/Phosphorous_Modelling/khavr-wordclim_aoi_clip/bio_15_khavr.tif")


plot(GNDVI_Mean_Sentinel_khavr)
View(GNDVI_Mean_Sentinel_khavr)

COVS_Expert_Op_Micro <- stack(b5_reflectance, ci_mean_kh, 
                              aoi_dem_clip_khavr, Twi_kh, 
                              ProflCur_kh, FlowAcc_kh, 
                              PlanCur_kh, spi_kh,  
                              slope_kh, GNDVI_Mean_Sentinel_khavr, 
                              Saturation_Mean_Sentinel_khavr, Sen_B08_Mean_30m_aoi,
                              bio_1_khavr, srad_khavr, bio_12_khavr, bio_15_khavr)
compareRaster(Sen_B08_Mean_30m_aoi, Saturation_Mean_Sentinel_khavr) ##extent karsilastirmak için kullanisli
proj4string(COVS_Expert_Op_Micro) <- CRS("+init=epsg:32640")



##mappingsandkhavr_Support_vector_machine
Expert_Op_Micro_Fe_SVR <- predict(COVS_Expert_Op_Micro, KhavrSVR_Micro_Fe_expert, "Expert_Op_Micro_Fe_SVR.tif",
                                  format = "GTiff", datatype = "FLT4S", overwrite = TRUE)

par(mfrow = c(2,1))

plot(Expert_Op_Micro_Fe_SVR,
     main = "Khavr Support Vector Regression 
     predicted 0-30 cm Fe (ppm)")


####Taylor and Hexbin graph
# internalvalidation
KhavrSVR_Micro_Fe_expert_Internal_Pred <- predict(KhavrSVR_Micro_Fe_expert, newdata = RF_Micro_Scenario_models_dataset[Training_RF_Micro_scenario, ])
View(KhavrSVR_Micro_Fe_expert_Internal_Pred)

KhavrSVR_Micro_Fe_expert_Internal_Obs <- subset(RF_Micro_Scenario_models_dataset[Training_RF_Micro_scenario, ], select= c(Fe))
View(KhavrSVR_Micro_Fe_expert_Internal_Obs)

###Birlestir
KhavrSVR_Micro_Fe_expert_column_bind_Calibration <- cbind(KhavrSVR_Micro_Fe_expert_Internal_Obs, KhavrSVR_Micro_Fe_expert_Internal_Pred)
View(KhavrSVR_Micro_Fe_expert_column_bind_Calibration)

colnames(KhavrSVR_Micro_Fe_expert_column_bind_Calibration) <- c("Observed", "Predicted")
View(KhavrSVR_Micro_Fe_expert_column_bind_Calibration)
write.table(KhavrSVR_Micro_Fe_expert_column_bind_Calibration, "KhavrSVR_Micro_Fe_expert_Internal_Observed_Predicted.txt", sep = ",")


#####scatterPlot(KhavrRF_Micro_Fe_expert_column_bind_Calibration, x ="Observed" , y = "Predicted",xlab=10, ylab=10,method = "hexbin",mod.line=T,auto.text=F, col = "jet", xbin = 30)








# externalvalidation
KhavrSVR_Micro_Fe_expert_External_Pred <- predict(KhavrSVR_Micro_Fe_expert, newdata = RF_Micro_Scenario_models_dataset[-Training_RF_Micro_scenario, ])
View(KhavrSVR_Micro_Fe_expert_External_Pred)

KhavrSVR_Micro_Fe_expert_External_Obs <- subset(RF_Micro_Scenario_models_dataset[-Training_RF_Micro_scenario, ], select= c(Fe))
View(KhavrSVR_Micro_Fe_expert_External_Obs)

###Birlestir
KhavrSVR_Micro_Fe_expert_column_bind_Validation <- cbind(KhavrSVR_Micro_Fe_expert_External_Obs, KhavrSVR_Micro_Fe_expert_External_Pred)
View(KhavrSVR_Micro_Fe_expert_column_bind_Validation)

colnames(KhavrSVR_Micro_Fe_expert_column_bind_Validation) <- c("Observed", "Predicted")
View(KhavrSVR_Micro_Fe_expert_column_bind_Validation)
write.table(KhavrSVR_Micro_Fe_expert_column_bind_Validation, "KhavrSVR_Micro_Fe_expert_Validation_Observed_Predicted.txt", sep = ",")









