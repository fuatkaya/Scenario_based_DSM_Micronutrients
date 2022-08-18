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



###tuning_Mn
ctrl_Micro_rf <- trainControl(method= "repeatedcv", number = 5, repeats = 3, search = "random")

###Landsat
KhavrRF_Micro_modeltune_Zn_RS_Landsat <- train(Zn ~ b2_reflectance + b3_reflectance + b4_reflectance + b5_reflectance + b6_reflectance + b7_reflectance + 
                                                 ci_mean_kh + gndvi_mean_kh + ndvi_mean_kh + satind_mean_kh + sgsi_mean_kh, method ="rf", data = RF_Micro_Scenario_models_dataset[Training_RF_Micro_scenario, ], 
                                               trainControl=ctrl_Micro_rf)

KhavrRF_Micro_modeltune_Zn_RS_Landsat

plot(KhavrRF_Micro_modeltune_Zn_RS_Landsat)
library(randomForest)
KhavrRF_Micro_Zn_RS_Landsat <- randomForest(Zn ~ b2_reflectance + b3_reflectance + b4_reflectance + b5_reflectance + b6_reflectance + b7_reflectance + 
                                              ci_mean_kh + gndvi_mean_kh + ndvi_mean_kh + satind_mean_kh + sgsi_mean_kh, data = RF_Micro_Scenario_models_dataset[Training_RF_Micro_scenario, ], mtry = 2, ntree = 500)


KhavrRF_Micro_Zn_RS_Landsat
plot(KhavrRF_Micro_Zn_RS_Landsat)

par(mfrow = c(2,1))
# Internal validation
KhavrRF_Micro_Zn_RS_Landsat_Internal <- predict(KhavrRF_Micro_Zn_RS_Landsat, newdata = RF_Micro_Scenario_models_dataset[Training_RF_Micro_scenario, ])
KhavrRF_Micro_Zn_RS_Landsat_Internal
goof(observed = RF_Micro_Scenario_models_dataset$Zn[Training_RF_Micro_scenario], predicted = KhavrRF_Micro_Zn_RS_Landsat_Internal ,  plot.it = TRUE)
# externalvalidation
KhavrRF_Micro_Zn_RS_Landsat_External <- predict(KhavrRF_Micro_Zn_RS_Landsat, newdata = RF_Micro_Scenario_models_dataset[-Training_RF_Micro_scenario, ])

goof(observed =  RF_Micro_Scenario_models_dataset$Zn[-Training_RF_Micro_scenario], predicted = KhavrRF_Micro_Zn_RS_Landsat_External ,  plot.it = TRUE)


library(hydroGOF)
library(PerformanceAnalytics)
library(MLmetrics)
#training
nrmse(KhavrRF_Micro_Zn_RS_Landsat_Internal, RF_Micro_Scenario_models_dataset$Zn[Training_RF_Micro_scenario])
MAPE(KhavrRF_Micro_Zn_RS_Landsat_Internal, RF_Micro_Scenario_models_dataset$Zn[Training_RF_Micro_scenario])

##testing
nrmse(KhavrRF_Micro_Zn_RS_Landsat_External, RF_Micro_Scenario_models_dataset$Zn[-Training_RF_Micro_scenario])
MAPE(KhavrRF_Micro_Zn_RS_Landsat_External, RF_Micro_Scenario_models_dataset$Zn[-Training_RF_Micro_scenario])





##Sentinel
KhavrRF_Micro_modeltune_Zn_RS_Sentinel <- train(Zn ~ Sen_B02_30m_aoi + Sen_B03_Mean_30m_aoi + Sen_B04_Mean_30m_aoi + Sen_B05_Mean_30m_aoi + Sen_B06_Mean_30m_aoi + Sen_B07_Mean_30m_aoi + Sen_B08_Mean_30m_aoi + Sen_B08A_Mean_30m_aoi + Sen_B11_Mean_30m_aoi + Sen_B12_Mean_30m_aoi + 
                                                  ClayInd_Mean_Sentinel_khavr + GNDVI_Mean_Sentinel_khavr + MSAVI2_Mean_Sentinel_khavr + NDVI_Mean_Sentinel_khavr + Saturation_Mean_Sentinel_khavr + SGSI_Mean_Sentinel_khavr, 
                                                method ="rf", data = RF_Micro_Scenario_models_dataset[Training_RF_Micro_scenario, ], trainControl=ctrl_Micro_rf)


KhavrRF_Micro_modeltune_Zn_RS_Sentinel

plot(KhavrRF_Micro_modeltune_Zn_RS_Sentinel)


KhavrRF_Micro_Zn_RS_Sentinel <- randomForest(Zn ~ Sen_B02_30m_aoi + Sen_B03_Mean_30m_aoi + Sen_B04_Mean_30m_aoi + Sen_B05_Mean_30m_aoi + Sen_B06_Mean_30m_aoi + Sen_B07_Mean_30m_aoi + Sen_B08_Mean_30m_aoi + Sen_B08A_Mean_30m_aoi + Sen_B11_Mean_30m_aoi + Sen_B12_Mean_30m_aoi + 
                                               ClayInd_Mean_Sentinel_khavr + GNDVI_Mean_Sentinel_khavr + MSAVI2_Mean_Sentinel_khavr + NDVI_Mean_Sentinel_khavr + Saturation_Mean_Sentinel_khavr + SGSI_Mean_Sentinel_khavr, 
                                             data = RF_Micro_Scenario_models_dataset[Training_RF_Micro_scenario, ], mtry = 2, ntree = 500)

KhavrRF_Micro_Zn_RS_Sentinel
plot(KhavrRF_Micro_Zn_RS_Sentinel)

par(mfrow = c(2,1))
# Internal validation
KhavrRF_Micro_Zn_RS_Sentinel_Internal <- predict(KhavrRF_Micro_Zn_RS_Sentinel, newdata = RF_Micro_Scenario_models_dataset[Training_RF_Micro_scenario, ])

goof(observed = RF_Micro_Scenario_models_dataset$Zn[Training_RF_Micro_scenario], predicted = KhavrRF_Micro_Zn_RS_Sentinel_Internal ,  plot.it = TRUE)
# externalvalidation
KhavrRF_Micro_Zn_RS_Sentinel_External <- predict(KhavrRF_Micro_Zn_RS_Sentinel, newdata = RF_Micro_Scenario_models_dataset[-Training_RF_Micro_scenario, ])

goof(observed =  RF_Micro_Scenario_models_dataset$Zn[-Training_RF_Micro_scenario], predicted = KhavrRF_Micro_Zn_RS_Sentinel_External,  plot.it = TRUE)
#training
nrmse(KhavrRF_Micro_Zn_RS_Sentinel_Internal, RF_Micro_Scenario_models_dataset$Zn[Training_RF_Micro_scenario])
MAPE(KhavrRF_Micro_Zn_RS_Sentinel_Internal, RF_Micro_Scenario_models_dataset$Zn[Training_RF_Micro_scenario])

##testing
nrmse(KhavrRF_Micro_Zn_RS_Sentinel_External, RF_Micro_Scenario_models_dataset$Zn[-Training_RF_Micro_scenario])
MAPE(KhavrRF_Micro_Zn_RS_Sentinel_External, RF_Micro_Scenario_models_dataset$Zn[-Training_RF_Micro_scenario])




##Landsat+Sentinel
KhavrRF_Micro_modeltune_Zn_RS_Landsat_Sentinel <- train(Zn ~ b2_reflectance + b3_reflectance + b4_reflectance + b5_reflectance + b6_reflectance + b7_reflectance + 
                                                          ci_mean_kh + gndvi_mean_kh + ndvi_mean_kh + satind_mean_kh + sgsi_mean_kh +  
                                                          Sen_B02_30m_aoi + Sen_B03_Mean_30m_aoi + Sen_B04_Mean_30m_aoi + Sen_B05_Mean_30m_aoi + Sen_B06_Mean_30m_aoi + Sen_B07_Mean_30m_aoi + Sen_B08_Mean_30m_aoi + Sen_B08A_Mean_30m_aoi + Sen_B11_Mean_30m_aoi + Sen_B12_Mean_30m_aoi + 
                                                          ClayInd_Mean_Sentinel_khavr + GNDVI_Mean_Sentinel_khavr + MSAVI2_Mean_Sentinel_khavr + NDVI_Mean_Sentinel_khavr + Saturation_Mean_Sentinel_khavr + SGSI_Mean_Sentinel_khavr   
                                                        , method ="rf", data = RF_Micro_Scenario_models_dataset[Training_RF_Micro_scenario, ], trainControl=ctrl_Micro_rf)

KhavrRF_Micro_modeltune_Zn_RS_Landsat_Sentinel

plot(KhavrRF_Micro_modeltune_Zn_RS_Landsat_Sentinel)


KhavrRF_Micro_Zn_RS_Landsat_Sentinel <- randomForest(Zn ~ b2_reflectance + b3_reflectance + b4_reflectance + b5_reflectance + b6_reflectance + b7_reflectance + 
                                                       ci_mean_kh + gndvi_mean_kh + ndvi_mean_kh + satind_mean_kh + sgsi_mean_kh +  
                                                       Sen_B02_30m_aoi + Sen_B03_Mean_30m_aoi + Sen_B04_Mean_30m_aoi + Sen_B05_Mean_30m_aoi + Sen_B06_Mean_30m_aoi + Sen_B07_Mean_30m_aoi + Sen_B08_Mean_30m_aoi + Sen_B08A_Mean_30m_aoi + Sen_B11_Mean_30m_aoi + Sen_B12_Mean_30m_aoi + 
                                                       ClayInd_Mean_Sentinel_khavr + GNDVI_Mean_Sentinel_khavr + MSAVI2_Mean_Sentinel_khavr + NDVI_Mean_Sentinel_khavr + Saturation_Mean_Sentinel_khavr + SGSI_Mean_Sentinel_khavr   
                                                     ,data = RF_Micro_Scenario_models_dataset[Training_RF_Micro_scenario, ], mtry=2, ntree=500)

KhavrRF_Micro_Zn_RS_Landsat_Sentinel

plot(KhavrRF_Micro_Zn_RS_Landsat_Sentinel)

par(mfrow = c(2,1))
# Internal validation
KhavrRF_Micro_Zn_RS_Landsat_Sentinel_Internal <- predict(KhavrRF_Micro_Zn_RS_Landsat_Sentinel, newdata = RF_Micro_Scenario_models_dataset[Training_RF_Micro_scenario, ])

goof(observed = RF_Micro_Scenario_models_dataset$Zn[Training_RF_Micro_scenario], predicted = KhavrRF_Micro_Zn_RS_Landsat_Sentinel_Internal ,  plot.it = TRUE)
# externalvalidation
KhavrRF_Micro_Zn_RS_Landsat_Sentinel_External <- predict(KhavrRF_Micro_Zn_RS_Landsat_Sentinel, newdata = RF_Micro_Scenario_models_dataset[-Training_RF_Micro_scenario, ])

goof(observed =  RF_Micro_Scenario_models_dataset$Zn[-Training_RF_Micro_scenario], predicted = KhavrRF_Micro_Zn_RS_Landsat_Sentinel_External,  plot.it = TRUE)
#training
nrmse(KhavrRF_Micro_Zn_RS_Landsat_Sentinel_Internal, RF_Micro_Scenario_models_dataset$Zn[Training_RF_Micro_scenario])
MAPE(KhavrRF_Micro_Zn_RS_Landsat_Sentinel_Internal, RF_Micro_Scenario_models_dataset$Zn[Training_RF_Micro_scenario])

##testing
nrmse(KhavrRF_Micro_Zn_RS_Landsat_Sentinel_External, RF_Micro_Scenario_models_dataset$Zn[-Training_RF_Micro_scenario])
MAPE(KhavrRF_Micro_Zn_RS_Landsat_Sentinel_External, RF_Micro_Scenario_models_dataset$Zn[-Training_RF_Micro_scenario])




##RS+Topographic
KhavrRF_Micro_modeltune_Zn_RS_Topographic <- train(Zn ~ b2_reflectance + b3_reflectance + b4_reflectance + b5_reflectance + b6_reflectance + b7_reflectance + 
                                                     ci_mean_kh + gndvi_mean_kh + ndvi_mean_kh + satind_mean_kh + sgsi_mean_kh +  
                                                     Sen_B02_30m_aoi + Sen_B03_Mean_30m_aoi + Sen_B04_Mean_30m_aoi + Sen_B05_Mean_30m_aoi + Sen_B06_Mean_30m_aoi + Sen_B07_Mean_30m_aoi + Sen_B08_Mean_30m_aoi + Sen_B08A_Mean_30m_aoi + Sen_B11_Mean_30m_aoi + Sen_B12_Mean_30m_aoi +
                                                     ClayInd_Mean_Sentinel_khavr + GNDVI_Mean_Sentinel_khavr + MSAVI2_Mean_Sentinel_khavr + NDVI_Mean_Sentinel_khavr + Saturation_Mean_Sentinel_khavr + SGSI_Mean_Sentinel_khavr +    
                                                     FlowAcc_kh + PlanCur_kh + ProflCur_kh + slope_kh + spi_kh + Twi_kh + aoi_dem_clip_khavr, method ="rf", data = RF_Micro_Scenario_models_dataset[Training_RF_Micro_scenario, ], trainControl=ctrl_Micro_rf)
KhavrRF_Micro_modeltune_Zn_RS_Topographic
plot(KhavrRF_Micro_modeltune_Zn_RS_Topographic)


KhavrRF_Micro_Zn_RS_Topographic <- randomForest(Zn ~ b2_reflectance + b3_reflectance + b4_reflectance + b5_reflectance + b6_reflectance + b7_reflectance + 
                                                  ci_mean_kh + gndvi_mean_kh + ndvi_mean_kh + satind_mean_kh + sgsi_mean_kh +  
                                                  Sen_B02_30m_aoi + Sen_B03_Mean_30m_aoi + Sen_B04_Mean_30m_aoi + Sen_B05_Mean_30m_aoi + Sen_B06_Mean_30m_aoi + Sen_B07_Mean_30m_aoi + Sen_B08_Mean_30m_aoi + Sen_B08A_Mean_30m_aoi + Sen_B11_Mean_30m_aoi + Sen_B12_Mean_30m_aoi +
                                                  ClayInd_Mean_Sentinel_khavr + GNDVI_Mean_Sentinel_khavr + MSAVI2_Mean_Sentinel_khavr + NDVI_Mean_Sentinel_khavr + Saturation_Mean_Sentinel_khavr + SGSI_Mean_Sentinel_khavr +    
                                                  FlowAcc_kh + PlanCur_kh + ProflCur_kh + slope_kh + spi_kh + Twi_kh + aoi_dem_clip_khavr, data = RF_Micro_Scenario_models_dataset[Training_RF_Micro_scenario, ], mtry = 2 , ntree = 500)

KhavrRF_Micro_Zn_RS_Topographic
plot(KhavrRF_Micro_Zn_RS_Topographic)


# Internal validation
KhavrRF_Micro_Zn_RS_Topographic_Internal <- predict(KhavrRF_Micro_Zn_RS_Topographic, newdata = RF_Micro_Scenario_models_dataset[Training_RF_Micro_scenario, ])

goof(observed = RF_Micro_Scenario_models_dataset$Zn[Training_RF_Micro_scenario], predicted = KhavrRF_Micro_Zn_RS_Topographic_Internal ,  plot.it = TRUE)
# externalvalidation
KhavrRF_Micro_Zn_RS_Topographic_External <- predict(KhavrRF_Micro_Zn_RS_Topographic, newdata = RF_Micro_Scenario_models_dataset[-Training_RF_Micro_scenario, ])

goof(observed =  RF_Micro_Scenario_models_dataset$Zn[-Training_RF_Micro_scenario], predicted = KhavrRF_Micro_Zn_RS_Topographic_External,  plot.it = TRUE)
#training
nrmse(KhavrRF_Micro_Zn_RS_Topographic_Internal, RF_Micro_Scenario_models_dataset$Zn[Training_RF_Micro_scenario])
MAPE(KhavrRF_Micro_Zn_RS_Topographic_Internal, RF_Micro_Scenario_models_dataset$Zn[Training_RF_Micro_scenario])

##testing
nrmse(KhavrRF_Micro_Zn_RS_Topographic_External, RF_Micro_Scenario_models_dataset$Zn[-Training_RF_Micro_scenario])
MAPE(KhavrRF_Micro_Zn_RS_Topographic_External, RF_Micro_Scenario_models_dataset$Zn[-Training_RF_Micro_scenario])


##RS+Topographic+Climate
KhavrRF_Micro_modeltune_Zn_RS_Topographic_Climate <- train(Zn ~ b2_reflectance + b3_reflectance + b4_reflectance + b5_reflectance + b6_reflectance + b7_reflectance + ci_mean_kh + gndvi_mean_kh + ndvi_mean_kh + satind_mean_kh + sgsi_mean_kh +  
                                                             Sen_B02_30m_aoi + Sen_B03_Mean_30m_aoi + Sen_B04_Mean_30m_aoi + Sen_B05_Mean_30m_aoi + Sen_B06_Mean_30m_aoi + Sen_B07_Mean_30m_aoi + Sen_B08_Mean_30m_aoi + Sen_B08A_Mean_30m_aoi + Sen_B11_Mean_30m_aoi + Sen_B12_Mean_30m_aoi + ClayInd_Mean_Sentinel_khavr + GNDVI_Mean_Sentinel_khavr + MSAVI2_Mean_Sentinel_khavr + NDVI_Mean_Sentinel_khavr + Saturation_Mean_Sentinel_khavr + SGSI_Mean_Sentinel_khavr +    
                                                             FlowAcc_kh + PlanCur_kh + ProflCur_kh + slope_kh + spi_kh + Twi_kh + aoi_dem_clip_khavr + 
                                                             srad_khavr + bio_15_khavr + bio_12_khavr + bio_1_khavr, method ="rf", data = RF_Micro_Scenario_models_dataset[Training_RF_Micro_scenario, ], trainControl=ctrl_Micro_rf)



KhavrRF_Micro_modeltune_Zn_RS_Topographic_Climate
plot(KhavrRF_Micro_modeltune_Zn_RS_Topographic_Climate)


KhavrRF_Micro_Zn_RS_Topographic_Climate <- randomForest(Zn ~ b2_reflectance + b3_reflectance + b4_reflectance + b5_reflectance + b6_reflectance + b7_reflectance + ci_mean_kh + gndvi_mean_kh + ndvi_mean_kh + satind_mean_kh + sgsi_mean_kh +  
                                                          Sen_B02_30m_aoi + Sen_B03_Mean_30m_aoi + Sen_B04_Mean_30m_aoi + Sen_B05_Mean_30m_aoi + Sen_B06_Mean_30m_aoi + Sen_B07_Mean_30m_aoi + Sen_B08_Mean_30m_aoi + Sen_B08A_Mean_30m_aoi + Sen_B11_Mean_30m_aoi + Sen_B12_Mean_30m_aoi + ClayInd_Mean_Sentinel_khavr + GNDVI_Mean_Sentinel_khavr + MSAVI2_Mean_Sentinel_khavr + NDVI_Mean_Sentinel_khavr + Saturation_Mean_Sentinel_khavr + SGSI_Mean_Sentinel_khavr +    
                                                          FlowAcc_kh + PlanCur_kh + ProflCur_kh + slope_kh + spi_kh + Twi_kh + aoi_dem_clip_khavr + 
                                                          srad_khavr + bio_15_khavr + bio_12_khavr + bio_1_khavr, method ="rf", data = RF_Micro_Scenario_models_dataset[Training_RF_Micro_scenario, ], mtry = 2 , ntree = 500)


KhavrRF_Micro_Zn_RS_Topographic_Climate
plot(KhavrRF_Micro_Zn_RS_Topographic_Climate)



# Internal validation
KhavrRF_Micro_Zn_RS_Topographic_Climate_Internal <- predict(KhavrRF_Micro_Zn_RS_Topographic_Climate, newdata = RF_Micro_Scenario_models_dataset[Training_RF_Micro_scenario, ])

goof(observed = RF_Micro_Scenario_models_dataset$Zn[Training_RF_Micro_scenario], predicted = KhavrRF_Micro_Zn_RS_Topographic_Climate_Internal ,  plot.it = TRUE)
# externalvalidation
KhavrRF_Micro_Zn_RS_Topographic_Climate_External <- predict(KhavrRF_Micro_Zn_RS_Topographic_Climate, newdata = RF_Micro_Scenario_models_dataset[-Training_RF_Micro_scenario, ])

goof(observed =  RF_Micro_Scenario_models_dataset$Zn[-Training_RF_Micro_scenario], predicted = KhavrRF_Micro_Zn_RS_Topographic_Climate_External,  plot.it = TRUE)
#training
nrmse(KhavrRF_Micro_Zn_RS_Topographic_Climate_Internal, RF_Micro_Scenario_models_dataset$Zn[Training_RF_Micro_scenario])
MAPE(KhavrRF_Micro_Zn_RS_Topographic_Climate_Internal, RF_Micro_Scenario_models_dataset$Zn[Training_RF_Micro_scenario])

##testing
nrmse(KhavrRF_Micro_Zn_RS_Topographic_Climate_External, RF_Micro_Scenario_models_dataset$Zn[-Training_RF_Micro_scenario])
MAPE(KhavrRF_Micro_Zn_RS_Topographic_Climate_External, RF_Micro_Scenario_models_dataset$Zn[-Training_RF_Micro_scenario])




###RS+Topographic+Climate+Soil
KhavrRF_Micro_modeltune_Zn_RS_Topographic_Climate_Soil <- train(Zn ~ b2_reflectance + b3_reflectance + b4_reflectance + b5_reflectance + b6_reflectance + b7_reflectance + ci_mean_kh + gndvi_mean_kh + ndvi_mean_kh + satind_mean_kh + sgsi_mean_kh +  
                                                                  Sen_B02_30m_aoi + Sen_B03_Mean_30m_aoi + Sen_B04_Mean_30m_aoi + Sen_B05_Mean_30m_aoi + Sen_B06_Mean_30m_aoi + Sen_B07_Mean_30m_aoi + Sen_B08_Mean_30m_aoi + Sen_B08A_Mean_30m_aoi + Sen_B11_Mean_30m_aoi + Sen_B12_Mean_30m_aoi + ClayInd_Mean_Sentinel_khavr + GNDVI_Mean_Sentinel_khavr + MSAVI2_Mean_Sentinel_khavr + NDVI_Mean_Sentinel_khavr + Saturation_Mean_Sentinel_khavr + SGSI_Mean_Sentinel_khavr +    
                                                                  FlowAcc_kh + PlanCur_kh + ProflCur_kh + slope_kh + spi_kh + Twi_kh + aoi_dem_clip_khavr + 
                                                                  srad_khavr + bio_15_khavr + bio_12_khavr + bio_1_khavr + 
                                                                  Clay_khavr + pH_khavr + Sand_khavr, method ="rf", data = RF_Micro_Scenario_models_dataset[Training_RF_Micro_scenario, ], trainControl=ctrl_Micro_rf)

KhavrRF_Micro_modeltune_Zn_RS_Topographic_Climate_Soil
plot(KhavrRF_Micro_modeltune_Zn_RS_Topographic_Climate_Soil)

KhavrRF_Micro_Zn_RS_Topographic_Climate_Soil <- randomForest(Zn ~ b2_reflectance + b3_reflectance + b4_reflectance + b5_reflectance + b6_reflectance + b7_reflectance + ci_mean_kh + gndvi_mean_kh + ndvi_mean_kh + satind_mean_kh + sgsi_mean_kh +  
                                                               Sen_B02_30m_aoi + Sen_B03_Mean_30m_aoi + Sen_B04_Mean_30m_aoi + Sen_B05_Mean_30m_aoi + Sen_B06_Mean_30m_aoi + Sen_B07_Mean_30m_aoi + Sen_B08_Mean_30m_aoi + Sen_B08A_Mean_30m_aoi + Sen_B11_Mean_30m_aoi + Sen_B12_Mean_30m_aoi + ClayInd_Mean_Sentinel_khavr + GNDVI_Mean_Sentinel_khavr + MSAVI2_Mean_Sentinel_khavr + NDVI_Mean_Sentinel_khavr + Saturation_Mean_Sentinel_khavr + SGSI_Mean_Sentinel_khavr +    
                                                               FlowAcc_kh + PlanCur_kh + ProflCur_kh + slope_kh + spi_kh + Twi_kh + aoi_dem_clip_khavr + 
                                                               srad_khavr + bio_15_khavr + bio_12_khavr + bio_1_khavr + 
                                                               Clay_khavr + pH_khavr + Sand_khavr, method ="rf", data = RF_Micro_Scenario_models_dataset[Training_RF_Micro_scenario, ], mtry = 21, ntree = 500)

KhavrRF_Micro_Zn_RS_Topographic_Climate_Soil
plot(KhavrRF_Micro_Zn_RS_Topographic_Climate_Soil)

# Internal validation
KhavrRF_Micro_Zn_RS_Topographic_Climate_Soil_Internal <- predict(KhavrRF_Micro_Zn_RS_Topographic_Climate_Soil, newdata = RF_Micro_Scenario_models_dataset[Training_RF_Micro_scenario, ])

goof(observed = RF_Micro_Scenario_models_dataset$Zn[Training_RF_Micro_scenario], predicted = KhavrRF_Micro_Zn_RS_Topographic_Climate_Soil_Internal ,  plot.it = TRUE)
# externalvalidation
KhavrRF_Micro_Zn_RS_Topographic_Climate_Soil_External <- predict(KhavrRF_Micro_Zn_RS_Topographic_Climate_Soil, newdata = RF_Micro_Scenario_models_dataset[-Training_RF_Micro_scenario, ])

goof(observed =  RF_Micro_Scenario_models_dataset$Zn[-Training_RF_Micro_scenario], predicted = KhavrRF_Micro_Zn_RS_Topographic_Climate_Soil_External,  plot.it = TRUE)
#training
nrmse(KhavrRF_Micro_Zn_RS_Topographic_Climate_Soil_Internal, RF_Micro_Scenario_models_dataset$Zn[Training_RF_Micro_scenario])
MAPE(KhavrRF_Micro_Zn_RS_Topographic_Climate_Soil_Internal, RF_Micro_Scenario_models_dataset$Zn[Training_RF_Micro_scenario])

##testing
nrmse(KhavrRF_Micro_Zn_RS_Topographic_Climate_Soil_External, RF_Micro_Scenario_models_dataset$Zn[-Training_RF_Micro_scenario])
MAPE(KhavrRF_Micro_Zn_RS_Topographic_Climate_Soil_External, RF_Micro_Scenario_models_dataset$Zn[-Training_RF_Micro_scenario])


###Topographic
KhavrRF_Micro_modeltune_Zn_Topographic <- train(Zn ~ FlowAcc_kh + PlanCur_kh + ProflCur_kh + slope_kh + spi_kh + Twi_kh + aoi_dem_clip_khavr 
                                                , method ="rf", data = RF_Micro_Scenario_models_dataset[Training_RF_Micro_scenario, ], trainControl=ctrl_Micro_rf)
KhavrRF_Micro_modeltune_Zn_Topographic
plot(KhavrRF_Micro_modeltune_Zn_Topographic)

KhavrRF_Micro_Zn_Topographic <- randomForest(Zn ~ FlowAcc_kh + PlanCur_kh + ProflCur_kh + slope_kh + spi_kh + Twi_kh + aoi_dem_clip_khavr 
                                             , method ="rf", data = RF_Micro_Scenario_models_dataset[Training_RF_Micro_scenario, ], mtry =2 , ntree = 500)

KhavrRF_Micro_Zn_Topographic
plot(KhavrRF_Micro_Zn_Topographic)


# Internal validation
KhavrRF_Micro_Zn_Topographic_Internal <- predict(KhavrRF_Micro_Zn_Topographic, newdata = RF_Micro_Scenario_models_dataset[Training_RF_Micro_scenario, ])

goof(observed = RF_Micro_Scenario_models_dataset$Zn[Training_RF_Micro_scenario], predicted = KhavrRF_Micro_Zn_Topographic_Internal ,  plot.it = TRUE)
# externalvalidation
KhavrRF_Micro_Zn_Topographic_External <- predict(KhavrRF_Micro_Zn_Topographic, newdata = RF_Micro_Scenario_models_dataset[-Training_RF_Micro_scenario, ])

goof(observed =  RF_Micro_Scenario_models_dataset$Zn[-Training_RF_Micro_scenario], predicted = KhavrRF_Micro_Zn_Topographic_External,  plot.it = TRUE)
#training
nrmse(KhavrRF_Micro_Zn_Topographic_Internal, RF_Micro_Scenario_models_dataset$Zn[Training_RF_Micro_scenario])
MAPE(KhavrRF_Micro_Zn_Topographic_Internal, RF_Micro_Scenario_models_dataset$Zn[Training_RF_Micro_scenario])

##testing
nrmse(KhavrRF_Micro_Zn_Topographic_External, RF_Micro_Scenario_models_dataset$Zn[-Training_RF_Micro_scenario])
MAPE(KhavrRF_Micro_Zn_Topographic_External, RF_Micro_Scenario_models_dataset$Zn[-Training_RF_Micro_scenario])


###Climate
KhavrRF_Micro_modeltune_Zn_Climate <- train(Zn ~  srad_khavr + bio_15_khavr + bio_12_khavr + bio_1_khavr, method ="rf", data = RF_Micro_Scenario_models_dataset[Training_RF_Micro_scenario, ], trainControl=ctrl_Micro_rf)

KhavrRF_Micro_modeltune_Zn_Climate
plot(KhavrRF_Micro_modeltune_Zn_Climate)

KhavrRF_Micro_Zn_Climate <- randomForest(Zn ~  srad_khavr + bio_15_khavr + bio_12_khavr + bio_1_khavr, data = RF_Micro_Scenario_models_dataset[Training_RF_Micro_scenario, ], mtry = 2, ntree = 500)
KhavrRF_Micro_Zn_Climate
plot(KhavrRF_Micro_Zn_Climate)

# Internal validation
KhavrRF_Micro_Zn_Climate_Internal <- predict(KhavrRF_Micro_Zn_Climate, newdata = RF_Micro_Scenario_models_dataset[Training_RF_Micro_scenario, ])

goof(observed = RF_Micro_Scenario_models_dataset$Zn[Training_RF_Micro_scenario], predicted = KhavrRF_Micro_Zn_Climate_Internal ,  plot.it = TRUE)
# externalvalidation
KhavrRF_Micro_Zn_Climate_External <- predict(KhavrRF_Micro_Zn_Climate, newdata = RF_Micro_Scenario_models_dataset[-Training_RF_Micro_scenario, ])

goof(observed =  RF_Micro_Scenario_models_dataset$Zn[-Training_RF_Micro_scenario], predicted = KhavrRF_Micro_Zn_Climate_External,  plot.it = TRUE)
#training
nrmse(KhavrRF_Micro_Zn_Climate_Internal, RF_Micro_Scenario_models_dataset$Zn[Training_RF_Micro_scenario])
MAPE(KhavrRF_Micro_Zn_Climate_Internal, RF_Micro_Scenario_models_dataset$Zn[Training_RF_Micro_scenario])

##testing
nrmse(KhavrRF_Micro_Zn_Climate_External, RF_Micro_Scenario_models_dataset$Zn[-Training_RF_Micro_scenario])
MAPE(KhavrRF_Micro_Zn_Climate_External, RF_Micro_Scenario_models_dataset$Zn[-Training_RF_Micro_scenario])

###Soil
KhavrRF_Micro_modeltune_Zn_Soil <- train(Zn ~ Clay_khavr + pH_khavr + Sand_khavr, method ="rf", data = RF_Micro_Scenario_models_dataset[Training_RF_Micro_scenario, ], trainControl=ctrl_Micro_rf)
KhavrRF_Micro_modeltune_Zn_Soil
plot(KhavrRF_Micro_modeltune_Mn_Soil)

KhavrRF_Micro_Zn_Soil <- randomForest(Zn ~ Clay_khavr + pH_khavr + Sand_khavr, data = RF_Micro_Scenario_models_dataset[Training_RF_Micro_scenario, ], mtry = 2, ntree = 500)
KhavrRF_Micro_Zn_Soil

# Internal validation
KhavrRF_Micro_Zn_Soil_Internal <- predict(KhavrRF_Micro_Zn_Soil, newdata = RF_Micro_Scenario_models_dataset[Training_RF_Micro_scenario, ])

goof(observed = RF_Micro_Scenario_models_dataset$Zn[Training_RF_Micro_scenario], predicted = KhavrRF_Micro_Zn_Soil_Internal ,  plot.it = TRUE)
# externalvalidation
KhavrRF_Micro_Zn_Soil_External <- predict(KhavrRF_Micro_Zn_Soil, newdata = RF_Micro_Scenario_models_dataset[-Training_RF_Micro_scenario, ])

goof(observed =  RF_Micro_Scenario_models_dataset$Zn[-Training_RF_Micro_scenario], predicted = KhavrRF_Micro_Zn_Soil_External,  plot.it = TRUE)
#training
nrmse(KhavrRF_Micro_Zn_Soil_Internal, RF_Micro_Scenario_models_dataset$Zn[Training_RF_Micro_scenario])
MAPE(KhavrRF_Micro_Zn_Soil_Internal, RF_Micro_Scenario_models_dataset$Zn[Training_RF_Micro_scenario])

##testing
nrmse(KhavrRF_Micro_Zn_Soil_External, RF_Micro_Scenario_models_dataset$Zn[-Training_RF_Micro_scenario])
MAPE(KhavrRF_Micro_Zn_Soil_External, RF_Micro_Scenario_models_dataset$Zn[-Training_RF_Micro_scenario])

#####Recursive Feature Elimination
KhavrRF_Micro_modeltune_Zn_Rec_Fea_Elimination <- train(Zn ~ Clay_khavr + Sand_khavr, method ="rf", data = RF_Micro_Scenario_models_dataset[Training_RF_Micro_scenario, ], trainControl=ctrl_Micro_rf)

KhavrRF_Micro_modeltune_Zn_Rec_Fea_Elimination
plot(KhavrRF_Micro_modeltune_Mn_Rec_Fea_Elimination)


KhavrRF_Micro_Zn_Rec_Fea_Elimination <- randomForest(Zn ~ Clay_khavr + Sand_khavr, data = RF_Micro_Scenario_models_dataset[Training_RF_Micro_scenario, ], mtry = 2, ntree = 500)
KhavrRF_Micro_Zn_Rec_Fea_Elimination
# Internal validation
KhavrRF_Micro_Zn_Rec_Fea_Elimination_Internal <- predict(KhavrRF_Micro_Zn_Rec_Fea_Elimination, newdata = RF_Micro_Scenario_models_dataset[Training_RF_Micro_scenario, ])

goof(observed = RF_Micro_Scenario_models_dataset$Zn[Training_RF_Micro_scenario], predicted = KhavrRF_Micro_Zn_Rec_Fea_Elimination_Internal ,  plot.it = TRUE)
# externalvalidation
KhavrRF_Micro_Zn_Rec_Fea_Elimination_External <- predict(KhavrRF_Micro_Zn_Rec_Fea_Elimination, newdata = RF_Micro_Scenario_models_dataset[-Training_RF_Micro_scenario, ])

goof(observed =  RF_Micro_Scenario_models_dataset$Zn[-Training_RF_Micro_scenario], predicted = KhavrRF_Micro_Zn_Rec_Fea_Elimination_External,  plot.it = TRUE)
#training
nrmse(KhavrRF_Micro_Zn_Rec_Fea_Elimination_Internal, RF_Micro_Scenario_models_dataset$Zn[Training_RF_Micro_scenario])
MAPE(KhavrRF_Micro_Zn_Rec_Fea_Elimination_Internal, RF_Micro_Scenario_models_dataset$Zn[Training_RF_Micro_scenario])

##testing
nrmse(KhavrRF_Micro_Zn_Rec_Fea_Elimination_External, RF_Micro_Scenario_models_dataset$Zn[-Training_RF_Micro_scenario])
MAPE(KhavrRF_Micro_Zn_Rec_Fea_Elimination_External, RF_Micro_Scenario_models_dataset$Zn[-Training_RF_Micro_scenario])


#################EXPERT OPINION AND VIF ############################

###eXPERT OPINION AND vif COVARIATE analysis
KhavrRF_Micro_modeltune_Zn_expert <- train(Zn ~ b2_reflectance + b5_reflectance  + ci_mean_kh +  
                                             Sen_B08_Mean_30m_aoi + GNDVI_Mean_Sentinel_khavr +  Saturation_Mean_Sentinel_khavr +   
                                             FlowAcc_kh + PlanCur_kh + ProflCur_kh + slope_kh + spi_kh + Twi_kh + aoi_dem_clip_khavr + 
                                             srad_khavr + bio_15_khavr + bio_12_khavr + bio_1_khavr, method ="rf", data = RF_Micro_Scenario_models_dataset, trainControl=ctrl_Micro_rf)

KhavrRF_Micro_modeltune_Zn_expert
plot(KhavrRF_Micro_modeltune_Zn_expert)

KhavrRF_Micro_Zn_expert <- randomForest(Zn ~ b5_reflectance  + ci_mean_kh +  
                                          Sen_B08_Mean_30m_aoi + GNDVI_Mean_Sentinel_khavr +  Saturation_Mean_Sentinel_khavr +   
                                          FlowAcc_kh + PlanCur_kh + ProflCur_kh + slope_kh + spi_kh + Twi_kh + aoi_dem_clip_khavr + 
                                          srad_khavr + bio_15_khavr + bio_12_khavr + bio_1_khavr
                                        , method ="rf", data = RF_Micro_Scenario_models_dataset[Training_RF_Micro_scenario, ], mtry = 2, ntree = 250, importance =TRUE)

KhavrRF_Micro_Zn_expert
plot(KhavrRF_Micro_Zn_expert)
par(mfrow = c(2,1))
# Internal validation
KhavrRF_Micro_Zn_expert_Internal <- predict(KhavrRF_Micro_Zn_expert, newdata = RF_Micro_Scenario_models_dataset[Training_RF_Micro_scenario, ])
KhavrRF_Micro_Zn_expert_Internal
goof(observed = RF_Micro_Scenario_models_dataset$Zn[Training_RF_Micro_scenario], predicted = KhavrRF_Micro_Zn_expert_Internal ,  plot.it = TRUE)
# externalvalidation
KhavrRF_Micro_Zn_expert_External <- predict(KhavrRF_Micro_Zn_expert, newdata = RF_Micro_Scenario_models_dataset[-Training_RF_Micro_scenario, ])

goof(observed =  RF_Micro_Scenario_models_dataset$Zn[-Training_RF_Micro_scenario], predicted = KhavrRF_Micro_Zn_expert_External,  plot.it = TRUE)
#training
nrmse(KhavrRF_Micro_Zn_expert_Internal, RF_Micro_Scenario_models_dataset$Zn[Training_RF_Micro_scenario])
MAPE(KhavrRF_Micro_Zn_expert_Internal, RF_Micro_Scenario_models_dataset$Zn[Training_RF_Micro_scenario])

##testing
nrmse(KhavrRF_Micro_Zn_expert_External, RF_Micro_Scenario_models_dataset$Zn[-Training_RF_Micro_scenario])
MAPE(KhavrRF_Micro_Zn_expert_External, RF_Micro_Scenario_models_dataset$Zn[-Training_RF_Micro_scenario])






#########importance###
varImpPlot(KhavrRF_Micro_Zn_expert)
KHAVR_RF_Zn_Expert_Op_IMP <- importance(KhavrRF_Micro_Zn_expert)
KHAVR_RF_Zn_Expert_Op_IMP
KHAVR_RF_Zn_Expert_Op_IMP <- as.data.frame(KHAVR_RF_Zn_Expert_Op_IMP)
write.table(KHAVR_RF_Zn_Expert_Op_IMP, "KHAVR_RF_Zn_Expert_Op_IMP.txt", sep = ",")

############################MAPPING###############


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



##mappingsandkhavr_randomforest
Expert_Op_Micro_Zn_RF <- predict(COVS_Expert_Op_Micro, KhavrRF_Micro_Zn_expert, "Expert_Op_Micro_Zn_RF.tif",
                                 format = "GTiff", datatype = "FLT4S", overwrite = TRUE)
par(mfrow = c(1,1))

plot(Expert_Op_Micro_Zn_RF,
     main = "Khavr Random Forest model 
     predicted 0-30 cm Zn (ppm)")
####Taylor and Hexbin graph
# internalvalidation
KhavrRF_Micro_Zn_expert_Internal_Pred <- predict(KhavrRF_Micro_Zn_expert, newdata = RF_Micro_Scenario_models_dataset[Training_RF_Micro_scenario, ])
View(KhavrRF_Micro_Zn_expert_Internal_Pred)

KhavrRF_Micro_Zn_expert_Internal_Obs <- subset(RF_Micro_Scenario_models_dataset[Training_RF_Micro_scenario, ], select= c(Zn))
View(KhavrRF_Micro_Zn_expert_Internal_Obs)

###Birlestir
KhavrRF_Micro_Zn_expert_column_bind_Calibration <- cbind(KhavrRF_Micro_Zn_expert_Internal_Obs, KhavrRF_Micro_Zn_expert_Internal_Pred)
View(KhavrRF_Micro_Zn_expert_column_bind_Calibration)

colnames(KhavrRF_Micro_Zn_expert_column_bind_Calibration) <- c("Observed", "Predicted")
View(KhavrRF_Micro_Zn_expert_column_bind_Calibration)
write.table(KhavrRF_Micro_Zn_expert_column_bind_Calibration, "KhavrRF_Micro_Zn_expert_Internal_Observed_Predicted.txt", sep = ",")


#####scatterPlot(KhavrRF_Micro_Fe_expert_column_bind_Calibration, x ="Observed" , y = "Predicted",xlab=10, ylab=10,method = "hexbin",mod.line=T,auto.text=F, col = "jet", xbin = 30)








# externalvalidation
KhavrRF_Micro_Zn_expert_External_Pred <- predict(KhavrRF_Micro_Zn_expert, newdata = RF_Micro_Scenario_models_dataset[-Training_RF_Micro_scenario, ])
View(KhavrRF_Micro_Zn_expert_External_Pred)

KhavrRF_Micro_Zn_expert_External_Obs <- subset(RF_Micro_Scenario_models_dataset[-Training_RF_Micro_scenario, ], select= c(Zn))
View(KhavrRF_Micro_Zn_expert_External_Obs)

###Birlestir
KhavrRF_Micro_Zn_expert_column_bind_Validation <- cbind(KhavrRF_Micro_Zn_expert_External_Obs, KhavrRF_Micro_Zn_expert_External_Pred)
View(KhavrRF_Micro_Zn_expert_column_bind_Validation)

colnames(KhavrRF_Micro_Zn_expert_column_bind_Validation) <- c("Observed", "Predicted")
View(KhavrRF_Micro_Zn_expert_column_bind_Validation)
write.table(KhavrRF_Micro_Zn_expert_column_bind_Validation, "KhavrRF_Micro_Zn_expert_Validation_Observed_Predicted.txt", sep = ",")






