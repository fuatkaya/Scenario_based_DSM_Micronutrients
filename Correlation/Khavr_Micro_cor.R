####Khavr_micronutrients_corr
Khavr_micronutrients_corrrelation <-read.csv(file.choose(),header =T, sep = "," )
Khavr_micronutrients_corrrelation <- as.data.frame(Khavr_micronutrients_corrrelation)
View(Khavr_micronutrients_corrrelation)
str(Khavr_micronutrients_corrrelation)
##sütun isimlerinin degistirme
library(corrplot)
library(RColorBrewer)
colnames(Khavr_micronutrients_corrrelation) <- c("Fe", 
                                                 "Mn", 
                                                 "Zn", 
                                                 "Cu",
                                                 "Landsat-B2-Mean",
                                                 "Landsat-B3-Mean",
                                                 "Landsat-B4-Mean",
                                                 "Landsat-B5-Mean",
                                                 "Landsat-B6-Mean",
                                                 "Landsat-B7-Mean",
                                                 "Landsat_Clay_Index_Mean",
                                                 "Flow_Accumulation", 
                                                 "Landsat_GNDVI_Mean", 
                                                 "Landsat_NDVI_Mean",
                                                 "Planform Curvature", 
                                                 "Profile_Curvature", 
                                                 "Landsat_Saturation_Index_Mean",
                                                 "Landsat_TopSGSI_Mean", 
                                                 "Slope", 
                                                 "Stream_Power_Index", 
                                                 "Topographic_Wetness_Index", 
                                                 "Elevation",
                                                 "Sentinel-B2-Mean",
                                                 "Sentinel-B3-Mean",
                                                 "Sentinel-B4-Mean",
                                                 "Sentinel-B5-Mean",
                                                 "Sentinel-B6-Mean",
                                                 "Sentinel-B7-Mean",
                                                 "Sentinel-B8-Mean",
                                                 "Sentinel-B8A-Mean",
                                                 "Sentinel-B11-Mean",
                                                 "Sentinel-B12-Mean",
                                                 "Sentinel_Clay_Index_Mean", 
                                                 "Sentinel_GNDVI_Mean",
                                                 "Sentinel_MSAVI2_Mean",
                                                 "Sentinel_NDVI_Mean", 
                                                 "Sentinel_Saturation_Index_Mean", 
                                                 "Sentinel_TopSGSI_Mean", 
                                                 "Clay", 
                                                 "pH", 
                                                 "Sand",
                                         "Solar_Radiation", 
                                        "Precipitation_Seasonality_Bio_15",
                                        "Annual_Precipitation_Bio_12", 
                                        "Annual_Mean_Temperature_Bio_1")
View(Khavr_micronutrients_corrrelation)
library(corrplot)
CorrelationKhavr_micronutrients_corrrelation <- cor(Khavr_micronutrients_corrrelation, method ="spearman")
CorrelationKhavr_micronutrients_corrrelationRESULTS_Spearman <- as.data.frame(CorrelationKhavr_micronutrients_corrrelation)
write.csv(CorrelationKhavr_micronutrients_corrrelationRESULTS, "Khavr_Micro_corrRESULTS_spearman.csv", col.names = T, row.names = FALSE, sep = ",")
CorrelationKhavr_micronutrients_corrrelation

View(CorrelationKhavr_micronutrients_corrrelationRESULTS_Spearman)
sigkahvr_micro <- cor.mtest(CorrelationKhavr_micronutrients_corrrelation, conf.level = .95)
write.csv(sigkahvr_micro, "sigkahvr_micro_0.95.csv", col.names = T, row.names = FALSE, sep = ",")
View(sigkahvr_micro)

corrplot(CorrelationKhavr_micronutrients_corrrelation, p.mat = sigkahvr_micro$p, sig.level = .5, type = "upper", tl.col = "black", insig = "blank")
corrplot(CorrelationKhavr_micronutrients_corrrelation, method = "color",
         type = "upper", number.cex = .50,
         addCoef.col = "black", tl.col = "black", tl.srt = 80,
         p.mat = sigkahvr_micro$p,sig.level = 0.05, insig = "blank")
####order = c("AOE", "FPC", "hclust")

