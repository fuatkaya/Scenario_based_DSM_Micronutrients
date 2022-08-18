
###########



KhavrRF_Micro_Fe_expert_Internal_column_merge <-read.table(file.choose(), header =T, sep = ",")
View(KhavrRF_Micro_Fe_expert_Internal_column_merge)



KhavrRF_Micro_Fe_expert_Internal_column_merge <- as.data.frame(KhavrRF_Micro_Fe_expert_Internal_column_merge)

library(openair)
library(hexbin)
scatterPlot(Cu_SVR_RF_Training, x ="Observed" , y = "Predicted", group = "Model", xlab=10, ylab=10,method = "hexbin",mod.line=T, auto.text=F, col = "jet", xbin = 30)

par(mfrow= c(2,1))
####§CU_RF_SVR_Training
Cu_SVR_RF_Training <-read.csv(file.choose(), header =T, sep = ";")
Cu_SVR_RF_Training <- as.data.frame(Cu_SVR_RF_Training)
View(Cu_SVR_RF_Training)
str(Cu_SVR_RF_Training)
####Taylor_deneme$Model <- as.factor(Taylor_deneme$Model)
TaylorDiagram(Cu_SVR_RF_Training, obs = "Observed", mod = "Predicted", group = "Model", cols = "PiYG", cor.col = "#386cb0", rms.col ="#b2182b")


####§CU_RF_SVR_Trainingandvalidationentegrated
Cu_SVR_RF_Trainingandvalidation <-read.csv(file.choose(), header =T, sep = ";")
Cu_SVR_RF_Trainingandvalidation <- as.data.frame(Cu_SVR_RF_Trainingandvalidation)
View(Cu_SVR_RF_Trainingandvalidation)
str(Cu_SVR_RF_Trainingandvalidation)
####Taylor_deneme$Model <- as.factor(Taylor_deneme$Model)
TaylorDiagram(Cu_SVR_RF_Trainingandvalidation, obs = "Observed", mod = "Predicted", group = "Model", cols = "Spectral", cor.col = "#386cb0", rms.col ="#b2182b")


####§Fe_RF_SVR_Trainingandvalidationentegrated
Fe_SVR_RF_Trainingandvalidation <-read.csv(file.choose(), header =T, sep = ";")
Fe_SVR_RF_Trainingandvalidation <- as.data.frame(Fe_SVR_RF_Trainingandvalidation)
View(Fe_SVR_RF_Trainingandvalidation)
str(Fe_SVR_RF_Trainingandvalidation)
####Taylor_deneme$Model <- as.factor(Taylor_deneme$Model)
TaylorDiagram(Fe_SVR_RF_Trainingandvalidation, obs = "Observed", mod = "Predicted", group = "Model", cols = "RdYlBu", cor.col = "#386cb0", rms.col ="#b2182b")



####§Mn_RF_SVR_Trainingandvalidationentegrated
Mn_SVR_RF_Trainingandvalidation <-read.csv(file.choose(), header =T, sep = ";")
Mn_SVR_RF_Trainingandvalidation <- as.data.frame(Mn_SVR_RF_Trainingandvalidation)
View(Mn_SVR_RF_Trainingandvalidation)
str(Mn_SVR_RF_Trainingandvalidation)
####Taylor_deneme$Model <- as.factor(Taylor_deneme$Model)
TaylorDiagram(Mn_SVR_RF_Trainingandvalidation, obs = "Observed", mod = "Predicted", group = "Model", cols = "Set3", cor.col = "#386cb0", rms.col ="#b2182b")


####§Zn_RF_SVR_Trainingandvalidationentegrated
Zn_SVR_RF_Trainingandvalidation <-read.csv(file.choose(), header =T, sep = ";")
Zn_SVR_RF_Trainingandvalidation <- as.data.frame(Zn_SVR_RF_Trainingandvalidation)
View(Zn_SVR_RF_Trainingandvalidation)
str(Zn_SVR_RF_Trainingandvalidation)
####Taylor_deneme$Model <- as.factor(Taylor_deneme$Model)
TaylorDiagram(Zn_SVR_RF_Trainingandvalidation, obs = "Observed", mod = "Predicted", group = "Model", cols = "Paired", cor.col = "#386cb0", rms.col ="#b2182b")
