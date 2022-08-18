library(randomForest)
library(ithir)
library(MASS)
library(caret)
library(ggplot2)
library(rasterVis)
library(lattice)
library(latticeExtra)

RF_Khavr_Micro_rfe <-read.csv(file.choose(),header =T, sep = ",") 
set.seed(2021)
str(RF_Khavr_Micro_rfe)
par(mfrow = c(4,2))
###Non-Transformed
hist(RF_Khavr_Micro_rfe$Fe,  xlab = "Fe - Nontransformed")
qqnorm(RF_Khavr_Micro_rfe$Fe, plot.it = TRUE, pch = 4, cex =  0.7, xlab = "Fe - Nontransformed")
qqline(RF_Khavr_Micro_rfe$Fe, col = "blue", lwd = 2)
###Mn
hist(RF_Khavr_Micro_rfe$Mn,  xlab = "Mn - Nontransformed")
qqnorm(RF_Khavr_Micro_rfe$Mn, plot.it = TRUE, pch = 4, cex =  0.7, xlab = "Mn - Nontransformed")
qqline(RF_Khavr_Micro_rfe$Mn, col = "blue", lwd = 2)


##Zn
hist(RF_Khavr_Micro_rfe$Zn,  xlab = "Zn - Nontransformed")
qqnorm(RF_Khavr_Micro_rfe$Zn, plot.it = TRUE, pch = 4, cex =  0.7, xlab = "Zn - Nontransformed")
qqline(RF_Khavr_Micro_rfe$Zn, col = "blue", lwd = 2)

#Cu
hist(RF_Khavr_Micro_rfe$Cu,  xlab = "Cu - Nontransformed")
qqnorm(RF_Khavr_Micro_rfe$Cu, plot.it = TRUE, pch = 4, cex =  0.7, xlab = "Cu - Nontransformed")
qqline(RF_Khavr_Micro_rfe$Cu, col = "blue", lwd = 2)


###Sqrt-Transformed
##SQRTFe
RF_Khavr_Micro_rfe$sqrt_Fe <-sqrt(RF_Khavr_Micro_rfe$Fe)
hist(RF_Khavr_Micro_rfe$sqrt_Fe,  xlab = "Sqrt Fe- Transformed")
qqnorm(RF_Khavr_Micro_rfe$sqrt_Fe, plot.it = TRUE, pch = 4, cex =  0.7, xlab = "Sqrt Fe- Transformed")
qqline(RF_Khavr_Micro_rfe$sqrt_Fe, col = "blue", lwd = 2)
####SQRTMn
RF_Khavr_Micro_rfe$sqrt_Mn <-sqrt(RF_Khavr_Micro_rfe$Mn)
hist(RF_Khavr_Micro_rfe$sqrt_Mn,  xlab = "Sqrt Mn- Transformed")
qqnorm(RF_Khavr_Micro_rfe$sqrt_Mn, plot.it = TRUE, pch = 4, cex =  0.7, xlab = "Sqrt Mn- Transformed")
qqline(RF_Khavr_Micro_rfe$sqrt_Mn, col = "blue", lwd = 2)
###SQRTZn
RF_Khavr_Micro_rfe$sqrt_Zn <-sqrt(RF_Khavr_Micro_rfe$Zn)
hist(RF_Khavr_Micro_rfe$sqrt_Zn,  xlab = "Sqrt Zn- Transformed")
qqnorm(RF_Khavr_Micro_rfe$sqrt_Zn, plot.it = TRUE, pch = 4, cex =  0.7, xlab = "Sqrt Zn- Transformed")
qqline(RF_Khavr_Micro_rfe$sqrt_Zn, col = "blue", lwd = 2)
##SQRTCu
RF_Khavr_Micro_rfe$sqrt_Cu <-sqrt(RF_Khavr_Micro_rfe$Cu)
hist(RF_Khavr_Micro_rfe$sqrt_Cu,  xlab = "Sqrt Cu- Transformed")
qqnorm(RF_Khavr_Micro_rfe$sqrt_Cu, plot.it = TRUE, pch = 4, cex =  0.7, xlab = "Sqrt Cu- Transformed")
qqline(RF_Khavr_Micro_rfe$sqrt_Cu, col = "blue", lwd = 2)

##Log_transformed
##LogFe
RF_Khavr_Micro_rfe$Log_Fe <-log(RF_Khavr_Micro_rfe$Fe)
hist(RF_Khavr_Micro_rfe$Log_Fe,  xlab = "Log Fe- Transformed")
qqnorm(RF_Khavr_Micro_rfe$Log_Fe, plot.it = TRUE, pch = 4, cex =  0.7, xlab = "Log Fe- Transformed")
qqline(RF_Khavr_Micro_rfe$Log_Fe, col = "blue", lwd = 2)
####LogMn
RF_Khavr_Micro_rfe$Log_Mn <-log(RF_Khavr_Micro_rfe$Mn)
hist(RF_Khavr_Micro_rfe$Log_Mn,  xlab = "Log Mn- Transformed")
qqnorm(RF_Khavr_Micro_rfe$Log_Mn, plot.it = TRUE, pch = 4, cex =  0.7, xlab = "Log Mn- Transformed")
qqline(RF_Khavr_Micro_rfe$Log_Mn, col = "blue", lwd = 2)
###LogZn
RF_Khavr_Micro_rfe$Log_Zn <-log(RF_Khavr_Micro_rfe$Zn)
hist(RF_Khavr_Micro_rfe$Log_Zn,  xlab = "Log Zn- Transformed")
qqnorm(RF_Khavr_Micro_rfe$Log_Zn, plot.it = TRUE, pch = 4, cex =  0.7, xlab = "Log Zn- Transformed")
qqline(RF_Khavr_Micro_rfe$Log_Zn, col = "blue", lwd = 2)
##LogCu
RF_Khavr_Micro_rfe$Log_Cu <-log(RF_Khavr_Micro_rfe$Cu)
hist(RF_Khavr_Micro_rfe$Log_Cu,  xlab = "Log Cu- Transformed")
qqnorm(RF_Khavr_Micro_rfe$Log_Cu, plot.it = TRUE, pch = 4, cex =  0.7, xlab = "Log Cu- Transformed")
qqline(RF_Khavr_Micro_rfe$Log_Cu, col = "blue", lwd = 2)
View(RF_Khavr_Micro_rfe)
str(RF_Khavr_Micro_rfe)
#RecusiveFE_Fe
###RF_Khavr_P_rfe <- subset(RF_Khavr_P_rfe, select= -c(sqrt_Fe, ))
View(RF_Khavr_Micro_rfe)
RF_Khavr_P_rfe <-  as.data.frame(RF_Khavr_Micro_rfe)
View(RF_Khavr_Micro_rfe)
str(RF_Khavr_Micro_rfe)
summary(RF_Khavr_Micro_rfe)


###########recursive feature elimination_Random_forest_Fe-Demir
x <- subset(RF_Khavr_Micro_rfe, select= -c(Fe, Mn, Zn, Cu))
y <- RF_Khavr_Micro_rfe$Cu 
set.seed(2021)
#############method: repeatedcv, LOOCV or LGOCV
####repeatedcv
train_RF_Khavr_RecusiveFE_Cu <- createDataPartition(y, p = .70, list = FALSE)[,1]
View(train_RF_Khavr_RecusiveFE_Cu)
x_train <- x[train_RF_Khavr_RecusiveFE_Cu, ]
x_test  <- x[-train_RF_Khavr_RecusiveFE_Cu, ]


y_train <- y[ train_RF_Khavr_RecusiveFE_Cu]
y_test  <- y[-train_RF_Khavr_RecusiveFE_Cu]

###Repeated_CV
RF_Khavr_micro_control <- rfeControl(functions = rfFuncs,
                                        method ="repeatedcv",
                                        repeats = 3,
                                        number = 5)
modelLookup("svmRadial")
result_rfe1 <- rfe(x = x_test, 
                   y = y_test, 
                   sizes = c(1:41),
                   rfeControl = RF_Khavr_micro_control)
result_rfe1
predictors(result_rfe1)

library(ggplot2)
ggplot(data = result_rfe1, metric = "RMSE") + theme_bw()

ggplot(data = result_rfe1, metric = "Rsquared") + theme_bw()

###LOOCV
RF_Khavr_micro_control <- rfeControl(functions = rfFuncs,
                                     method ="LOOCV")

result_rfe1 <- rfe(x = x_test, 
                   y = y_test, 
                   sizes = c(1:41),
                   rfeControl = RF_Khavr_micro_control)
result_rfe1
predictors(result_rfe1)

library(ggplot2)
ggplot(data = result_rfe1, metric = "RMSE") + theme_bw()

ggplot(data = result_rfe1, metric = "Rsquared") + theme_bw()


###LGOCV
RF_Khavr_micro_control <- rfeControl(functions = rfFuncs,
                                     method ="LGOCV")

result_rfe1 <- rfe(x = x_test, 
                   y = y_test, 
                   sizes = c(1:41),
                   rfeControl = RF_Khavr_micro_control)
result_rfe1
predictors(result_rfe1)

library(ggplot2)
ggplot(data = result_rfe1, metric = "RMSE") + theme_bw()

ggplot(data = result_rfe1, metric = "Rsquared") + theme_bw()

#################SVR_RFE
###########recursive feature elimination_Random_forest_Fe-Demir
x <- subset(RF_Khavr_Micro_rfe, select= -c(Mn, Zn, Cu))
y <- RF_Khavr_Micro_rfe$Fe 
set.seed(2021)
#############method: repeatedcv, LOOCV or LGOCV
####repeatedcv
trainingSVR <- sample(nrow(RF_Khavr_Micro_rfe), 0.7 * nrow(RF_Khavr_Micro_rfe))

###Repeated_CV
ctrl_Fe_SVR<- trainControl(method= "repeatedcv", number = 5, repeats = 3, search = "random")
khavrSVR_Fe_modeltune <- train(Fe  ~ .,  method ="svmRadial", data = x[trainingSVR, ], trainControl=ctrl_Fe_SVR)
khavrSVR_Fe_modeltune

##LOOCV
ctrl_Fe_SVR<- trainControl(method= "LOOCV")
khavrSVR_Fe_modeltune <- train(Fe  ~ .,  method ="svmRadial", data = x[trainingSVR, ], trainControl=ctrl_Fe_SVR)
khavrSVR_Fe_modeltune
packageVersion('caret')


###LGOCV
ctrl_Fe_SVR<- trainControl(method= "LGOCV")
khavrSVR_Fe_modeltune <- train(Fe  ~ .,  method ="svmRadial", data = x[trainingSVR, ], trainControl=ctrl_Fe_SVR)
khavrSVR_Fe_modeltune
