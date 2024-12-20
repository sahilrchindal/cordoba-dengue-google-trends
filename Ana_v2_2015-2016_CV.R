#######------- Clear Workspace -------#######
rm(list=ls())

#######------- Set Working Directory -------#######
setwd("G:/My Drive/Classes/Research/Human Movement/Google/Data/Yearly/2015-2016")

#######------- Load Packages -------#######
library(gtrendsR)
library(tidyverse)
library(caret)
library(leaps)
library(MASS)
library(ggplot2)
library(glmnet)
library(grpreg)
library(broom)
library(lubridate)
library(kernlab)
library(plyr)
library(reshape2)
library(GGally)
library(rgl)
library(dplyr)
library(rpart)
library(rpart.plot)
library(ISLR)
library(pls)
library(sandwich)
library(msm)
library(randomForest)
library(e1071)

#######------- Load Data -------#######
linreg <- read.csv("interpolation_mosquito_2015-2016.csv")
options(scipen=100, digits=4)

#######------- Modify Regressors -------#######
x2 <- model.matrix(Y~X1+X2+X3+X4+X5+X6+X11+X13+X17+X18+X19+X20+X22+X23+X24+X25
                   +X28+X29+X30+X31+X32+X33, linreg)[,-1]
x <- as.data.frame(x2)
y <- linreg$Y
i=0
for (i in length(x)) {
  x[x==0] <- 0.01
}
j=0
for (j in length(y)) {
  y[y==0] <- 0.001
}
df <- data.frame(y,x)
df2 <- data.frame(x)


#######------- Export Modified Regressors -------#######
write.csv(df, "interpolation_mod_mosquito_2015-2016.csv", row.names=FALSE)

#######------- Account for lag -------#######

k <- ncol(df2)
ccf_mat <- matrix(0, nrow=13, ncol=k)
corr_mat <- matrix(0, nrow=13, ncol=22)
i <- nrow(corr_mat)
j <- ncol(corr_mat)
palue <- 2*(1-pnorm(abs(corr_mat[i,j]), mean=0, sd=1/sqrt(52)))
p_mat_out <- matrix(0, nrow=13, ncol=22)
for(m in 1:k) {
  ccf_mat <- ccf(x=df2[,m], y=y, lag.max=6, plot=TRUE)
  corr_mat[,m] <- ccf_mat$acf
  p_mat_out[,m] <- 2*(1-pnorm(abs(corr_mat[,m]), mean=0, sd=1/sqrt(52)))
}

corr_mat2 <- corr_mat[1:7,]
p_mat_out2 <- p_mat_out[1:7,]
alpha <- 0.1
filtered_cor_mat <- corr_mat2
filtered_cor_mat[p_mat_out2 > alpha] <- NA

write.csv(corr_mat2, "corr_mat2_v2_2015-2016_CV.csv", row.names=FALSE)
min_p_val_loc <- apply(p_mat_out2, 2, which.min)
min_p_val_loc
min_p_val <- apply(p_mat_out2, 2, min)
min_p_val
corr_mins = numeric(length=ncol(corr_mat2))
for(j in 1:ncol(corr_mat2)){
  corr_mins[j] <- corr_mat2[p_mat_out2[,j]==min_p_val[j],j]
}
corr_mins

# Lag0
linreg_X <- linreg[,4:33]
t <- length(linreg$Y)
subsets.1 <- list()
for (n in 1) {
  subset1.1 <- linreg$date[1:t]
  subset2.1 <- linreg_X[1:t,]
  subset3.1 <- linreg$Y[n:t]
  subset4.1 <- list(subset1=subset1.1, subset2=subset2.1, subset3=subset3.1)
  subsets.1[[n]] <- subset4.1
}
lag0_date <- subsets.1[[1]][["subset1"]]
lag0_Y <- subsets.1[[1]][["subset3"]]
lag0_X_all <- subsets.1[[1]][["subset2"]]
lag0 <- data.frame(date=lag0_date, Y=lag0_Y, lag0_X_all)

# Lag
subsets <- list()
for (n in 1:6) {
  subset1 <- linreg$date[1:(t-n)]
  subset2 <- linreg_X[1:(t-n),]
  subset3 <- linreg$Y[(n+1):t]
  subset4 <- list(subset1=subset1, subset2=subset2, subset3=subset3)
  subsets[[n]] <- subset4
}

lag1_date <- subsets[[1]][["subset1"]]
lag1_Y <- subsets[[1]][["subset3"]]
lag1_X_all <- subsets[[1]][["subset2"]]
lag1 <- data.frame(date=lag1_date, Y=lag1_Y, lag1_X_all)

lag2_date <- subsets[[2]][["subset1"]]
lag2_Y <- subsets[[2]][["subset3"]]
lag2_X_all <- subsets[[2]][["subset2"]]
lag2 <- data.frame(date=lag2_date, Y=lag2_Y, lag2_X_all)

lag3_date <- subsets[[3]][["subset1"]]
lag3_Y <- subsets[[3]][["subset3"]]
lag3_X_all <- subsets[[3]][["subset2"]]
lag3 <- data.frame(date=lag3_date, Y=lag3_Y, lag3_X_all)

lag4_date <- subsets[[4]][["subset1"]]
lag4_Y <- subsets[[4]][["subset3"]]
lag4_X_all <- subsets[[4]][["subset2"]]
lag4 <- data.frame(date=lag4_date, Y=lag4_Y, lag4_X_all)

lag5_date <- subsets[[5]][["subset1"]]
lag5_Y <- subsets[[5]][["subset3"]]
lag5_X_all <- subsets[[5]][["subset2"]]
lag5 <- data.frame(date=lag5_date, Y=lag5_Y, lag5_X_all)

lag6_date <- subsets[[6]][["subset1"]]
lag6_Y <- subsets[[6]][["subset3"]]
lag6_X_all <- subsets[[6]][["subset2"]]
lag6 <- data.frame(date=lag6_date, Y=lag6_Y, lag6_X_all)


#######------- Lag Input -------#######
data <- lag3 # change this for different lags
my_model <- data
# x7,x8,x9,x12,x14,x15,x16,x21,x26 have all 0s, so omitting these predictors below
my_model2 <- my_model[-c(9,10,11,13,15,16,17,27)]
my_model2 <- my_model2[2:24]

# 5-fold CV
set.seed(123)
s <- length(my_model2$Y)
train.control <- trainControl(method="CV", number=5)
X_train <- my_model2[,2:23]
y_train <- my_model2[,1]
a <- 1





#######------- Collinearity Diagnostics -------#######
library(car)
library(corrplot)
library(regclass)

corr_matrix <- cor(my_model2[,-c(1)])
rownames(corr_matrix)  <- colnames(corr_matrix) <-
  c("aedes","aedes aegypti","aegypti","vomit","mosquito","dengue virus",
    "mosquito dengue","fever","disease","pain","rash","headache","nausea",
    "virus del dengue","síntomas del dengue","mosquitos","zancudo","erupción",
    "dolor de cabeza","náuseas","vomitar","vómitos")
corrplot(corr_matrix, method="color", order="FPC", type="lower")

mylm2 <- lm(Y~., data=my_model2)
vif_values <- VIF(mylm2)
vif_values

eigens <- eigen(corr_matrix)
eigenvalues <- eigens$values
condition_indices <- sqrt(max(eigenvalues) / eigenvalues)
condition_indices




#######------- Linear Model -------#######
set.seed(123)
mylm <- train(Y~., data=my_model2, method="lm", trControl=train.control)
mylm

predicted_values <- predict(mylm, newdata=X_train)

plot(y_train, main="Predicted vs. Observed Values", xlim=c(0,52), ylim=c(-25,135), 
     xlab="Observed Values", ylab="Dengue Cases", pch=19)
lines(predicted_values)

summary(mylm$finalModel) # Significant Predictors

# coefficients <- coef(mylm)
# significant_predictors <- names(coefficients[abs(coefficients)>2])
# print(significant_predictors)
# var_importance <- varImp(mylm, scale=FALSE)
# significant_predictors <- rownames(var_importance[var_importance$Overall>0.1,])







#######------- Subset Model -------#######

# Forward Selection
set.seed(123)
smylm1 <- train(Y~., data=my_model2, method="leapForward", trControl=train.control)
smylm1
summary(smylm1$finalModel)

smylm1.1 <- train(Y~X5+X21, data=my_model2, method="lm", trControl=train.control)
summary(smylm1.1)

# Backward Selection
set.seed(123)
smylm2 <- train(Y~., data=my_model2, method="leapBackward", trControl=train.control)
smylm2
summary(smylm2$finalModel)

smylm2.1 <- train(Y~X2+X23, data=my_model2, method="lm", trControl=train.control)
summary(smylm2.1)

# Stepwise Selection
set.seed(123)
smylm3 <- train(Y~., data=my_model2, method="leapSeq", trControl=train.control)
smylm3
summary(smylm3$finalModel)

smylm3.1 <- train(Y~X5+X21, data=my_model2, method="lm", trControl=train.control)
summary(smylm3.1)





#######------- PCR -------#######
set.seed(123)
mypcr <- train(Y~., data=my_model2, method="pcr", trControl=train.control)
mypcr
pcr_k <- mypcr$finalModel$ncomp

pcr_model2 <- train(Y~., data=my_model2, method="pcr", 
                    tuneGrid = expand.grid(ncomp=pcr_k), trControl=train.control)
pcr_model2
pcr_model2.1 <- pcr_model2$finalModel
pcr_pcs <- pcr_model2.1$loadings[,1:pcr_k]
pcr_pcs
pcr_pcs2 <- pcr_pcs**2
pcr_pcs2
pcr_indices2 <- order(-pcr_pcs2)
pcr_sorted_mat2 <- pcr_pcs2[pcr_indices2]
dim(pcr_sorted_mat2) <- dim(pcr_pcs2)
pcr_sorted_mat2 # PCs in descending order
# plot(pcr_sorted_mat2)

# sum(pcr_sorted_mat2[,1])
# sum(pcr_sorted_mat2[,2])

pcr_rmse_values <- pcr_model2$results$RMSE
pcr_rmse_values

plot(mypcr)
plot(pcr_model2.1)



#######------- PLS -------#######
set.seed(123)
mypls <- train(Y~., data=my_model2, method="pls", trControl=train.control)
mypls
pls_k <- mypls$finalModel$ncomp

pls_model2 <- train(Y~., data=my_model2, method="pls", 
                    tuneGrid = expand.grid(ncomp=pls_k), trControl=train.control)
pls_model2.1 <- pls_model2$finalModel
pls_pcs <- pls_model2.1$loadings[,1:pls_k]
pls_pcs
pls_pcs2 <- pls_pcs**2
pls_pcs2
pls_indices2 <- order(-pls_pcs2)
pls_sorted_mat2 <- pls_pcs2[pls_indices2]
dim(pls_sorted_mat2) <- dim(pls_pcs2)
pls_sorted_mat2 # PCs in descending order

pls_rmse_values <- pls_model2$results$RMSE
pls_rmse_values





#######------- Random Forest -------#######
set.seed(123)
mtryGrid <- expand.grid(mtry=2)
myrf <- train(Y~., data=my_model2, method="rf", 
              trControl=train.control, tuneGrid=mtryGrid)
myrf

var_imp <- varImp(myrf)
var_imp
plot(var_imp, xlab="Importance", ylab="Variables")

rf_var_imp <- varImp(myrf$finalModel)
rf_var_imp
barplot(rf_var_imp$Overall, xlab="Importance", ylab="Variables")

rf_sorted_indices2 <- order(-rf_var_imp[,1])
rf_sorted_mat2 <- rf_var_imp[rf_sorted_indices2, ]
rf_sorted_mat2

