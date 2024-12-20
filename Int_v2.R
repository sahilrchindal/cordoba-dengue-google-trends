#######------- Clear Workspace -------#######
rm(list=ls())

#######------- Set Working Directory -------#######
setwd("G:/My Drive/Classes/Research/Human Movement/Google/Data/Combined/2011-2022")

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
library(dplyr)

#######------- Load Data -------#######
pred <- read.csv("st_ARX_mosquito_com_2013-2016.csv")
pred_2013 <- pred
pred_2015 <- pred
pred_2016 <- pred

# 2012-12-17 to 2016-05-30
start_date <- as.Date("2012-12-17")
end_date <- as.Date("2016-05-30")

pred <- pred[pred[,1]>=start_date & pred[,1]<=end_date, ]


#######------- Predictors -------#######
X1 <- pred$st1_hit1  # aedes
X2 <- pred$st1_hit2  # aedes aegypti
X3 <- pred$st1_hit3  # aegypti
X4 <- pred$st1_hit4  # vomit
X5 <- pred$st1_hit5  # mosquito
X6 <- pred$st2_hit1  # dengue virus
X7 <- pred$st2_hit2  # dengue fever
X8 <- pred$st2_hit3  # dengue hemorrhagic fever
X9 <- pred$st2_hit4  # dengue symptoms
# x10 <- pred$st3_hit1  # mosquito
X11 <- pred$st3_hit2  # mosquito dengue
X12 <- pred$st3_hit3  # mosquitoes
X13 <- pred$st3_hit4  # fever
X14 <- pred$st4_hit1  # new dengue virus
X15 <- pred$st4_hit2  # DENV
X16 <- pred$st4_hit3  # DHF
X17 <- pred$st4_hit4  # disease
X18 <- pred$st5_hit1  # pain
X19 <- pred$st5_hit2  # rash
X20 <- pred$st5_hit3  # headache
X21 <- pred$st5_hit4  # nausea
X22 <- pred$st6_hit1  # virus del dengue
X23 <- pred$st6_hit2  # síntomas del dengue
X24 <- pred$st6_hit3  # mosquitos
X25 <- pred$st6_hit4  # zancudo
X26 <- pred$st7_hit1  # nuevo virus del dengue
# x27 <- pred$st7_hit2  # dolor
X28 <- pred$st7_hit2  # erupción
X29 <- pred$st7_hit3  # dolor de cabeza
X30 <- pred$st8_hit1  # náuseas
X31 <- pred$st8_hit2  # vomitar
X32 <- pred$st8_hit3  # vómitos
X33 <- pred$st8_hit4  # fiebre
# x34 <- pred$st9_hit1  # enfermedad

# Combined version
terms <- pred[,2:63]



#######------- Response -------#######
arbo_data <- as.data.frame(read.table("Cordoba_dengue_2009-2018.csv",
                                      sep = ",", header=TRUE))
names(arbo_data)[1] = "YEAR"

YR2010 = c(2009,2010)
YR2012 = c(2011,2012)
YR2014 = c(2013,2014)
YR2016 = c(2015,2016)
YR2018 = c(2017,2018)

data2010 <- arbo_data[which(arbo_data[,"YEAR"]==YR2010),]
data2012 <- arbo_data[which(arbo_data[,"YEAR"]==YR2012),]
data2014 <- arbo_data[which(arbo_data[,"YEAR"]==YR2014),]
data2016 <- arbo_data[which(arbo_data[,"YEAR"]==YR2016),]
data2018 <- arbo_data[which(arbo_data[,"YEAR"]==YR2018),]

data <- rbind(data2010,data2012,data2014,data2016,data2018)
data$DATE = as.Date(data$DATE, format="%m/%d/%Y")
cases <- as.numeric(data$AUTO_CASES)
ew <- epiweek(data$DATE)

plot(data$AUTO_CASES, xlab="Weeks", ylab="Cases", col="black", type="o")
lines(data$AUTO_CASES, xlab="Weeks", ylab="Cases", col="blue")

# Pull subset from outbreak time period
outbreak <- data[54:130,]
cases2 <- outbreak$AUTO_CASES
cases2


#######------- Interpolation of Response -------#######

## Outbreak 2013

# Load data
outbreak2013 <- outbreak[1:5,]
my_x_2013 <- as_date(outbreak2013$DATE)
my_x_2013
my_y_2013 <- outbreak2013$AUTO_CASES
my_y_2013
df_2013 <- data.frame(my_x_2013, my_y_2013)
df_2013
Dates_2013 <- seq.Date(ymd("2013-02-04"), ymd("2013-05-13"), by=7)
Dates_2013

# Add EW2 column to change EW sequence
EW2_2013 <- 1:length(Dates_2013)
ew_b_2013 <- epiweek(outbreak2013$DATE)
ew_b_2013
ew_d_2013 <- vector(length = length(ew_b_2013))
# for loop rearranges ew_d_2013, starting at 1
for (i in 1:length(ew_b_2013)) {
  ew_d_2013[i] <- ((ew_b_2013[i]-(ew_b_2013[1]))%%(length(EW2_2013)))+1
}
x_2013 <- ew_d_2013
x_2013
y_2013 <- my_y_2013
y_2013

# Spline Interpolation
SplineInt_2013 <- data.frame(spline(x=x_2013, y=y_2013, n=15, 
                                    method="natural", xmin=1, xmax=15))
SplineInt_2013$y <- pmax(SplineInt_2013$y, 0)
my_SplineInt_2013 <- round(SplineInt_2013$y, digits=2)
my_SplineInt_2013
plot(SplineInt_2013$x, SplineInt_2013$y, xlab="Weeks", ylab="Cases", 
     col="black", type="o")
lines(SplineInt_2013$x, SplineInt_2013$y, col="blue")


## Outbreak 2015

# Load data
outbreak2015 <- outbreak[46:51,]
my_x_2015 <- as_date(outbreak2015$DATE)
my_x_2015
my_y_2015 <- outbreak2015$AUTO_CASES
my_y_2015
df_2015 <- data.frame(my_x_2015, my_y_2015)
df_2015
Dates_2015 <- seq.Date(ymd("2015-02-23"), ymd("2015-05-09"), by=7)

# Add EW2 column to change EW sequence
EW2_2015 <- 1:length(Dates_2015)
ew_b_2015 <- epiweek(outbreak2015$DATE)
ew_b_2015
ew_d_2015 <- vector(length = length(ew_b_2015))
for (i in 1:length(ew_b_2015)) {
  ew_d_2015[i] <- ((ew_b_2015[i]-(ew_b_2015[1]))%%(length(EW2_2015)))+1
}
x_2015 <- ew_d_2015
x_2015
y_2015 <- my_y_2015
y_2015

# Spline Interpolation
SplineInt_2015 <- data.frame(spline(x=x_2015, y=y_2015, n=11, 
                                    method="natural", xmin=1, xmax=11))
SplineInt_2015$y <- pmax(SplineInt_2015$y, 0)
my_SplineInt_2015 <- round(SplineInt_2015$y, digits=2)
my_SplineInt_2015
plot(SplineInt_2015$x, SplineInt_2015$y, xlab="Weeks", ylab="Cases", 
     col="black", type="o")
lines(SplineInt_2015$x, SplineInt_2015$y, col="blue")


## Outbreak 2016

# Load data
outbreak2016 <- outbreak[67:77,]
my_x_2016 <- as_date(outbreak2016$DATE)
my_x_2016
my_y_2016 <- outbreak2016$AUTO_CASES
my_y_2016
df_2016 <- data.frame(my_x_2016, my_y_2016)
df_2016
Dates_2016 <- seq.Date(ymd("2015-12-28"), ymd("2016-05-30"), by=7)

# Add EW2 column to change EW sequence
EW2_2016 <- 1:length(Dates_2016)
ew_b_2016 <- epiweek(outbreak2016$DATE)
ew_b_2016
ew_d_2016 <- vector(length = length(ew_b_2016))
for (i in 1:length(ew_b_2016)) {
  ew_d_2016[i] <- ((ew_b_2016[i]-(ew_b_2016[1]))%%52)+1
}
x_2016 <- ew_d_2016
x_2016
y_2016 <- my_y_2016
y_2016

# Spline Interpolation
SplineInt_2016 <- data.frame(spline(x=x_2016, y=y_2016, n=23, 
                                    method="natural", xmin=1, xmax=23))
SplineInt_2016$y <- pmax(SplineInt_2016$y, 0)
my_SplineInt_2016 <- round(SplineInt_2016$y, digits=2)
my_SplineInt_2016
plot(SplineInt_2016$x, SplineInt_2016$y, xlab="Weeks", ylab="Cases", 
     col="black", type="o")
lines(SplineInt_2016$x, SplineInt_2016$y, col="blue")


#######------- Simplification of Y and Xs -------#######

Y_2013 <- my_SplineInt_2013
Y_2015 <- my_SplineInt_2015
Y_2016 <- my_SplineInt_2016
Y_2013_fill <- integer(93) # 114-21
Y_2015_fill <- integer(33) # 158-125
my_zeros <- integer(5)
Y <- c(my_zeros,Y_2013,Y_2013_fill,Y_2015,Y_2015_fill,Y_2016)

df2 <- data.frame(pred$st1_date,Y,pred$st1_hit1)
df2

pred_date <- pred$st1_date
cases_date <- seq.Date(ymd("2013-02-04"), ymd("2016-05-30"), by=7)
# na <- integer(0)
# class(na) <- "Date"
# cases_date2 <- c(na,cases_date)

# Export
df <- data.frame(pred_date, Y, 
                 X1, X2, X3, X4, X5, X6, X7, X8, X9,
                 X11, X12, X13, X14, X15, X16, X17, X18, X19, X20,
                 X21, X22, X23, X24, X25, X26, X28, X29, X30,
                 X31, X32, X33)
df
write.csv(df, "interpolation_mosquito_com_v2_2013-2016.csv", row.names=FALSE)


#######------- Export Interpolation Data -------#######
Y_2013 <- my_SplineInt_2013
Y_2015 <- my_SplineInt_2015
Y_2016 <- my_SplineInt_2016
Y <- c(Y_2013,Y_2015,Y_2016)

pred_com <- rbind(pred_2013,pred_2015,pred_2016)
pred_date <- pred_com$st1_date
dates_2013 <- as.data.frame(Dates_2013)
colnames(dates_2013) <- c("case_date")
dates_2015 <- as.data.frame(Dates_2015)
colnames(dates_2015) <- c("case_date")
dates_2016 <- as.data.frame(Dates_2016)
colnames(dates_2016) <- c("case_date")
cases_date <- rbind(dates_2013,dates_2015,dates_2016)

X1 <- pred_com$st1_hit1  # aedes
X2 <- pred_com$st1_hit2  # aedes aegypti
X3 <- pred_com$st1_hit3  # aegypti
X4 <- pred_com$st1_hit4  # vomit
X5 <- pred_com$st1_hit5  # mosquito
X6 <- pred_com$st2_hit1  # dengue virus
X7 <- pred_com$st2_hit2  # dengue fever
X8 <- pred_com$st2_hit3  # dengue hemorrhagic fever
X9 <- pred_com$st2_hit4  # dengue symptoms
# x10 <- pred_com$st3_hit1  # mosquito
X11 <- pred_com$st3_hit2  # mosquito dengue
X12 <- pred_com$st3_hit3  # mosquitoes
X13 <- pred_com$st3_hit4  # fever
X14 <- pred_com$st4_hit1  # new dengue virus
X15 <- pred_com$st4_hit2  # DENV
X16 <- pred_com$st4_hit3  # DHF
X17 <- pred_com$st4_hit4  # disease
X18 <- pred_com$st5_hit1  # pain
X19 <- pred_com$st5_hit2  # rash
X20 <- pred_com$st5_hit3  # headache
X21 <- pred_com$st5_hit4  # nausea
X22 <- pred_com$st6_hit1  # virus del dengue
X23 <- pred_com$st6_hit2  # síntomas del dengue
X24 <- pred_com$st6_hit3  # mosquitos
X25 <- pred_com$st6_hit4  # zancudo
X26 <- pred_com$st7_hit1  # nuevo virus del dengue
# X27 <- pred_com$st7_hit2  # dolor
X28 <- pred_com$st7_hit2  # erupción
X29 <- pred_com$st7_hit3  # dolor de cabeza
X30 <- pred_com$st8_hit1  # náuseas
X31 <- pred_com$st8_hit2  # vomitar
X32 <- pred_com$st8_hit3  # vómitos
X33 <- pred_com$st8_hit4  # fiebre
# X34 <- pred_com$st9_hit1  # enfermedad

df <- data.frame(pred_date, cases_date, Y, 
                 X1, X2, X3, X4, X5, X6, X7, X8, X9,
                 X11, X12, X13, X14, X15, X16, X17, X18, X19, X20,
                 X21, X22, X23, X24, X25, X26, X28, X29, X30,
                 X31, X32, X33)
df
write.csv(df, "interpolation_mosquito_com_2013-2016.csv", row.names=FALSE)

