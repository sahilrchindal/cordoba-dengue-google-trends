#######------- Clear Workspace -------#######
rm(list=ls())

#######------- Set Working Directory -------#######
setwd("G:/My Drive/Classes/Research/Human Movement/Google")

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


#######------- Pull Data (search terms) (mosquito) [Single-year: 2015-2016] -------#######
# Use geo = "AR" for Argentina
# Use geo = "AR-X" for Cordoba province
# Use hl = "es" for Spanish (default is English)

st1 <- gtrends(c("aedes", "aedes aegypti", "aegypti", "vomit", "mosquito"),
               geo = "AR-X", time = "2015-10-01 2016-09-30", 
               gprop = c("web", "news", "images", "froogle", "youtube"))
plot(st1)
st2 <- gtrends(c("dengue virus", "dengue fever", "dengue hemorrhagic fever",
                 "dengue symptoms", "mosquito"),
               geo = "AR-X", time = "2015-10-01 2016-09-30", 
               gprop = c("web", "news", "images", "froogle", "youtube"))
plot(st2)
st3 <- gtrends(c("mosquito", "mosquito dengue", "mosquitoes", "fever"), 
               geo = "AR-X", time = "2015-10-01 2016-09-30", 
               gprop = c("web", "news", "images", "froogle", "youtube"))
plot(st3)
st4 <- gtrends(c("new dengue virus", "DENV", "DHF", "disease", "mosquito"), 
               geo = "AR-X", time = "2015-10-01 2016-09-30", 
               gprop = c("web", "news", "images", "froogle", "youtube"))
plot(st4)
st5 <- gtrends(c("pain", "rash", "headache", "nausea", "mosquito"), 
               geo = "AR-X", time = "2015-10-01 2016-09-30", 
               gprop = c("web", "news", "images", "froogle", "youtube"))
plot(st5)
st6 <- gtrends(c("virus del dengue", "síntomas del dengue", "mosquitos",
                 "zancudo", "mosquito"), 
               geo = "AR-X", time = "2015-10-01 2016-09-30", hl = "es",
               gprop = c("web", "news", "images", "froogle", "youtube"))
plot(st6)
st7 <- gtrends(c("nuevo virus del dengue", "erupción",
                 "dolor de cabeza", "mosquito"),
               geo = "AR-X", time = "2015-10-01 2016-09-30", hl = "es",
               gprop = c("web", "news", "images", "froogle", "youtube"))
plot(st7)
st8 <- gtrends(c("náuseas", "vomitar", "vómitos", "fiebre", "mosquito"),
               geo = "AR-X", time = "2015-10-01 2016-09-30", hl = "es",
               gprop = c("web", "news", "images", "froogle", "youtube"))
plot(st8)
st9 <- gtrends(c("enfermedad", "mosquito"),
               geo = "AR-X", time = "2015-10-01 2016-09-30", hl = "es",
               gprop = c("web", "news", "images", "froogle", "youtube"))
plot(st9)


st1_date <- st1$interest_over_time$date[1:52]
st1_term1 <- st1$interest_over_time$keyword[1:52]
st1_term2 <- st1$interest_over_time$keyword[53:104]
st1_term3 <- st1$interest_over_time$keyword[105:156]
st1_term4 <- st1$interest_over_time$keyword[157:208]
st1_term5 <- st1$interest_over_time$keyword[209:260]

st2_term1 <- st2$interest_over_time$keyword[1:52]
st2_term2 <- st2$interest_over_time$keyword[53:104]
st2_term3 <- st2$interest_over_time$keyword[105:156]
st2_term4 <- st2$interest_over_time$keyword[157:208]

st3_term1 <- st3$interest_over_time$keyword[1:52]
st3_term2 <- st3$interest_over_time$keyword[53:104]
st3_term3 <- st3$interest_over_time$keyword[105:156]
st3_term4 <- st3$interest_over_time$keyword[157:208]

st4_term1 <- st4$interest_over_time$keyword[1:52]
st4_term2 <- st4$interest_over_time$keyword[53:104]
st4_term3 <- st4$interest_over_time$keyword[105:156]
st4_term4 <- st4$interest_over_time$keyword[157:208]

st5_term1 <- st5$interest_over_time$keyword[1:52]
st5_term2 <- st5$interest_over_time$keyword[53:104]
st5_term3 <- st5$interest_over_time$keyword[105:156]
st5_term4 <- st5$interest_over_time$keyword[157:208]

st6_term1 <- st6$interest_over_time$keyword[1:52]
st6_term2 <- st6$interest_over_time$keyword[53:104]
st6_term3 <- st6$interest_over_time$keyword[105:156]
st6_term4 <- st6$interest_over_time$keyword[157:208]

st7_term1 <- st7$interest_over_time$keyword[1:52]
st7_term2 <- st7$interest_over_time$keyword[53:104]
st7_term3 <- st7$interest_over_time$keyword[105:156]
# st7_term4 <- st7$interest_over_time$keyword[157:208]

st8_term1 <- st8$interest_over_time$keyword[1:52]
st8_term2 <- st8$interest_over_time$keyword[53:104]
st8_term3 <- st8$interest_over_time$keyword[105:156]
st8_term4 <- st8$interest_over_time$keyword[157:208]

# st9_term1 <- st9$interest_over_time$keyword[1:52]


st1_hit1 <- st1$interest_over_time$hits[1:52]
st1_hit2 <- st1$interest_over_time$hits[53:104]
st1_hit3 <- st1$interest_over_time$hits[105:156]
st1_hit4 <- st1$interest_over_time$hits[157:208]
st1_hit5 <- st1$interest_over_time$hits[209:260]

st2_hit1 <- st2$interest_over_time$hits[1:52]
st2_hit2 <- st2$interest_over_time$hits[53:104]
st2_hit3 <- st2$interest_over_time$hits[105:156]
st2_hit4 <- st2$interest_over_time$hits[157:208]

st3_hit1 <- st3$interest_over_time$hits[1:52]
st3_hit2 <- st3$interest_over_time$hits[53:104]
st3_hit3 <- st3$interest_over_time$hits[105:156]
st3_hit4 <- st3$interest_over_time$hits[157:208]

st4_hit1 <- st4$interest_over_time$hits[1:52]
st4_hit2 <- st4$interest_over_time$hits[53:104]
st4_hit3 <- st4$interest_over_time$hits[105:156]
st4_hit4 <- st4$interest_over_time$hits[157:208]

st5_hit1 <- st5$interest_over_time$hits[1:52]
st5_hit2 <- st5$interest_over_time$hits[53:104]
st5_hit3 <- st5$interest_over_time$hits[105:156]
st5_hit4 <- st5$interest_over_time$hits[157:208]

st6_hit1 <- st6$interest_over_time$hits[1:52]
st6_hit2 <- st6$interest_over_time$hits[53:104]
st6_hit3 <- st6$interest_over_time$hits[105:156]
st6_hit4 <- st6$interest_over_time$hits[157:208]

st7_hit1 <- st7$interest_over_time$hits[1:52]
st7_hit2 <- st7$interest_over_time$hits[53:104]
st7_hit3 <- st7$interest_over_time$hits[105:156]
# st7_hit4 <- st7$interest_over_time$hits[157:208]

st8_hit1 <- st8$interest_over_time$hits[1:52]
st8_hit2 <- st8$interest_over_time$hits[53:104]
st8_hit3 <- st8$interest_over_time$hits[105:156]
st8_hit4 <- st8$interest_over_time$hits[157:208]

# st9_hit1 <- st9$interest_over_time$hits[1:52]


#######------- Pull Data (search terms) (mosquito) [Multi-year: 2013-2016] -------#######
# Use geo = "AR" for Argentina
# Use geo = "AR-X" for Cordoba province
# Use hl = "es" for Spanish (default is English)

st1 <- gtrends(c("aedes", "aedes aegypti", "aegypti", "vomit", "mosquito"),
               geo = "AR-X", time = "2012-12-23 2016-05-30", 
               gprop = c("web", "news", "images", "froogle", "youtube"))
plot(st1)
st2 <- gtrends(c("dengue virus", "dengue fever", "dengue hemorrhagic fever",
                 "dengue symptoms", "mosquito"),
               geo = "AR-X", time = "2012-12-23 2016-05-30", 
               gprop = c("web", "news", "images", "froogle", "youtube"))
plot(st2)
st3 <- gtrends(c("mosquito", "mosquito dengue", "mosquitoes", "fever"), 
               geo = "AR-X", time = "2012-12-23 2016-05-30", 
               gprop = c("web", "news", "images", "froogle", "youtube"))
plot(st3)
st4 <- gtrends(c("new dengue virus", "DENV", "DHF", "disease", "mosquito"), 
               geo = "AR-X", time = "2012-12-23 2016-05-30", 
               gprop = c("web", "news", "images", "froogle", "youtube"))
plot(st4)
st5 <- gtrends(c("pain", "rash", "headache", "nausea", "mosquito"), 
               geo = "AR-X", time = "2012-12-23 2016-05-30", 
               gprop = c("web", "news", "images", "froogle", "youtube"))
plot(st5)
st6 <- gtrends(c("virus del dengue", "síntomas del dengue", "mosquitos",
                 "zancudo", "mosquito"), 
               geo = "AR-X", time = "2012-12-23 2016-05-30", hl = "es",
               gprop = c("web", "news", "images", "froogle", "youtube"))
plot(st6)
st7 <- gtrends(c("nuevo virus del dengue", "erupción",
                 "dolor de cabeza", "mosquito"),
               geo = "AR-X", time = "2012-12-23 2016-05-30", hl = "es",
               gprop = c("web", "news", "images", "froogle", "youtube"))
plot(st7)
st8 <- gtrends(c("náuseas", "vomitar", "vómitos", "fiebre", "mosquito"),
               geo = "AR-X", time = "2012-12-23 2016-05-30", hl = "es",
               gprop = c("web", "news", "images", "froogle", "youtube"))
plot(st8)
st9 <- gtrends(c("enfermedad", "mosquito"),
               geo = "AR-X", time = "2012-12-23 2016-05-30", hl = "es",
               gprop = c("web", "news", "images", "froogle", "youtube"))
plot(st9)


st1_date <- st1$interest_over_time$date[1:180]
st1_term1 <- st1$interest_over_time$keyword[1:180]
st1_term2 <- st1$interest_over_time$keyword[181:360]
st1_term3 <- st1$interest_over_time$keyword[361:540]
st1_term4 <- st1$interest_over_time$keyword[541:720]
st1_term5 <- st1$interest_over_time$keyword[721:900]

st2_term1 <- st2$interest_over_time$keyword[1:180]
st2_term2 <- st2$interest_over_time$keyword[181:360]
st2_term3 <- st2$interest_over_time$keyword[361:540]
st2_term4 <- st2$interest_over_time$keyword[541:720]

st3_term1 <- st3$interest_over_time$keyword[1:180]
st3_term2 <- st3$interest_over_time$keyword[181:360]
st3_term3 <- st3$interest_over_time$keyword[361:540]
st3_term4 <- st3$interest_over_time$keyword[541:720]

st4_term1 <- st4$interest_over_time$keyword[1:180]
st4_term2 <- st4$interest_over_time$keyword[181:360]
st4_term3 <- st4$interest_over_time$keyword[361:540]
st4_term4 <- st4$interest_over_time$keyword[541:720]

st5_term1 <- st5$interest_over_time$keyword[1:180]
st5_term2 <- st5$interest_over_time$keyword[181:360]
st5_term3 <- st5$interest_over_time$keyword[361:540]
st5_term4 <- st5$interest_over_time$keyword[541:720]

st6_term1 <- st6$interest_over_time$keyword[1:180]
st6_term2 <- st6$interest_over_time$keyword[181:360]
st6_term3 <- st6$interest_over_time$keyword[361:540]
st6_term4 <- st6$interest_over_time$keyword[541:720]

st7_term1 <- st7$interest_over_time$keyword[1:180]
st7_term2 <- st7$interest_over_time$keyword[181:360]
st7_term3 <- st7$interest_over_time$keyword[361:540]
# st7_term4 <- st7$interest_over_time$keyword[541:720]

st8_term1 <- st8$interest_over_time$keyword[1:180]
st8_term2 <- st8$interest_over_time$keyword[181:360]
st8_term3 <- st8$interest_over_time$keyword[361:540]
st8_term4 <- st8$interest_over_time$keyword[541:720]

# st9_term1 <- st9$interest_over_time$keyword[1:180]


st1_hit1 <- st1$interest_over_time$hits[1:180]
st1_hit2 <- st1$interest_over_time$hits[181:360]
st1_hit3 <- st1$interest_over_time$hits[361:540]
st1_hit4 <- st1$interest_over_time$hits[541:720]
st1_hit5 <- st1$interest_over_time$hits[721:900]

st2_hit1 <- st2$interest_over_time$hits[1:180]
st2_hit2 <- st2$interest_over_time$hits[181:360]
st2_hit3 <- st2$interest_over_time$hits[361:540]
st2_hit4 <- st2$interest_over_time$hits[541:720]

st3_hit1 <- st3$interest_over_time$hits[1:180]
st3_hit2 <- st3$interest_over_time$hits[181:360]
st3_hit3 <- st3$interest_over_time$hits[361:540]
st3_hit4 <- st3$interest_over_time$hits[541:720]

st4_hit1 <- st4$interest_over_time$hits[1:180]
st4_hit2 <- st4$interest_over_time$hits[181:360]
st4_hit3 <- st4$interest_over_time$hits[361:540]
st4_hit4 <- st4$interest_over_time$hits[541:720]

st5_hit1 <- st5$interest_over_time$hits[1:180]
st5_hit2 <- st5$interest_over_time$hits[181:360]
st5_hit3 <- st5$interest_over_time$hits[361:540]
st5_hit4 <- st5$interest_over_time$hits[541:720]

st6_hit1 <- st6$interest_over_time$hits[1:180]
st6_hit2 <- st6$interest_over_time$hits[181:360]
st6_hit3 <- st6$interest_over_time$hits[361:540]
st6_hit4 <- st6$interest_over_time$hits[541:720]

st7_hit1 <- st7$interest_over_time$hits[1:180]
st7_hit2 <- st7$interest_over_time$hits[181:360]
st7_hit3 <- st7$interest_over_time$hits[361:540]
# st7_hit4 <- st7$interest_over_time$hits[541:720]

st8_hit1 <- st8$interest_over_time$hits[1:180]
st8_hit2 <- st8$interest_over_time$hits[181:360]
st8_hit3 <- st8$interest_over_time$hits[361:540]
st8_hit4 <- st8$interest_over_time$hits[541:720]

# st9_hit1 <- st9$interest_over_time$hits[1:180]

#######------- Export Data -------#######
df <- data.frame(st1_date, 
                 st1_term1, st1_hit1, st1_term2, st1_hit2, 
                 st1_term3, st1_hit3, st1_term4, st1_hit4, st1_term5, st1_hit5, 
                 st2_term1, st2_hit1, st2_term2, st2_hit2, 
                 st2_term3, st2_hit3, st2_term4, st2_hit4, 
                 # st3_term1, st3_hit1, 
                 st3_term2, st3_hit2, 
                 st3_term3, st3_hit3, st3_term4, st3_hit4, 
                 st4_term1, st4_hit1, st4_term2, st4_hit2,
                 st4_term3, st4_hit3, st4_term4, st4_hit4, 
                 st5_term1, st5_hit1, st5_term2, st5_hit2, 
                 st5_term3, st5_hit3, st5_term4, st5_hit4,
                 st6_term1, st6_hit1, st6_term2, st6_hit2, 
                 st6_term3, st6_hit3, st6_term4, st6_hit4,
                 st7_term1, st7_hit1, st7_term2, st7_hit2, 
                 st7_term3, st7_hit3, 
                 # st7_term4, st7_hit4,
                 st8_term1, st8_hit1, st8_term2, st8_hit2, 
                 st8_term3, st8_hit3, st8_term4, st8_hit4
                 # st9_term1, st9_hit1
                 )
df  # includes 31 search terms
setwd("G:/My Drive/Classes/Research/Human Movement/Google/Data/Yearly")
write.csv(df, "st_ARX_mosquito_com_2013-2016.csv", row.names=FALSE)

