##########  Customer+Orderform.csv #############

install.packages("readxl")
library(readxl)
customerdata <- read.csv(file.choose()) # customer+orderform.csv #
View(customerdata)
attach(customerdata)
library(e1071)
customerorder <- as.data.frame(lapply(customerdata, as.numeric))
View(customerorder)

#######  R1 : Defective Percentage are Same across center
#######  R2 : Defective Percentage are varies across center 
attach(customerorder)
?chisq.test
chisq.test(customerorder)
 ####data:  customOrder
#X-squared = 44.655, df = 897, p-value = 1

chisq.test(customOrder,simulate.p.value = TRUE)

# X-squared = 44.655, df = NA, p-value = 1
# p-value = 1 >0.05 so p high null fly => accept NULL hypothesis.
# Hence Defective percentage are Same across the Countries ##

#----------------------------------------------------

############### BuyerRatio.csv ##################
# sales of Products in different regions #
library(readxl)
buyer_ratio <- read.csv(file.choose())  
attach(buyer_ratio)
View(buyer_ratio)
attach(buyer_ratio)
r1 <- East
r2 <- West
r3 <- North
r4 <- South
r <- data.frame(r1,r2,r3,r4)
View(r)
chisq.test(r)
 ### data:  buyer_ratio
####X-squared = 1.5959, df = 3, p-value = 0.6603
#---------------------------------------------------

############# Faltoons.csv Dataset ###########
faltoons <- read.csv(file.choose())
View(faltoons)
attach(faltoons)

table1 <- table(Weekdays,Weekend)
View(table1)
table1

Weekdays <- table(Weekdays)
Weekdays

##Female   Male 
#   287    113

Weekend <- table(Weekend)
Weekend

##Female   Male 
#   233    167

# In this Dataset both variables are Discrete so, We
# proceed with 2-Proportion Test.

# p1 = Proportions of Male & Feamle are same on weekdays.
# p2 = Proportions of Male & Female are different on Weekdays.

prop.test(x=c(287,116),n=c(520,270),conf.level = 0.95,correct = FALSE,alternative = "two.sided")

# data:  c(287, 116) out of c(520, 270)
# X-squared = 10.636, df = 1, p-value = 0.001109 < 0.05, so We accept Alternate Hypothesis.
#  prop 1    prop 2 
# 0.5519231 0.4296296

# Hence proportions of males and female on weekdays are same.


#--------------------------------------------------------

################# Cutlets.csv Dataset ###########
library(readxl)
library(e1071)
cutlets <- read.csv(file.choose())
View(cutlets)
attach(cutlets)

chisq.test(cutlets)
  #  Pearson's Chi-squared test

  #  data:  u
  #  X-squared = 0.45428, df = 34, p-value = 1 > 0.05, So, p is high Null fly.
  #  We accpet Null Hypothesis.

#---------------------------------------------------------

############### Labtat.csv Dataset ###############
library(readxl)
library(e1071)
labdata <- read.csv(file.choose())
View(labdata)
attach(labdata)

chisq.test(labdata)
# Pearson's Chi-squared, testdata:  labdata
#  X-squared = 450.67, df = 357, p-value = 0.0005508 < 0.05, So we accept Alternate Hypothesis.

#----------------------------------------------------------------------
getwd()
q()
