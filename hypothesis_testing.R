setwd("E:\\Excelr Data\\R Codes\\Hyothesis Testing")

##### Normality Test##################
install.packages("readxl")

install.packages("readxl")
library(readxl)
#The attach function allows to access variables of a data.frame without calling the data.frame#data1$bmi, bmi
#attach(pmi)
#attach(bahaman)
#attach(Promotion)

######## Promotion.xlsx data ###################

Promotion<-read_excel(file.choose())# Promotion.xlsx
View(Promotion)
#attach(Promotion)
colnames(Promotion)<-c("Credit","Promotion.Type","InterestRateWaiver","StandardPromotion")
# Changing column names
View(Promotion)
attach(Promotion)

#############Normality test###############

shapiro.test(InterestRateWaiver)
# p-value = 0.2246 >0.05 so p high null fly => It follows normal distribution

shapiro.test(StandardPromotion)
# p-value = 0.1916 >0.05 so p high null fly => It follows normal distribution

#############Variance test###############
#var(InterestRateWaiver)=var(standardpromotion)
#var(InterestRateWaiver) is not equal to var(standardpromotion)
var.test(InterestRateWaiver,StandardPromotion)#variance test
# p-value = 0.653 > 0.05 so p high null fly => Equal variances


############2 sample T Test ##################
#mean(interestratewaiver)=mean(standardpromotion)
#mean(interestwaiver) is not equal to mean(standardpromotion)
?t.test
t.test(InterestRateWaiver,StandardPromotion,alternative = "two.sided",conf.level = 0.95,correct = TRUE)#two sample T.Test
# alternative = "two.sided" means we are checking for equal and unequal
# means
# null Hypothesis -> Equal means
# Alternate Hypothesis -> Unequal means
# p-value = 0.02423 < 0.05 accept alternate Hypothesis 
# unequal means

?t.test
t.test(InterestRateWaiver,StandardPromotion,alternative = "greater",var.equal = T)

# alternative = "greater means true difference is greater than 0
# Null Hypothesis -> (InterestRateWaiver-StandardPromotion) <= 0
# Alternative Hypothesis ->  (InterestRateWaiver-StandardPromotion) > 0
# p-value = 0.01211 < 0.05 => p low null go => accept alternate hypothesis
# InterestRateWaiver better strategy than StandardPromotion


###################Proportional T Test(JohnyTalkers data)##########

Johnytalkers<-read_excel(file.choose())   # JohnyTalkers.xlsx
View(Johnytalkers) 
attach(Johnytalkers)
table1 <- table(Drinks,Person)
View(table1)
table1
?prop.test
prop.test(x=c(58,152),n=c(480,740),conf.level = 0.95,correct = FALSE,alternative = "two.sided")
# two. sided -> means checking for equal proportions and unequal proportions of Adults and children under purchased
# p-value = 0.000132 < 0.05 accept alternate hypothesis i.e.
# Unequal proportions 

prop.test(x=c(58,152),n=c(480,740),conf.level = 0.95,correct = FALSE,alternative = "less")
# Ho -> Proportions of Children >equal Proportions of Adults
# Ha -> Proportions of Adults > Proportions of Children
# p-value = 6.559e-05 <0.05  donotaccept null hypothesis 
# so proportion of Children < proportion of adults 
# launch the drinks shop


#########Chi Square(Bahaman Research)#################

library(readxl)
Bahaman<-read_excel(file.choose()) # Bahaman.xlsx
View(Bahaman)
attach(Bahaman)
?table
?attach
table(Country,Defective)

#t2 <- prop.table(table(Defective))
#t1 <- table(Country)
?chisq.test

chisq.test(Country, Defective)
# p-value = 0.6315 > 0.05  => Accept null hypothesis
# => All countries have equal proportions 

#############Anova (Contract_Renewal Data )##########
library(car)
CRD<-read_excel(file.choose())   # ContractRenewal_Data(unstacked).xlsx
View(CRD)
attach(CRD)
shapiro.test(`Supplier A`)
shapiro.test(`Supplier B`)
shapiro.test(`Supplier C`)
Stacked_Data <- stack(CRD)
View(Stacked_Data)
attach(Stacked_Data)
?bartlett.test
?aov
bartlett.test(values~ind, data=Stacked_Data) #test for homogeneity of variances
Anova_results <- aov(values~ind,data = Stacked_Data)
summary(Anova_results)
# p-value = 0.104 > 0.05 accept null hypothesis 
#Null hypothesis: All means are equal
#Alternate hypothesis: means are not equal





