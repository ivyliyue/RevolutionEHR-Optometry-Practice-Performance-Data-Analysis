setwd("C:/Users/ivyli/Downloads")
setwd("C:/Users/ivyli/Desktop/Predictive Analytics/CSC424/Project")
install.packages("corrplot")
library("corrplot")



RawPerformance = read.csv("practice_performance.csv", header=TRUE)

######################### Data cleaning - this part is group effort ##############################

# This batch of cleaning is all about row removal.  We utilize subset and a boolean operator to select data
# from the list.  Create variable for the count of the subset along the way so that we can record the impact of the data removal.
count_pre_cleaning = nrow(RawPerformance)
count_pre_cleaning


# For the first bit, we are going to require sales and receipts amounts that are all greater than zero.
RawPerformance = subset(RawPerformance, salesbyproviderall > 0)
RawPerformance = subset(RawPerformance, salesbyproviderinsurance > 0)
RawPerformance = subset(RawPerformance, salesbyproviderpatient > 0)
RawPerformance = subset(RawPerformance, receiptsbyproviderall > 0)
RawPerformance = subset(RawPerformance, receiptsbyproviderinsurance > 0)
RawPerformance = subset(RawPerformance, receiptsbyproviderpatient > 0)
count_after_cleaning_sales = nrow(RawPerformance)
count_after_cleaning_sales


# In order to use the count data, we need to have some counts.
# Several of the count variables can be turned into categorical. One such count would be
# the eyeglasscaptureratebyorder vs. the eyeglasscaptureratebybilling.  The different between byorder and
# bybilling is subtle.  In effect, the bybilling is the more restrictive of the two, as it requires eyeglass
# orders that are "properly" linked to invoices.  So we could use just the byorder rate and include a
# 1/0 value indicating if the sample makes use of the order to billing linkage.
RawPerformance = subset(RawPerformance, eyeglasscaptureratebyorder > 0)
RawPerformance = subset(RawPerformance, arcaptureratebyorder > 0)
RawPerformance = subset(RawPerformance, contactlenscapturerate > 0)
RawPerformance = subset(RawPerformance, medicalmanagementratio > 0)
RawPerformance = subset(RawPerformance, ratio9921x > 0)
RawPerformance = subset(RawPerformance, newpatientpercentage > 0)
RawPerformance = subset(RawPerformance, productionbooked > 0)
RawPerformance = subset(RawPerformance, productioncompleted > 0)
count_after_cleaning_rates = nrow(RawPerformance)
count_after_cleaning_rates


# This pulls forward just the practices with annual sales between 500k and 6000k
RawPerformance = subset(RawPerformance, salesbyproviderall <= 1500000)
RawPerformance = subset(RawPerformance, salesbyproviderall >= 125000)
count_after_cleaning_annual = nrow(RawPerformance)
count_after_cleaning_annual


# The removal here is all about scrubbing the greatest outliers. The first two lines remove samples
# with a high difference between sales and recieipts.  When the differences are outside of the range, it
# is typically a sign that something is off at the practice.  The final test excludes practices with
# extremely high accountsRecievable, meaning that they have a lot more outstanding invoices than they
# have sales numbers.
RawPerformance = subset(RawPerformance, salesbyproviderinsurance/receiptsbyproviderinsurance < 5)
RawPerformance = subset(RawPerformance, salesbyproviderinsurance/receiptsbyproviderinsurance > 0.2)
RawPerformance = subset(RawPerformance, accountsreceivableall < salesbyproviderall * 1.5)
count_after_cleaning_ledgers = nrow(RawPerformance)
count_after_cleaning_ledgers


# Building a summary of the rows removed so that we know the impact.
DataRemoved = data.frame(step = "Remove Negative Dollar Rows", records = (count_pre_cleaning - count_after_cleaning_sales))
DataRemoved = rbind(DataRemoved, data.frame(step = "Remove Zero-Value Rates", records = (count_after_cleaning_sales - count_after_cleaning_rates)))
DataRemoved = rbind(DataRemoved, data.frame(step = "Remove Outlying Annual Revenue", records = (count_after_cleaning_rates - count_after_cleaning_annual)))
DataRemoved = rbind(DataRemoved, data.frame(step = "Remove Outlier AR", records = (count_after_cleaning_annual - count_after_cleaning_ledgers)))
DataRemoved = rbind(DataRemoved, data.frame(step = "Total Removed", records = (count_pre_cleaning - count_after_cleaning_ledgers)))


#The rm() call here is all about deleting unnecessary variables from the environment
rm(
    count_pre_cleaning,
    count_after_cleaning_sales,
    count_after_cleaning_rates,
    count_after_cleaning_annual,
    count_after_cleaning_ledgers
)


# Fixing the ratio9921x so that it makes sense as a rate. The business unit wanted a ratio, not a true percentage calculation.
ratio9921xdenominator = RawPerformance$ratio9921xdenominator + RawPerformance$ratio9921xnumerator
ratio9921x = RawPerformance$ratio9921xnumerator / ratio9921xdenominator
RawPerformance = subset(RawPerformance, select=-c(ratio9921x, ratio9921xdenominator))
RawPerformance = cbind(RawPerformance, ratio9921x, ratio9921xdenominator)
rm(ratio9921x, ratio9921xdenominator)


# Dividing the data into count performance vs. rate performance.  For these metrics
# rate = numerator / denominator.

CountPerformance <- subset(RawPerformance, select=c(
  arcaptureratebyordernumerator,
  contactlenscaptureratenumerator,
  eyeglasscaptureratebyordernumerator,
  medicalmanagementrationumerator,
  newpatientpercentagenumerator,
  productionbookednumerator,
  patientage0to9,
  patientage10to19,
  patientage20to29,
  patientage30to39,
  patientage40to49,
  patientage50to59,
  patientage60to69,
  patientage70to79,
  patientage80to89,
  patientage90to99,
  patientgenderfemale,
  ratio9921xdenominator,
  ratio9921xnumerator,
  salesbyproviderinsurance,
  salesbyproviderpatient,
  receiptsbyproviderinsurance,
  receiptsbyproviderpatient,
  accountsreceivablecollections,
  accountsreceivableinsurance,
  accountsreceivablepatient
))

RatePerformance <- subset(RawPerformance, select=c(
  arcaptureratebyorder,
  contactlenscapturerate,
  eyeglasscaptureratebyorder,
  medicalmanagementratio,
  newpatientpercentage,
  ratio9921x,
  salesbyproviderinsurance,
  salesbyproviderpatient,
  receiptsbyproviderinsurance,
  receiptsbyproviderpatient,
  accountsreceivablecollections,
  accountsreceivableinsurance,
  accountsreceivablepatient
))


# As gender and age buckets are counts only, we calculate the relative size vs the total number of patients.

totalpatients = RawPerformance$patientgenderfemale + RawPerformance$patientgendermale
patientage0to9 = RawPerformance$patientage0to9 / totalpatients
patientage10to19 = RawPerformance$patientage10to19 / totalpatients
patientage20to29 = RawPerformance$patientage20to29 / totalpatients
patientage30to39 = RawPerformance$patientage30to39 / totalpatients
patientage40to49 = RawPerformance$patientage40to49 / totalpatients
patientage50to59 = RawPerformance$patientage50to59 / totalpatients
patientage60to69 = RawPerformance$patientage60to69 / totalpatients
patientage70to79 = RawPerformance$patientage70to79 / totalpatients
patientage80to89 = RawPerformance$patientage80to89 / totalpatients
patientage90to99 = RawPerformance$patientage90to99 / totalpatients
patientgenderfemale = RawPerformance$patientgenderfemale / totalpatients
patientgendermale = RawPerformance$patientgendermale / totalpatients

RatePerformance = cbind(RatePerformance,
                        patientage0to9,
                        patientage10to19,
                        patientage20to29,
                        patientage30to39,
                        patientage40to49,
                        patientage60to69,
                        patientage70to79,
                        patientage80to89,
                        patientage90to99,
                        patientgenderfemale,
                        patientgendermale,
                        totalpatients
)

rm(totalpatients,
   patientage0to9,
   patientage10to19,
   patientage20to29,
   patientage30to39,
   patientage40to49,
   patientage50to59,
   patientage60to69,
   patientage70to79,
   patientage80to89,
   patientage90to99,
   patientgenderfemale,
   patientgendermale
)

set.seed(42)
TrainingSet=sample(nrow(RawPerformance),nrow(RawPerformance)*0.8)


#  KMO and Bartlett's Tests on the Two Data Sets
library("psych")

CountPerformanceKMO <- KMO(cor(CountPerformance))
CountPerformanceKMO$MSA
CountPerformanceBartlett <- cortest.bartlett(cor(CountPerformance), n=nrow(CountPerformance))
CountPerformanceBartlett$p.value
RatePerformanceKMO <- KMO(cor(RatePerformance))
RatePerformanceKMO$MSA
RatePerformanceBartlett <- cortest.bartlett(cor(RatePerformance), n=nrow(RatePerformance))
RatePerformanceBartlett$p.value




####################### Correspondence Analysis_Huy Tran ###############################
install.packages("FactoMineR")
install.packages("ggplot2")
install.packages("factoextra")
install.packages("ca")
install.packages("MASS")
install.packages("vcd")

library(FactoMineR)
library(ggplot2)
library(factoextra)
library(ca)
library(MASS)
library(vcd)

#install.packages("prettyGraphs")
#install.packages("ExPosition")
#library(prettyGraphs)
#library(ExPosition)



## Creating categorical variables from Patientage variables 
#First, combine patientage variables to create wider age groups
RawPerformance$patientage0to29 = RawPerformance$patientage0to9 + RawPerformance$patientage10to19 + RawPerformance$patientage20to29
RawPerformance$patientage30to59 = RawPerformance$patientage30to39 + RawPerformance$patientage40to49 + RawPerformance$patientage50to59
RawPerformance$patientage60to89 = RawPerformance$patientage60to69 + RawPerformance$patientage70to79 + RawPerformance$patientage80to89
RawPerformance$patientage90to119 = RawPerformance$patientage90to99 + RawPerformance$patientage100to109 + RawPerformance$patientage110to119

#Remove the original patientage variables (THIS IS OPTIONAL)
RawPerformance$patientage0to9 = NULL
RawPerformance$patientage10to19 = NULL
RawPerformance$patientage20to29 = NULL
RawPerformance$patientage30to39 = NULL
RawPerformance$patientage40to49 = NULL
RawPerformance$patientage50to59 = NULL
RawPerformance$patientage60to69 = NULL
RawPerformance$patientage70to79 = NULL
RawPerformance$patientage80to89 = NULL
RawPerformance$patientage90to99 = NULL
RawPerformance$patientage100to109 = NULL
RawPerformance$patientage110to119 = NULL

dim(RawPerformance)

# Let's check the mean counts of each patientage variable
summary(RawPerformance$patientage0to29)
summary(RawPerformance$patientage30to59)
summary(RawPerformance$patientage60to89)
summary(RawPerformance$patientage90to119)

##If need to re-create new age groups, first remove them from table
#RawPerformance$patientage0to29 = NULL
#RawPerformance$patientage30to59 = NULL
#RawPerformance$patientage60to89 = NULL
#RawPerformance$patientage90to119 = NULL

# For each of the new patientage variables, if the number of patients (value in each cell) 
# is smaller than a certain value, replace it with a category
attach(RawPerformance)

RawPerformance$patientage0to29[patientage0to29 <= 350] = "Low" 
RawPerformance$patientage0to29[patientage0to29 > 350 & patientage0to29 <= 550] = "Medium" 
RawPerformance$patientage0to29[patientage0to29 > 550] = "High"

RawPerformance$patientage30to59[patientage30to59 <= 450] = "Low" 
RawPerformance$patientage30to59[patientage30to59 > 450 & patientage30to59 <= 730] = "Medium" 
RawPerformance$patientage30to59[patientage30to59 > 730] = "High"

RawPerformance$patientage60to89[patientage60to89 <= 310] = "Low" 
RawPerformance$patientage60to89[patientage60to89 > 310 & patientage60to89 <= 560] = "Medium" 
RawPerformance$patientage60to89[patientage60to89 > 560] = "High"

RawPerformance$patientage90to119[patientage90to119 <= 7] = "Low" 
RawPerformance$patientage90to119[patientage90to119 > 7 & patientage90to119 <= 17] = "Medium" 
RawPerformance$patientage90to119[patientage90to119 > 17] = "High"

detach(RawPerformance)

#Check frequencies of Low/Medium/High values in age variables
table(RawPerformance$patientage0to29)
table(RawPerformance$patientage30to59)
table(RawPerformance$patientage60to89)
table(RawPerformance$patientage90to119)




## Creating categorical variables from Gender variables
# Check the mean counts of each gender variable
summary(RawPerformance$patientgendermale)
summary(RawPerformance$patientgenderfemale)
summary(RawPerformance$patientgenderunknown)



# Referencing the mean counts, replace counts in gender variables with 1,2,3,4,5 depending on the counts
attach(RawPerformance)

RawPerformance$patientgendermale[patientgendermale <= 370] = "I"
RawPerformance$patientgendermale[370 < patientgendermale & patientgendermale <= 540] = "II"
RawPerformance$patientgendermale[540 < patientgendermale & patientgendermale <= 840] = "III"
RawPerformance$patientgendermale[840 < patientgendermale] = "IV"

RawPerformance$patientgenderfemale[patientgenderfemale <= 540] = "I"
RawPerformance$patientgenderfemale[540 < patientgenderfemale & patientgenderfemale <= 770] = "II"
RawPerformance$patientgenderfemale[770 < patientgenderfemale & patientgenderfemale <= 1200] = "III"
RawPerformance$patientgenderfemale[1200 < patientgenderfemale] = "IV"

#RawPerformance$patientgenderunknown[patientgenderunknown <= 100] = 1
#RawPerformance$patientgenderunknown[patientgenderunknown >100] = 2

detach(RawPerformance)



#Converting discrete numerical values (1,2,etc.) to characters
RawPerformance$patientgendermale = as.character(RawPerformance$patientgendermale)
RawPerformance$patientgenderfemale = as.character(RawPerformance$patientgenderfemale)
#RawPerformance$patientgenderunknown = as.character(RawPerformance$patientgenderunknown)

table(RawPerformance$patientgendermale)
table(RawPerformance$patientgenderfemale)
#table(RawPerformance$patientgenderunknown)


## Creating frequency tables between gender (Male, Female, Unknown) vs age groups (new patientage variables) 
#Male vs. 
detach(RawPerformance)     #Check if dataframe was attached. If not yet, run attach() below.
attach(RawPerformance)

t1 = table(patientgenderfemale, patientage0to29)
t2 = table(patientgendermale, patientage0to29)
t3 = table(patientgenderfemale, patientage30to59)
t4 = table(patientgendermale, patientage30to59)
t5 = table(patientgenderfemale, patientage60to89)
t6 = table(patientgendermale, patientage60to89)
t7 = table(patientgenderfemale, patientage90to119)
t8 = table(patientgendermale, patientage90to119)



## Running Correspondence Analysis 
library(ca)

round(prop.table(t1, 1),3)   #Row %
round(prop.table(t1, 2), 3)   #Col %
round(prop.table(t2, 2), 3)
round(prop.table(t3, 2), 3)
round(prop.table(t4, 2), 3)
round(prop.table(t5, 2), 3)
round(prop.table(t6, 2), 3)
round(prop.table(t7, 2), 3)
round(prop.table(t8, 2), 3)



#CA on t1 - CA between Female and Age group 0-29
ca1 = ca(t1)
summary(ca1)
ca1
plot(ca1)

plot(ca1, mass=T, contrib="absolute", 
     map="rowgreen", arrows=c(F, T))

mosaic(t1, shade=TRUE, legend=TRUE) 

##package FactoMineR
chisq2 = chisq.test(t2)
chisq2
res.ca1 = CA(t1, graph = TRUE) 
print(res.ca1)
summary(res.ca1)

##Package factoextra
get_eigenvalue(res.ca2)  
fviz_screeplot(res.ca1, addlabels = TRUE, ylim = c(0, 80))
1/(ncol(t1)-1) 


##Scree plot to determine the number of dimensions to be used
fviz_screeplot(res.ca1) +
  geom_hline(yintercept=50.00, color="red")


##Biplot 
fviz_ca_biplot(res.ca1, repel = TRUE)



##Visualize cos2 of row points on all the dimensions 
row1 = get_ca_row(res.ca1)
corrplot(row1$cos2, is.corr=FALSE)


##Dimension description
res.desc1 = dimdesc(res.ca1, axes = c(1,2))
res.desc1


#CA on t2 - CA between Male and Age group 0-29
ca2 = ca(t2)
summary(ca2)
ca2
plot(ca2)

plot(ca2, mass=T, contrib="absolute", 
     map="rowgreen", arrows=c(F, T))

mosaic(t2, shade=TRUE, legend=TRUE) 



#CA on t3 - CA between Female and Age group 30-59
ca3 = ca(t3)
summary(ca3)
ca3
plot(ca3)

plot(ca3, mass=T, contrib="absolute", 
     map="rowgreen", arrows=c(F, T))

mosaic(t3, shade=TRUE, legend=TRUE) 



#CA on t4 - CA between Male and Age group 30-59
ca4 = ca(t4)
summary(ca4)
ca4
plot(ca4)

plot(ca4, mass=T, contrib="absolute", 
     map="rowgreen", arrows=c(F, T))

mosaic(t4, shade=TRUE, legend=TRUE) 



#CA on t5 - CA between Female and Age group 60-89
ca5 = ca(t5)
summary(ca5)
ca5
plot(ca5)

plot(ca5, mass=T, contrib="absolute", 
     map="rowgreen", arrows=c(F, T))

mosaic(t5, shade=TRUE, legend=TRUE) 











#Use package corrplot to visualize correlation between pairs of predictor variables within CountPerformance and RatePerformance dateframes 
corrplot(cor(CountPerformance), method="ellipse")
corrplot(cor(RatePerformance), method="ellipse")



#Factor analysis - For categorical predictor variables such as Age and Gender
#Run this with a new subset that only contain categorical variables of Age, Gender 
fit = factanal(newbfi, 5)
print(fit$loadings, cutoff=.4, sort=T)



#All age groups
summary(RawPerformance$patientage0to9)
summary(RawPerformance$patientage10to19)
summary(RawPerformance$patientage20to29)
summary(RawPerformance$patientage30to39)
summary(RawPerformance$patientage40to49)
summary(RawPerformance$patientage50to59)
summary(RawPerformance$patientage60to69)
summary(RawPerformance$patientage70to79)
summary(RawPerformance$patientage80to89)
summary(RawPerformance$patientage90to99)
summary(RawPerformance$patientage100to109)
summary(RawPerformance$patientage110to119)

