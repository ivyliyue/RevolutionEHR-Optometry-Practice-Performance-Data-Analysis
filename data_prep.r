
#
ActualData <- read.csv("C:\\Repos\\Depaul\\CSC423\\Project\\practice_performance.csv", header=TRUE)
RawPerformance <- read.csv("C:\\Repos\\Depaul\\CSC423\\Project\\practice_performance.csv", header=TRUE)


#
# This batch of cleaning is all about row removal.  I utilize subset and a boolean operator to select data
# from the list.  I grabbed a count of the subset along the way so that we can record the impact of the data removal.
#
count_pre_cleaning <- nrow(RawPerformance)

#
# For the first bit, require sales and receipts amounts that are all greater than zero.
#
RawPerformance <- subset(RawPerformance, salesbyproviderall > 0)
RawPerformance <- subset(RawPerformance, salesbyproviderinsurance > 0)
RawPerformance <- subset(RawPerformance, salesbyproviderpatient > 0)
RawPerformance <- subset(RawPerformance, receiptsbyproviderall > 0)
RawPerformance <- subset(RawPerformance, receiptsbyproviderinsurance > 0)
RawPerformance <- subset(RawPerformance, receiptsbyproviderpatient > 0)
count_after_cleaning_sales <- nrow(RawPerformance)

#
# In order to use the count data, need to have some counts.
#
# I think several of the count variables can be turned into categorical behavior.  One such count would be
# the eyeglasscaptureratebyorder vs. the eyeglasscaptureratebybilling.  The different between byorder and
# bybilling is subtle.  In effect, the bybilling is the more restrictive of the two.  It requires eyeglass
# orders that are "properly" linked to invoices.  So we could use just the byorder rate and include a
# 1/0 value indicating if the sample makes use of the order to billing linking.
#
RawPerformance <- subset(RawPerformance, eyeglasscaptureratebyorder > 0)
RawPerformance <- subset(RawPerformance, arcaptureratebyorder > 0)
RawPerformance <- subset(RawPerformance, contactlenscapturerate > 0)
RawPerformance <- subset(RawPerformance, medicalmanagementratio > 0)
RawPerformance <- subset(RawPerformance, ratio9921x > 0)
RawPerformance <- subset(RawPerformance, newpatientpercentage > 0)
RawPerformance <- subset(RawPerformance, productionbooked > 0)
RawPerformance <- subset(RawPerformance, productioncompleted > 0)
count_after_cleaning_rates <- nrow(RawPerformance)

#
# This pulls forward just the practices with annual sales between 500k and 6000k
#
RawPerformance <- subset(RawPerformance, salesbyproviderall <= 1500000)
RawPerformance <- subset(RawPerformance, salesbyproviderall >= 125000)
count_after_cleaning_annual <- nrow(RawPerformance)

#
# The removal here is all about scrubbing the greatest outliers.  The first two lines remove samples
# with a high difference between sales and recieipts.  When the differences are outside of the range, it
# is typically a sign that something is off at the practice.  The final test excludes practices with
# extremely high accountsRecievable, meaning that they have a lot more outstanding invoices than they
# have sales numbers.
#
RawPerformance <- subset(RawPerformance, salesbyproviderinsurance/receiptsbyproviderinsurance < 5)
RawPerformance <- subset(RawPerformance, salesbyproviderinsurance/receiptsbyproviderinsurance > 0.2)
RawPerformance <- subset(RawPerformance, accountsreceivableall < salesbyproviderall * 1.5)
count_after_cleaning_ledgers <- nrow(RawPerformance)

#
# Building a summary of the rows removed so that we know the impact.
#
DataRemoved <- data.frame(step = "Remove Negative Dollar Rows", records = (count_pre_cleaning - count_after_cleaning_sales))
DataRemoved <- rbind(DataRemoved, data.frame(step = "Remove Zero-Value Rates", records = (count_after_cleaning_sales - count_after_cleaning_rates)))
DataRemoved <- rbind(DataRemoved, data.frame(step = "Remove Outlying Annual Revenue", records = (count_after_cleaning_rates - count_after_cleaning_annual)))
DataRemoved <- rbind(DataRemoved, data.frame(step = "Remove Outlying AR", records = (count_after_cleaning_annual - count_after_cleaning_ledgers)))
DataRemoved <- rbind(DataRemoved, data.frame(step = "Total Removed", records = (count_pre_cleaning - count_after_cleaning_ledgers)))

#The rm() call here is all about deleting unnecessary variables from the environment
rm(
    count_pre_cleaning,
    count_after_cleaning_sales,
    count_after_cleaning_rates,
    count_after_cleaning_annual,
    count_after_cleaning_ledgers
)

#
# Fixing the ratio9921x so that it makes sense as a rate.  The business unit wanted a ratio, not a true
# percentage calculation.
#
ratio9921xdenominator = RawPerformance$ratio9921xdenominator + RawPerformance$ratio9921xnumerator
ratio9921x = RawPerformance$ratio9921xnumerator / ratio9921xdenominator
RawPerformance <- subset(RawPerformance, select=-c(ratio9921x, ratio9921xdenominator))
RawPerformance <- cbind(RawPerformance, ratio9921x, ratio9921xdenominator)
rm(ratio9921x, ratio9921xdenominator)

#
# For my analysis, I divided the data into count performance vs. rate performance.  For these metrics
# rate = numerator / denominator.
#
CountPerformance <- subset(RawPerformance, select=c(
    arcaptureratebyorderdenominator,
    arcaptureratebyordernumerator,
    contactlenscaptureratedenominator,
    contactlenscaptureratenumerator,
    eyeglasscaptureratebyorderdenominator,
    eyeglasscaptureratebyordernumerator,
    medicalmanagementratiodenominator,
    medicalmanagementrationumerator,
    newpatientpercentagedenominator,
    newpatientpercentagenumerator,
    productionbookednumerator,
    productionbookeddenominator,
    productioncompletednumerator,
    productioncompleteddenominator,
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
    patientgendermale,
    patientgenderunknown,
    ratio9921xdenominator,
    ratio9921xnumerator,
    receiptsbyproviderinsurance
))

RatePerformance <- subset(RawPerformance, select=c(
    arcaptureratebyorder,
    contactlenscapturerate,
    eyeglasscaptureratebyorder,
    medicalmanagementratio,
    newpatientpercentage,
    ratio9921x,
    productionbooked,
    productioncompleted,
    receiptsbyproviderinsurance
    ))

#
# As gender and age buckets are counts only, we calculate the relative size vs the total number of patients.
#
totalpatients = RawPerformance$patientgenderfemale + RawPerformance$patientgendermale + RawPerformance$patientgenderunknown
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
patientgenderunknown = RawPerformance$patientgenderunknown / totalpatients

RatePerformance = cbind(RatePerformance,
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
    patientgendermale,
    patientgenderunknown
    )

set.seed(42)
TrainingSet=sample(nrow(RawPerformance),nrow(RawPerformance)*0.8)
