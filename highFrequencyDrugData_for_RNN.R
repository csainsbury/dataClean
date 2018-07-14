# source("~/R/_workingDirectory/_perAdmissionRewriteDataTableFunctions.R")
# library(gtools)
# library(igraph)
library(data.table)

id_per_location <- function(ID) {
  return(length(unique(ID)))
}

flagMove <- function(ID, charL) {
  
  charLreport <- charL
  charLnumeric <- as.numeric(factor(charL))
  
  testFrame <- data.frame(charLreport, charLnumeric)
  
  testFrame$flagMove <- 0
  testFrame$flagMove[1:nrow(testFrame)-1] <- diff(testFrame$charLnumeric)
  testFrame$nextL <- c("spacer")
  testFrame$nextL[1:(nrow(testFrame)-1)] <- charLreport[2:length(charLreport)]
  
  testFrame$charLreport <- as.character(factor(charL))
  
  outputList <- list(testFrame$charLreport, testFrame$nextL, testFrame$flagMove)
  
  return(outputList)
  
}

returnUnixDateTime<-function(date) {
  returnVal<-as.numeric(as.POSIXct(date, format="%Y-%m-%d", tz="GMT"))
  return(returnVal)
}

findSimilarDrugs <- function(inputFrame) {
  
  # inputFrame <- interestSet
  # inputFrame <- inputFrame[1:10000,]
  
  inputFrame$DrugName.original <- inputFrame$DrugName
  inputFrame$DrugNameNew <- inputFrame$DrugName
  
  inputFrame <- subset(inputFrame, DrugNameNew != "Disposable")
  
  inputFrame$DrugNameNew[grep("Glucose", inputFrame$DrugName, ignore.case = TRUE)] <- "Glucose"
  inputFrame$DrugNameNew[grep("Glucogel", inputFrame$DrugName, ignore.case = TRUE)] <- "Glucose"
  
  inputFrame$DrugNameNew[grep("Glucagen Hypokit", inputFrame$DrugName, ignore.case = TRUE)] <- "Glucagon"
  inputFrame$DrugNameNew[grep("Optium Plus", inputFrame$DrugName, ignore.case = TRUE)] <- "Test Strips"
  
  
  inputFrame$DrugNameNew[grep("Metformin", inputFrame$DrugName, ignore.case = TRUE)] <- "Metformin"
  inputFrame$DrugNameNew[grep("Glucophage", inputFrame$DrugName, ignore.case = TRUE)] <- "Metformin"
  
  inputFrame$DrugNameNew[grep("Gliclazide", inputFrame$DrugName, ignore.case = TRUE)] <- "Gliclazide"
  inputFrame$DrugNameNew[grep("Diamicron", inputFrame$DrugName, ignore.case = TRUE)] <- "Gliclazide"
  
  inputFrame$DrugNameNew[grep("Rosiglitazone", inputFrame$DrugName, ignore.case = TRUE)] <- "Rosiglitazone"
  inputFrame$DrugNameNew[grep("Avandia", inputFrame$DrugName, ignore.case = TRUE)] <- "Rosiglitazone"
  
  inputFrame$DrugNameNew[grep("Linagliptin", inputFrame$DrugName, ignore.case = TRUE)] <- "Linagliptin"
  
  inputFrame$DrugNameNew[grep("Victoza", inputFrame$DrugName, ignore.case = TRUE)] <- "Liraglutide"
  inputFrame$DrugNameNew[grep("Liraglutide", inputFrame$DrugName, ignore.case = TRUE)] <- "Liraglutide"
  
  inputFrame$DrugNameNew[grep("Pioglitazone", inputFrame$DrugName, ignore.case = TRUE)] <- "Pioglitazone"
  
  inputFrame$DrugNameNew[grep("Sitagliptin", inputFrame$DrugName, ignore.case = TRUE)] <- "Sitagliptin"
  inputFrame$DrugNameNew[grep("Januvia", inputFrame$DrugName, ignore.case = TRUE)] <- "Sitagliptin"
  
  inputFrame$DrugNameNew[grep("Dapagliflozin", inputFrame$DrugName, ignore.case = TRUE)] <- "Dapagliflozin"
  
  inputFrame$DrugNameNew[grep("Humalog Mix25", inputFrame$DrugName, ignore.case = TRUE)] <- "Humalog Mix 25"
  
  inputFrame$DrugNameNew[grep("Lantus", inputFrame$DrugName, ignore.case = TRUE)] <- "Insulin Glargine"
  inputFrame$DrugNameNew[grep("Levemir", inputFrame$DrugName, ignore.case = TRUE)] <- "Insulin Detemir"
  
  inputFrame$DrugNameNew[grep("Insulatard", inputFrame$DrugName, ignore.case = TRUE)] <- "Insulatard"
  
  inputFrame$DrugNameNew[grep("Actrapid", inputFrame$DrugName, ignore.case = TRUE)] <- "Actrapid"
  inputFrame$DrugNameNew[grep("Humalog 100units/ml solution", inputFrame$DrugName, ignore.case = TRUE)] <- "Humalog"
  
  inputFrame$DrugNameNew[grep("Novorapid", inputFrame$DrugName, ignore.case = TRUE)] <- "Novorapid"
  
  inputFrame$DrugNameNew[grep("Novomix 30", inputFrame$DrugName, ignore.case = TRUE)] <- "Novomix 30"
  
  inputFrame$DrugNameNew[grep("Mixtard 30", inputFrame$DrugName, ignore.case = TRUE)] <- "Mixtard 30"
  inputFrame$DrugNameNew[grep("Mixtard 20", inputFrame$DrugName, ignore.case = TRUE)] <- "Mixtard 20"
  
  inputFrame$DrugNameNew[grep("Humulin M3", inputFrame$DrugName, ignore.case = TRUE)] <- "Humulin M3"
  
  inputFrame$DrugNameNew[grep("Humalog Mix50", inputFrame$DrugName, ignore.case = TRUE)] <- "Humalog Mix50"
  
  inputFrame$DrugNameNew[grep("strip", inputFrame$DrugName, ignore.case = TRUE)] <- "Test Strips"
  
  inputFrame$DrugNameNew[grep("Bd-Microfine", inputFrame$DrugName, ignore.case = TRUE)] <- "Needle"
  inputFrame$DrugNameNew[grep("Needle", inputFrame$DrugName, ignore.case = TRUE)] <- "Needle"
  
  
  outputFrame <- inputFrame
  
  outputFrame$DrugName.original <- NULL
  outputFrame$DrugName <- outputFrame$DrugNameNew
  outputFrame$DrugNameNew <- NULL
  
  return(outputFrame)
}

simplifyDrugs <- function(inputFrame) {
  
  # inputFrame <- interestSet
  # inputFrame <- inputFrame[1:100000,]
  
  inputFrame$DrugName.original <- inputFrame$DrugName
  inputFrame$DrugNameNew <- inputFrame$DrugName
  
  inputFrame <- subset(inputFrame, DrugNameNew != "Disposable")
  
  inputFrame$DrugNameNew[grep("Glucose", inputFrame$DrugName, ignore.case = TRUE)] <- "Glucose"
  inputFrame$DrugNameNew[grep("Glucogel", inputFrame$DrugName, ignore.case = TRUE)] <- "Glucose"
  inputFrame$DrugNameNew[grep("Dextrogel", inputFrame$DrugName, ignore.case = TRUE)] <- "Glucose"
  inputFrame$DrugNameNew[grep("DEXTROSE", inputFrame$DrugName, ignore.case = TRUE)] <- "Glucose"
  
  
  inputFrame$DrugNameNew[grep("Glucagen Hypokit", inputFrame$DrugName, ignore.case = TRUE)] <- "Glucagon"
  inputFrame$DrugNameNew[grep("Glucagon", inputFrame$DrugName, ignore.case = TRUE)] <- "Glucagon"
  
  inputFrame$DrugNameNew[grep("Optium Plus", inputFrame$DrugName, ignore.case = TRUE)] <- "Test Strips"
  
  inputFrame$DrugNameNew[grep("Acarbose", inputFrame$DrugName, ignore.case = TRUE)] <- "Acarbose"
  inputFrame$DrugNameNew[grep("Glucobay", inputFrame$DrugName, ignore.case = TRUE)] <- "Acarbose"
  
  inputFrame$DrugNameNew[grep("REPAGLINIDE", inputFrame$DrugName, ignore.case = TRUE)] <- "meglitinide"
  inputFrame$DrugNameNew[grep("Repaglinide", inputFrame$DrugName, ignore.case = TRUE)] <- "meglitinide"
  inputFrame$DrugNameNew[grep("Nateglinide", inputFrame$DrugName, ignore.case = TRUE)] <- "meglitinide"
  
  inputFrame$DrugNameNew[grep("Metformin", inputFrame$DrugName, ignore.case = TRUE)] <- "Metformin_"
  inputFrame$DrugNameNew[grep("Glucophage", inputFrame$DrugName, ignore.case = TRUE)] <- "Metformin_"
  
  inputFrame$DrugNameNew[grep("Gliclazide", inputFrame$DrugName, ignore.case = TRUE)] <- "SU_"
  inputFrame$DrugNameNew[grep("Glipizide", inputFrame$DrugName, ignore.case = TRUE)] <- "SU_"
  inputFrame$DrugNameNew[grep("Glibenese", inputFrame$DrugName, ignore.case = TRUE)] <- "SU_"
  inputFrame$DrugNameNew[grep("Amaryl", inputFrame$DrugName, ignore.case = TRUE)] <- "SU_"
  inputFrame$DrugNameNew[grep("Glimepiride", inputFrame$DrugName, ignore.case = TRUE)] <- "SU_"
  inputFrame$DrugNameNew[grep("GLIMEPIRIDE", inputFrame$DrugName, ignore.case = TRUE)] <- "SU_"
  inputFrame$DrugNameNew[grep("Glibenclamide", inputFrame$DrugName, ignore.case = TRUE)] <- "SU_"
  inputFrame$DrugNameNew[grep("Daonil", inputFrame$DrugName, ignore.case = TRUE)] <- "SU_"

  inputFrame$DrugNameNew[grep("Rosiglitazone", inputFrame$DrugName, ignore.case = TRUE)] <- "TZD_"
  inputFrame$DrugNameNew[grep("Pioglitazone", inputFrame$DrugName, ignore.case = TRUE)] <- "TZD_"
  inputFrame$DrugNameNew[grep("Actos", inputFrame$DrugName, ignore.case = TRUE)] <- "TZD_"
  
  inputFrame$DrugNameNew[grep("Linagliptin", inputFrame$DrugName, ignore.case = TRUE)] <- "DPP4_"
  inputFrame$DrugNameNew[grep("Trajenta", inputFrame$DrugName, ignore.case = TRUE)] <- "DPP4_"
  inputFrame$DrugNameNew[grep("Sitagliptin", inputFrame$DrugName, ignore.case = TRUE)] <- "DPP4_"
  inputFrame$DrugNameNew[grep("Januvia", inputFrame$DrugName, ignore.case = TRUE)] <- "DPP4_"

  inputFrame$DrugNameNew[grep("Vildagliptin", inputFrame$DrugName, ignore.case = TRUE)] <- "DPP4_"
  inputFrame$DrugNameNew[grep("Galvus", inputFrame$DrugName, ignore.case = TRUE)] <- "DPP4_"
  inputFrame$DrugNameNew[grep("Saxagliptin", inputFrame$DrugName, ignore.case = TRUE)] <- "DPP4_"
  inputFrame$DrugNameNew[grep("SAXAGLIPTIN", inputFrame$DrugName, ignore.case = TRUE)] <- "DPP4_"
  inputFrame$DrugNameNew[grep("Dulaglutide", inputFrame$DrugName, ignore.case = TRUE)] <- "DPP4_"
  
  inputFrame$DrugNameNew[grep("Liraglutide", inputFrame$DrugName, ignore.case = TRUE)] <- "GLP1_"
  inputFrame$DrugNameNew[grep("Victoza", inputFrame$DrugName, ignore.case = TRUE)] <- "GLP1_"
  inputFrame$DrugNameNew[grep("Exenatide", inputFrame$DrugName, ignore.case = TRUE)] <- "GLP1_"
  inputFrame$DrugNameNew[grep("Byetta", inputFrame$DrugName, ignore.case = TRUE)] <- "GLP1_"
  inputFrame$DrugNameNew[grep("BYETTA", inputFrame$DrugName, ignore.case = TRUE)] <- "GLP1_"
  inputFrame$DrugNameNew[grep("Bydureon", inputFrame$DrugName, ignore.case = TRUE)] <- "GLP1_"
  inputFrame$DrugNameNew[grep("Lixisenatide", inputFrame$DrugName, ignore.case = TRUE)] <- "GLP1_"
  inputFrame$DrugNameNew[grep("Lyxumia", inputFrame$DrugName, ignore.case = TRUE)] <- "GLP1_"
  inputFrame$DrugNameNew[grep("Dulaglutide", inputFrame$DrugName, ignore.case = TRUE)] <- "GLP1_"
  
  inputFrame$DrugNameNew[grep("Dapagliflozin", inputFrame$DrugName, ignore.case = TRUE)] <- "SGLT2_"
  inputFrame$DrugNameNew[grep("Forxiga", inputFrame$DrugName, ignore.case = TRUE)] <- "SGLT2_"
  inputFrame$DrugNameNew[grep("Canagliflozin", inputFrame$DrugName, ignore.case = TRUE)] <- "SGLT2_"
  inputFrame$DrugNameNew[grep("Invokana", inputFrame$DrugName, ignore.case = TRUE)] <- "SGLT2_"
  inputFrame$DrugNameNew[grep("Empagliflozin", inputFrame$DrugName, ignore.case = TRUE)] <- "SGLT2_"
  inputFrame$DrugNameNew[grep("Jardiance", inputFrame$DrugName, ignore.case = TRUE)] <- "SGLT2_"
  
  # inputFrame$DrugNameNew[grep("Dapagliflozin", inputFrame$DrugName, ignore.case = TRUE)] <- "SGLT2dapa_"
  # inputFrame$DrugNameNew[grep("Forxiga", inputFrame$DrugName, ignore.case = TRUE)] <- "SGLT2dapa_"
  # inputFrame$DrugNameNew[grep("Canagliflozin", inputFrame$DrugName, ignore.case = TRUE)] <- "SGLT2cana_"
  # inputFrame$DrugNameNew[grep("Invokana", inputFrame$DrugName, ignore.case = TRUE)] <- "SGLT2cana_"
  # inputFrame$DrugNameNew[grep("Empagliflozin", inputFrame$DrugName, ignore.case = TRUE)] <- "SGLT2empa_"
  # inputFrame$DrugNameNew[grep("Jardiance", inputFrame$DrugName, ignore.case = TRUE)] <- "SGLT2empa_"
  
  # combinations
  inputFrame$DrugNameNew[grep("Avandamet", inputFrame$DrugName, ignore.case = TRUE)] <- "MetforminTZD_"
  inputFrame$DrugNameNew[grep("Competact", inputFrame$DrugName, ignore.case = TRUE)] <- "MetforminTZD_"
  inputFrame$DrugNameNew[grep("Eucreas", inputFrame$DrugName, ignore.case = TRUE)] <- "DPP4Metformin_"
  inputFrame$DrugNameNew[grep("EUCREAS", inputFrame$DrugName, ignore.case = TRUE)] <- "DPP4Metformin_"
  inputFrame$DrugNameNew[grep("Janumet", inputFrame$DrugName, ignore.case = TRUE)] <- "DPP4Metformin_"
  inputFrame$DrugNameNew[grep("JANUMET", inputFrame$DrugName, ignore.case = TRUE)] <- "DPP4Metformin_"
  
  inputFrame$DrugNameNew[grep("ideglira", inputFrame$DrugName, ignore.case = TRUE)] <- "ideglira"
  inputFrame$DrugNameNew[grep("xultophy", inputFrame$DrugName, ignore.case = TRUE)] <- "ideglira"

  # SSRI
  inputFrame$DrugNameNew[grep("Duloxetine", inputFrame$DrugName, ignore.case = TRUE)] <- "SSRI"
  inputFrame$DrugNameNew[grep("DULOXETINE", inputFrame$DrugName, ignore.case = TRUE)] <- "SSRI"
  
  
  # bd mix insulins
  inputFrame$DrugNameNew[grep("Humalog Mix", inputFrame$DrugName, ignore.case = TRUE)] <- "BDmixInsulin_"
  inputFrame$DrugNameNew[grep("Novomix", inputFrame$DrugName, ignore.case = TRUE)] <- "BDmixInsulin_"
  inputFrame$DrugNameNew[grep("Mixtard", inputFrame$DrugName, ignore.case = TRUE)] <- "BDmixInsulin_"
  inputFrame$DrugNameNew[grep("Humulin M4", inputFrame$DrugName, ignore.case = TRUE)] <- "BDmixInsulin_"
  
  inputFrame$DrugNameNew[grep("Humulin M3", inputFrame$DrugName, ignore.case = TRUE)] <- "BDmixInsulin_"
  inputFrame$DrugNameNew[grep("Humulin M2", inputFrame$DrugName, ignore.case = TRUE)] <- "BDmixInsulin_"
  inputFrame$DrugNameNew[grep("Humulin M1", inputFrame$DrugName, ignore.case = TRUE)] <- "BDmixInsulin_"
  
  inputFrame$DrugNameNew[grep("HUMULIN M2", inputFrame$DrugName, ignore.case = TRUE)] <- "BDmixInsulin_"
  inputFrame$DrugNameNew[grep("HUMULIN M1", inputFrame$DrugName, ignore.case = TRUE)] <- "BDmixInsulin_"
  
  inputFrame$DrugNameNew[grep("Humalog Mix", inputFrame$DrugName, ignore.case = TRUE)] <- "BDmixInsulin_"
  
  # basal insulins
  inputFrame$DrugNameNew[grep("Insulin Glargine", inputFrame$DrugName, ignore.case = TRUE)] <- "BasalInsulin_"
  inputFrame$DrugNameNew[grep("Lantus", inputFrame$DrugName, ignore.case = TRUE)] <- "BasalInsulin_"
  
  inputFrame$DrugNameNew[grep("degludec", inputFrame$DrugName, ignore.case = TRUE)] <- "BasalInsulin_"
  inputFrame$DrugNameNew[grep("Degludec", inputFrame$DrugName, ignore.case = TRUE)] <- "BasalInsulin_"
  inputFrame$DrugNameNew[grep("Tresiba", inputFrame$DrugName, ignore.case = TRUE)] <- "BasalInsulin_"
  inputFrame$DrugNameNew[grep("Insulin Detemir", inputFrame$DrugName, ignore.case = TRUE)] <- "BasalInsulin_"
  inputFrame$DrugNameNew[grep("Insulatard", inputFrame$DrugName, ignore.case = TRUE)] <- "BasalInsulin_"
  inputFrame$DrugNameNew[grep("ULTRATARD", inputFrame$DrugName, ignore.case = TRUE)] <- "BasalInsulin_"  
  inputFrame$DrugNameNew[grep("MONOTARD", inputFrame$DrugName, ignore.case = TRUE)] <- "BasalInsulin_"
  inputFrame$DrugNameNew[grep("Humulin I", inputFrame$DrugName, ignore.case = TRUE)] <- "BasalInsulin_"
  
  # prandial insulins
  inputFrame$DrugNameNew[grep("Actrapid", inputFrame$DrugName, ignore.case = TRUE)] <- "PrandialInsulin_"
  inputFrame$DrugNameNew[grep("Humalog", inputFrame$DrugName, ignore.case = TRUE)] <- "PrandialInsulin_"
  inputFrame$DrugNameNew[grep("Insulin Lispro", inputFrame$DrugName, ignore.case = TRUE)] <- "PrandialInsulin_"
  
  inputFrame$DrugNameNew[grep("Novorapid", inputFrame$DrugName, ignore.case = TRUE)] <- "PrandialInsulin_"
  inputFrame$DrugNameNew[grep("aspart", inputFrame$DrugName, ignore.case = TRUE)] <- "PrandialInsulin_"
  inputFrame$DrugNameNew[grep("fiasp", inputFrame$DrugName, ignore.case = TRUE)] <- "PrandialInsulin_"
  
  inputFrame$DrugNameNew[grep("Apidra", inputFrame$DrugName, ignore.case = TRUE)] <- "PrandialInsulin_"
  inputFrame$DrugNameNew[grep("Humulin S", inputFrame$DrugName, ignore.case = TRUE)] <- "PrandialInsulin_"
  
  # animal insulins
  inputFrame$DrugNameNew[grep("Bovine", inputFrame$DrugName, ignore.case = TRUE)] <- "animalInsulin"
  inputFrame$DrugNameNew[grep("BOVINE", inputFrame$DrugName, ignore.case = TRUE)] <- "animalInsulin"
  inputFrame$DrugNameNew[grep("Porcine", inputFrame$DrugName, ignore.case = TRUE)] <- "animalInsulin"
  
  inputFrame$DrugNameNew[grep("lancet", inputFrame$DrugName, ignore.case = TRUE)] <- "lancet"
  inputFrame$DrugNameNew[grep("Lancet", inputFrame$DrugName, ignore.case = TRUE)] <- "lancet"
  inputFrame$DrugNameNew[grep("LANCET", inputFrame$DrugName, ignore.case = TRUE)] <- "lancet"
  inputFrame$DrugNameNew[grep("Accu", inputFrame$DrugName, ignore.case = TRUE)] <- "lancet"
  inputFrame$DrugNameNew[grep("ACCU", inputFrame$DrugName, ignore.case = TRUE)] <- "lancet"
  inputFrame$DrugNameNew[grep("accu", inputFrame$DrugName, ignore.case = TRUE)] <- "lancet"
  
  inputFrame$DrugNameNew[grep("pen", inputFrame$DrugName, ignore.case = TRUE)] <- "pen"
  inputFrame$DrugNameNew[grep("Insulin Syringe", inputFrame$DrugName, ignore.case = TRUE)] <- "pen"
  
  inputFrame$DrugNameNew[grep("strip", inputFrame$DrugName, ignore.case = TRUE)] <- "TestStrips"
  
  inputFrame$DrugNameNew[grep("Bd-Microfine", inputFrame$DrugName, ignore.case = TRUE)] <- "Needle"
  inputFrame$DrugNameNew[grep("BD Micro-Fine", inputFrame$DrugName, ignore.case = TRUE)] <- "Needle"
  inputFrame$DrugNameNew[grep("Needle", inputFrame$DrugName, ignore.case = TRUE)] <- "Needle"
  inputFrame$DrugNameNew[grep("need", inputFrame$DrugName, ignore.case = TRUE)] <- "Needle"
  inputFrame$DrugNameNew[grep("ndle", inputFrame$DrugName, ignore.case = TRUE)] <- "Needle"
  
  inputFrame$DrugNameNew[grep("Bd Safe-Clip", inputFrame$DrugName, ignore.case = TRUE)] <- "ClippingDevice"
  
  x <- as.data.frame(table(inputFrame$DrugNameNew))
  x = x[order(x$Freq), ]
  
  outputFrame <- inputFrame
  
  outputFrame$DrugName.original <- NULL
  outputFrame$DrugName <- outputFrame$DrugNameNew
  outputFrame$DrugNameNew <- NULL
  
  return(outputFrame)
}


# generate node and link files
drugDataSet <- read.csv("~/R/GlCoSy/SDsource/Export_all_diabetes_drugs.txt",header=TRUE,row.names=NULL)
topUpDrugData <-paste("~/R/_workingDirectory/nEqOneTrial/rawData/diabetesDrugs_nov16-nov17.txt",sep="")
topUpDrugDataSet <- read.csv(topUpDrugData)

concatDrugSet <- rbind(drugDataSet, topUpDrugDataSet)
concatDrugSet <- unique(concatDrugSet)

drugDataSet <- concatDrugSet



# load and process mortality data

diagnosisSet<-paste("~/R/_workingDirectory/nEqOneTrial/cleanedData/deathData.csv",sep="")
diagnosisSetDF<-read.csv(diagnosisSet); diagnosisSetDT<-data.table(diagnosisSetDF)

deathData = diagnosisSetDF
# deathData <- read.csv("~/R/GlCoSy/SDsource/diagnosisDateDeathDate.txt", sep=",")
deathData$unix_deathDate <- returnUnixDateTime(deathData$DeathDate)
deathData$unix_deathDate[is.na(deathData$unix_deathDate)] <- 0
deathData$isDead <- ifelse(deathData$unix_deathDate > 0, 1, 0)
deathData$unix_diagnosisDate <- returnUnixDateTime(deathData$DateOfDiagnosis)

# drugDataSet <- read.csv("~/R/GlCoSy/SDsource/test_drug_out_second100kIDs_allTime.txt",header=TRUE,row.names=NULL)
drugDataSet$BNFCode <- as.character(drugDataSet$BNFCode)
drugDataSet$DrugName <- as.character(drugDataSet$DrugName)
drugDataSet$LinkId <- as.numeric(levels(drugDataSet$LinkId))[drugDataSet$LinkId]
# drugDataSet$LinkId <- as.numeric(drugDataSet$LinkId)
# drugDataSet <- read.csv("./test_drug_out_second100kIDs_allTime.txt",header=TRUE,row.names=NULL)

# restrict to diabetes drugs
interestSet <- subset(drugDataSet, substr(drugDataSet$BNFCode,1,3) == "6.1" | substr(drugDataSet$BNFCode,1,4) == "0601")

# restrict to prescriptions within dates
interestSetDT <- data.table(interestSet)
interestSetDT$prescription_dateplustime1 <- returnUnixDateTime(interestSetDT$PrescriptionDateTime)
interestSetDT <- interestSetDT[prescription_dateplustime1 < as.numeric(Sys.time())]

# set runin period of interest
startRuninPeriod <- '2011-01-01'
endRuninPeriod   <- '2017-01-01'
interestSetDT <- interestSetDT[prescription_dateplustime1 > returnUnixDateTime(startRuninPeriod) &
                                 prescription_dateplustime1 < returnUnixDateTime(endRuninPeriod)]

interestSetDF <- data.frame(interestSetDT)

# run simplifying functions
interestSetDF <- findSimilarDrugs(interestSetDF)
interestSetDF <- simplifyDrugs(interestSetDF)

## remove unaltered drugs
## if drug name has '_' after it in simplifydrugs() it will be retained.
interestSetDF$retain <- 0
interestSetDF$retain[grep("_", interestSetDF$DrugName, ignore.case = TRUE)] <- 1
interestSetDF <- subset(interestSetDF, retain == 1)
interestSetDF$retain <- NULL

# generate a top-25 etc list for merging back
# meeds a bit of data cleaning - merging synonymous drugs etc
n = 25
topNdrugs_DrugNames <- as.data.frame(table(interestSetDF$DrugName))
topNdrugs_DrugNames <- topNdrugs_DrugNames[order(topNdrugs_DrugNames$Freq), ]

topNdrugs <- tail(topNdrugs_DrugNames, n)

topNdrugs$Var1 <- gsub(" ", "", topNdrugs$Var1, fixed = TRUE)
topNdrugs$Var1 <- gsub("/", "", topNdrugs$Var1, fixed = TRUE)
topNdrugs$Var1 <- gsub("-", "", topNdrugs$Var1, fixed = TRUE)

# plot zipf's law
# topNdrugs$rank = c(nrow(topNdrugs):1)
# plot(log(topNdrugs$rank), log(topNdrugs$Freq))

# plot prescriptions over time
# empaHist <- hist(subset(interestSetDF, DrugName == 'SGLT2empa_')$prescription_dateplustime1, xlim = c(min(interestSetDF$prescription_dateplustime1), max(interestSetDF$prescription_dateplustime1)), breaks = 100)

# merge top drugs back with interestSet to generate working data frame:
interestSet_topN_merge <- merge(interestSetDF, topNdrugs, by.x="DrugName", by.y="Var1")

# write.table(interestSet_topN_merge, file = "~/R/_workingDirectory/dataClean/drugData_tempIntermediate.csv", sep = ",", row.names = FALSE, col.names = TRUE)


###############################
## start drug data manipulation
###############################
# interestSet_topN_merge <- read.csv("~/R/_workingDirectory/dataClean/drugData_tempIntermediate.csv", header = T)

drugsetDT <- data.table(interestSet_topN_merge)
drugsetDT$prescription_dateplustime1 <- returnUnixDateTime(drugsetDT$PrescriptionDateTime)
drugsetDT_original <-drugsetDT # preserve an original full dataset incase needed

drugsetDT <- transform(drugsetDT,id=as.numeric(factor(LinkId)))

# time bins derived in same was as in splineInterpolate.R
startDate_unix <- returnUnixDateTime(startRuninPeriod)
endDate_unix <- returnUnixDateTime(endRuninPeriod)
nBins = 72 # number of bins wanted
nBins = nBins - 1

# set time bins
binLengthSeconds = (endDate_unix - startDate_unix) / nBins
binLengthMonths = binLengthSeconds / (60*60*24*(365.25/12))
sequence <- seq(startDate_unix, endDate_unix, binLengthSeconds)

# generate bag of drugs frame
drugWordFrame <- as.data.frame(matrix(nrow = length(unique(drugsetDT$LinkId)), ncol = (length(sequence)) ))
colnames(drugWordFrame) <- c(1:(length(sequence)))
drugWordFrame$LinkId <- 0
    
    # function to generate drugwords for each time interval
    returnIntervals <- function(LinkId, DrugName, prescription_dateplustime1, sequence, id) {
      
      # DrugName <- subset(drugsetDT, id == 2)$DrugName; prescription_dateplustime1 <- subset(drugsetDT, id == 2)$prescription_dateplustime1; id = 2; LinkId <- subset(drugsetDT, id == 2)$LinkId
      
          inputSet <- data.table(DrugName, prescription_dateplustime1)
      
          ## add nil values to fill time slots without any drugs
          nilFrame <- as.data.frame(matrix(nrow = length(sequence), ncol = ncol(inputSet)))
          colnames(nilFrame) <- colnames(inputSet)
          
          nilFrame$DrugName <- 'nil'
          nilFrame$prescription_dateplustime1 <- sequence
          
          outputSet <- rbind(nilFrame, inputSet)
          
      ## generate drug words
          
      interimSet <- outputSet
      
      interimSet <- interimSet[, interv := cut(prescription_dateplustime1, sequence)][, .(drugs = (unique(DrugName))), by = interv]
      interimSet[, drugWord := paste(drugs, collapse = ''), by = interv]
      
      interimSet <- interimSet[order(interimSet$interv), ]
      interimSet[, drugSequenceNumber := seq(1, .N, 1), by = interv]
      
      reportSet <- interimSet[drugSequenceNumber == 1]
      reportSet$drugWord <- ifelse(substr(reportSet$drugWord,1,3) == 'nil' & nchar(reportSet$drugWord) == 3, reportSet$drugWord, substr(reportSet$drugWord,4,nchar(reportSet$drugWord)))
      
      reportSet <- reportSet[1:nrow(reportSet), ]
      reportSet$intervalNumber <- c(1:nrow(reportSet))
      
#      print(reportSet$drugWord)
      
      return(c(reportSet$drugWord, LinkId[1]))
      

    }
    
    print(max(drugsetDT$id))
    for (j in seq(1, max(drugsetDT$id), )) {
    # for (j in seq(1, 100, )) {
      
      if(j%%100 == 0) {print(j)}
      
      injectionSet <- drugsetDT[id == j]
      drugWordFrame[j, ] <- returnIntervals(injectionSet$LinkId, injectionSet$DrugName, injectionSet$prescription_dateplustime1, sequence, j)
    }
  
    # write.table(drugWordFrame, file = "~/R/_workingDirectory/dataClean/drugWordFrame_withID_2011_2017_72bins.csv", sep=",")
    # drugWordFrame <- read.csv("~/R/GlCoSy/MLsource/drugWordFrame.csv", stringsAsFactors = F, row.names = NULL); drugWordFrame$row.names <- NULL
    
    # the bin frequency is 0.5 that in splineInterpolation.R (72 bins vs 144)
    # need to replicate the dataframe, and then interleave to give 2 bins for every original time bin (bringing total n back to 144)
    drugWordFrame_1 <- drugWordFrame[, 1:72]
    drugWordFrame_2 <- drugWordFrame[, 1:72]
    
    doubledFrame = cbind(drugWordFrame_1, drugWordFrame_2)
    indx <- rbind(names(drugWordFrame_1),names(drugWordFrame_2))
    
    interleavedFrame <- doubledFrame[, indx]
    colnames(interleavedFrame) <- c(1:ncol(interleavedFrame))
    
    # reattach Link ID
    interleavedFrame_withID <- cbind(interleavedFrame, drugWordFrame[, (ncol(drugWordFrame))])
    colnames(interleavedFrame_withID)[ncol(interleavedFrame_withID)] <- c("LinkId")
    
    # here do analysis to select rows (IDs) for later analysis
    # replace existing drugWordFrame with new expanded version
    drugWordFrame <- interleavedFrame_withID
    
    # mortality outcome at 2017-01-01
    drugWordFrame_mortality <- merge(drugWordFrame, deathData, by.x = "LinkId", by.y= "LinkId")
    # remove those dead before end of FU
    # analysis frame = those who are not dead, or those who have died after the end of the runin period. ie all individuals in analysis alive at the end of the runin period
    drugWordFrame_mortality <- subset(drugWordFrame_mortality, isDead == 0 | (isDead == 1 & unix_deathDate > returnUnixDateTime(endRuninPeriod)) )
    # remove those diagnosed after the end of the runin period
    drugWordFrame_mortality <- subset(drugWordFrame_mortality, unix_diagnosisDate <= returnUnixDateTime(endRuninPeriod) )
    
    # set up drug sentences for analysis
      
      drugWordFrame_forAnalysis <- drugWordFrame_mortality
      
      drugWordFrame_drugNames <- drugWordFrame_forAnalysis[, 2:(1+(ncol(drugWordFrame_forAnalysis)-8)) ]
      
    drugSentenceFrame <- as.data.frame(matrix(nrow = nrow(drugWordFrame_forAnalysis), ncol = 1))
    colnames(drugSentenceFrame) <- c("drugSentence")
    
    vectorWords <- as.vector(as.matrix(drugWordFrame_drugNames))
    vectorNumbers <- as.numeric(as.factor(vectorWords))
    lookup <- data.frame(vectorWords, vectorNumbers)
    lookup <- unique(lookup)
    lookup <- data.table(lookup)
    
    # write out lookup table
    write.table(lookup, file = paste("~/R/_workingDirectory/dataClean/lookupTable_", startRuninPeriod, "_to_", endRuninPeriod, "_simplifiedDrugs_", binLengthMonths,"mBins.csv", sep=""), sep=",", row.names = FALSE)
    

    # vectorised lookup table use
    numericalDrugsFrame <- as.data.frame(matrix(0, nrow = nrow(drugWordFrame_drugNames), ncol = ncol(drugWordFrame_drugNames)))
    
    for (jj in seq(1, ncol(drugWordFrame_drugNames), 1)) {
      
      index <- match(drugWordFrame_drugNames[,jj], lookup$vectorWords)
      numericalDrugsFrame[,jj] <- lookup$vectorNumbers[index]
      
    }
    
    y_vector <- drugWordFrame_forAnalysis$isDead
    

    # write out sequence for analysis
    write.table(numericalDrugsFrame, file = paste("~/R/_workingDirectory/dataClean/numericalDrugsFrame_", startRuninPeriod, "_to_", endRuninPeriod, "_simplifiedDrugs_", binLengthMonths,"mBins.csv", sep=""), sep=",", row.names = FALSE)
    
    # write out sequence for analysis
    write.table(drugWordFrame_mortality, file = paste("~/R/_workingDirectory/dataClean/drugWordFrame_mortality_", startRuninPeriod, "_to_", endRuninPeriod, "_simplifiedDrugs_", binLengthMonths,"mBins.csv", sep=""), sep=",", row.names = FALSE)
    
    numericalDrugsFrame_withID <- data.frame(numericalDrugsFrame, drugWordFrame_forAnalysis$LinkId)
    # write out sequence for analysis
    write.table(numericalDrugsFrame_withID, file = paste("~/R/_workingDirectory/dataClean/numericalDrugsFrame_withID_", startRuninPeriod, "_to_", endRuninPeriod, "_simplifiedDrugs_", binLengthMonths,"mBins.csv", sep=""), sep=",", row.names = FALSE)