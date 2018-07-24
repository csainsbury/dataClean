## 
# generate equivalent sets of each with IDs in order
##

returnUnixDateTime<-function(date) {
  returnVal<-as.numeric(as.POSIXct(date, format="%Y-%m-%d", tz="GMT"))
  return(returnVal)
}

# number of follow up years
n_years = 6

# load data back in
hba1c_spline <- read.csv(paste('./hba1c_', n_years, 'y_shf_limitedValues.csv', sep = ""), header = T)
sbp_spline <- read.csv(paste('./sbp_', n_years, 'y_shf_limitedValues.csv', sep = ""), header = T)
dbp_spline <- read.csv(paste('./dbp_', n_years, 'y_shf_limitedValues.csv', sep = ""), header = T)
bmi_spline <- read.csv(paste('./bmi_', n_years, 'y_shf_limitedValues.csv', sep = ""), header = T)

numericalDrugs <- read.csv(paste('./numericalDrugsFrame_withID_', n_years, 'y_shf.csv', sep = ""), header = T)
colnames(numericalDrugs)[ncol(numericalDrugs)] <- c("LinkId")
drugWords <- read.csv(paste('./drugWordFrame_mortality_', n_years, 'y.csv', sep = ""), header = T)
drugWords = drugWords[, 1:289]


# generate LinkId single column frames
hba1cID <- as.data.frame(hba1c_spline$LinkId); colnames(hba1cID) <- c("LinkId")
sbpID <- as.data.frame(sbp_spline$LinkId); colnames(sbpID) <- c("LinkId")
dbpID <- as.data.frame(dbp_spline$LinkId); colnames(dbpID) <- c("LinkId")
bmiID <- as.data.frame(bmi_spline$LinkId); colnames(bmiID) <- c("LinkId")

numericalDrugsID <- as.data.frame(numericalDrugs$LinkId); colnames(numericalDrugsID) <- c("LinkId")
drugWordsID <- as.data.frame(drugWords$LinkId); colnames(drugWordsID) <- c("LinkId")


# find list of all IDs common to all 4 parameters
merge2 = merge(hba1cID, sbpID, by.x = "LinkId", by.y = "LinkId")
merge3 = merge(merge2, dbpID, by.x = "LinkId", by.y = "LinkId")
merge4 = merge(merge3, bmiID, by.x = "LinkId", by.y = "LinkId")

merge_numericalDrugs = merge(merge4, numericalDrugsID, by.x = "LinkId", by.y = "LinkId")
merge_drugWords = merge(merge4, drugWordsID, by.x = "LinkId", by.y = "LinkId")


# generate matched samples in order: numerical drug information
# ie match the _spline files with merge_numericalDrugs
hba1cComplete_numericalDrugData = hba1c_spline[hba1c_spline$LinkId %in% merge_numericalDrugs$LinkId,] 
sbpComplete_numericalDrugData = sbp_spline[sbp_spline$LinkId %in% merge_numericalDrugs$LinkId,] 
dbpComplete_numericalDrugData = dbp_spline[dbp_spline$LinkId %in% merge_numericalDrugs$LinkId,] 
bmiComplete_numericalDrugData = bmi_spline[bmi_spline$LinkId %in% merge_numericalDrugs$LinkId,]
drugs_numericalDrugData = numericalDrugs[numericalDrugs$LinkId %in% merge_numericalDrugs$LinkId,]

# generate matched samples in order: word drug information
# ie match the _spline files with merge_drugWords
hba1cComplete_drugWordData = hba1c_spline[hba1c_spline$LinkId %in% merge_drugWords$LinkId,] 
sbpComplete_drugWordData = sbp_spline[sbp_spline$LinkId %in% merge_drugWords$LinkId,] 
dbpComplete_drugWordData = dbp_spline[dbp_spline$LinkId %in% merge_drugWords$LinkId,] 
bmiComplete_drugWordData = bmi_spline[bmi_spline$LinkId %in% merge_drugWords$LinkId,]
drugs_drugWordData = drugWords[drugWords$LinkId %in% merge_drugWords$LinkId,]

'''
## generate age and gender information
diagnosisDataset<-read.csv("~/R/GlCoSy/SDsource/demogALL.txt", quote = "", row.names = NULL, stringsAsFactors = FALSE)
diagnosisDataset_sub <- data.frame(diagnosisDataset$LinkId, diagnosisDataset$CurrentGender_Mapped, diagnosisDataset$BirthDate, diagnosisDataset$DateOfDiagnosisDiabetes_Date); colnames(diagnosisDataset_sub) <- c("LinkId", "sex", "birthDate", "diagnosisDate")
diagnosisDataset_sub$sex <- ifelse(diagnosisDataset_sub$sex == "Male", 1, 0)
diagnosisDataset_sub$age = returnUnixDateTime("2011-01-01") - (returnUnixDateTime(as.character(diagnosisDataset_sub$birthDate)))
diagnosisDataset_sub$age = diagnosisDataset_sub$age / (60*60*24*365.25)
diagnosisDataset_sub$duration = returnUnixDateTime("2011-01-01") - (returnUnixDateTime(as.character(diagnosisDataset_sub$diagnosisDate)))
diagnosisDataset_sub$duration = diagnosisDataset_sub$duration / (60*60*24*365.25)
diagnosisDataset_sub$birthDate <- NULL
diagnosisDataset_sub$diagnosisDate <- NULL
'''
diagnosisDataset_sub <- read.csv("./diagnosisDataset_sub.csv", header = T)


numerical_diagnosisDataset_X = diagnosisDataset_sub[diagnosisDataset_sub$LinkId %in% merge_numericalDrugs$LinkId,]
words_diagnosisDataset_X = diagnosisDataset_sub[diagnosisDataset_sub$LinkId %in% merge_drugWords$LinkId,]

# interpolate missing duration with median
numerical_diagnosisDataset_X$duration[is.na(numerical_diagnosisDataset_X$duration)] <- quantile(numerical_diagnosisDataset_X$duration, na.rm = T)[3]
words_diagnosisDataset_X$duration[is.na(words_diagnosisDataset_X$duration)] <- quantile(words_diagnosisDataset_X$duration, na.rm = T)[3]

# export age and gender information
write.table(numerical_diagnosisDataset_X, file = "./numericalDrug_ageSex.csv", sep = ",", row.names = FALSE, col.names = TRUE)
write.table(words_diagnosisDataset_X, file = "./drugWords_ageSex.csv", sep = ",", row.names = FALSE, col.names = TRUE)

# n = 145
# x = c(1:(n-1))
# plot(x, hba1cComplete[n, 2:n], ylim = c(20, 150))
# points(x, sbpComplete[n, 2:n], col = "red")
# points(x, dbpComplete[n, 2:n], col = "pink")
# points(x, bmiComplete[n, 2:n], col = "blue")

## extract central section for analysis
## strip ID from data
paddingBins = 48 # number of bins to remove pre / post section

truncateFunction <- function(inputFrame, numberOfBins) {
  inputFrame$LinkId <- NULL
  outputFrame <- inputFrame[, (numberOfBins + 1):(ncol(inputFrame) - numberOfBins)]
  return(outputFrame)
}

# export numerical drug data files
write.table(truncateFunction(hba1cComplete_numericalDrugData, paddingBins), file = "./numericalDrug_hba1c.csv", sep = ",", row.names = FALSE, col.names = TRUE)
write.table(truncateFunction(sbpComplete_numericalDrugData, paddingBins), file = "./numericalDrug_sbp.csv", sep = ",", row.names = FALSE, col.names = TRUE)
write.table(truncateFunction(dbpComplete_drugWordData, paddingBins), file = "./numericalDrug_sbp.csv", sep = ",", row.names = FALSE, col.names = TRUE)
write.table(truncateFunction(bmiComplete_numericalDrugData, paddingBins), file = "./numericalDrug_bmi.csv", sep = ",", row.names = FALSE, col.names = TRUE)
write.table(truncateFunction(drugs_numericalDrugData, paddingBins), file = "./numericalDrug_drugs.csv", sep = ",", row.names = FALSE, col.names = TRUE)

# export word drug data files
write.table(truncateFunction(hba1cComplete_drugWordData, paddingBins), file = "./drugwords_hba1c.csv", sep = ",", row.names = FALSE, col.names = TRUE)
write.table(truncateFunction(sbpComplete_drugWordData, paddingBins), file = "./drugWords_sbp.csv", sep = ",", row.names = FALSE, col.names = TRUE)
write.table(truncateFunction(dbpComplete_drugWordData, paddingBins), file = "./drugWords_dbp.csv", sep = ",", row.names = FALSE, col.names = TRUE)
write.table(truncateFunction(bmiComplete_drugWordData, paddingBins), file = "./drugWords_bmi.csv", sep = ",", row.names = FALSE, col.names = TRUE)
write.table(truncateFunction(drugs_drugWordData, paddingBins), file = "./drugWords_drugs.csv", sep = ",", row.names = FALSE, col.names = TRUE)
