## 
# generate equivalent sets of each with IDs in order
##

# number of follow up years
n_years = 6

# load data back in
hba1c_spline <- read.csv(paste('~/R/_workingDirectory/dataClean/high_f_dataFiles/hba1c_', n_years, 'y_limitedValues.csv', sep = ""), header = T)
sbp_spline <- read.csv(paste('~/R/_workingDirectory/dataClean/high_f_dataFiles/sbp_', n_years, 'y_limitedValues.csv', sep = ""), header = T)
dbp_spline <- read.csv(paste('~/R/_workingDirectory/dataClean/high_f_dataFiles/dbp_', n_years, 'y_limitedValues.csv', sep = ""), header = T)
bmi_spline <- read.csv(paste('~/R/_workingDirectory/dataClean/high_f_dataFiles/bmi_', n_years, 'y_limitedValues.csv', sep = ""), header = T)

numericalDrugs <- read.csv(paste('~/R/_workingDirectory/dataClean/high_f_dataFiles/numericalDrugsFrame_withID_', n_years, 'y.csv', sep = ""), header = T)
drugWords <- read.csv(paste('~/R/_workingDirectory/dataClean/high_f_dataFiles/drugWordFrame_withID_', n_years, 'y.csv', sep = ""), header = T)


# generate LinkId single column frames
hba1cID <- as.data.frame(hba1c_spline$LinkId); colnames(hba1cID) <- c("LinkId")
sbpID <- as.data.frame(sbp_spline$LinkId); colnames(sbpID) <- c("LinkId")
dbpID <- as.data.frame(dbp_spline$LinkId); colnames(dbpID) <- c("LinkId")
bmiID <- as.data.frame(bmi_spline$LinkId); colnames(bmiID) <- c("LinkId")

numericalDrugsID <- as.data.frame(numericalDrugs$drugWordFrame_forAnalysis.LinkId); colnames(numericalDrugsID) <- c("LinkId")
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

# n = 145
# x = c(1:(n-1))
# plot(x, hba1cComplete[n, 2:n], ylim = c(20, 150))
# points(x, sbpComplete[n, 2:n], col = "red")
# points(x, dbpComplete[n, 2:n], col = "pink")
# points(x, bmiComplete[n, 2:n], col = "blue")

## extract central section for analysis
## strip ID from data
paddingBins = 24 # number of bins to remove pre / post section

truncateFunction <- function(inputFrame, numberOfBins) {
  inputFrame$LinkId <- NULL
  outputFrame <- inputFrame[, (numberOfBins + 1):(ncol(inputFrame) - numberOfBins)]
  return(outputFrame)
}

# export numerical drug data files
write.table(truncateFunction(hba1cComplete_numericalDrugData, paddingBins), file = "~/R/_workingDirectory/dataClean/high_f_dataFiles/numericalDrug_hba1c.csv", sep = ",", row.names = FALSE, col.names = TRUE)
write.table(truncateFunction(sbpComplete_numericalDrugData, paddingBins), file = "~/R/_workingDirectory/dataClean/high_f_dataFiles/numericalDrug_sbp.csv", sep = ",", row.names = FALSE, col.names = TRUE)
write.table(truncateFunction(dbpComplete_drugWordData, paddingBins), file = "~/R/_workingDirectory/dataClean/high_f_dataFiles/numericalDrug_sbp.csv", sep = ",", row.names = FALSE, col.names = TRUE)
write.table(truncateFunction(bmiComplete_numericalDrugData, paddingBins), file = "~/R/_workingDirectory/dataClean/high_f_dataFiles/numericalDrug_bmi.csv", sep = ",", row.names = FALSE, col.names = TRUE)
write.table(truncateFunction(drugs_numericalDrugData, paddingBins), file = "~/R/_workingDirectory/dataClean/high_f_dataFiles/numericalDrug_drugs.csv", sep = ",", row.names = FALSE, col.names = TRUE)

# export word drug data files
write.table(truncateFunction(hba1cComplete_drugWordData, paddingBins), file = "~/R/_workingDirectory/dataClean/high_f_dataFiles/drugwords_hba1c.csv", sep = ",", row.names = FALSE, col.names = TRUE)
write.table(truncateFunction(sbpComplete_drugWordData, paddingBins), file = "~/R/_workingDirectory/dataClean/high_f_dataFiles/drugWords_sbp.csv", sep = ",", row.names = FALSE, col.names = TRUE)
write.table(truncateFunction(dbpComplete_drugWordData, paddingBins), file = "~/R/_workingDirectory/dataClean/high_f_dataFiles/drugWords_dbp.csv", sep = ",", row.names = FALSE, col.names = TRUE)
write.table(truncateFunction(bmiComplete_drugWordData, paddingBins), file = "~/R/_workingDirectory/dataClean/high_f_dataFiles/drugWords_bmi.csv", sep = ",", row.names = FALSE, col.names = TRUE)
write.table(truncateFunction(drugs_drugWordData, paddingBins), file = "~/R/_workingDirectory/dataClean/high_f_dataFiles/drugWords_drugs.csv", sep = ",", row.names = FALSE, col.names = TRUE)
