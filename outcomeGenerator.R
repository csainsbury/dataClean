finalStepBins <- 24 # number of bins to (i) calculate outcome across and (ii) zero out for calc

numericalDelta <- function(inputFrame, finalStepBins) {
  finalCol = ncol(inputFrame)
  deltaVector <- inputFrame[, finalCol] - inputFrame[, (finalCol - finalStepBins)]
  return(deltaVector)
}

## read in data files
# numerical drug data
numerical_hba1c = read.csv("~/R/_workingDirectory/dataClean/high_f_dataFiles/numericalDrug_hba1c.csv", header = T)
numerical_sbp = read.csv("~/R/_workingDirectory/dataClean/high_f_dataFiles/numericalDrug_sbp.csv", header = T)
numerical_bmi = read.csv("~/R/_workingDirectory/dataClean/high_f_dataFiles/numericalDrug_bmi.csv", header = T)

# word drug data
words_hba1c = read.csv("~/R/_workingDirectory/dataClean/high_f_dataFiles/drugWords_hba1c.csv", header = T)
words_sbp = read.csv("~/R/_workingDirectory/dataClean/high_f_dataFiles/drugWords_sbp.csv", header = T)
words_bmi = read.csv("~/R/_workingDirectory/dataClean/high_f_dataFiles/drugWords_bmi.csv", header = T)

## find deltas and save out
# numerical drug data
write.table(numericalDelta(numerical_hba1c, finalStepBins), file = "~/R/_workingDirectory/dataClean/high_f_dataFiles/numerical_hba1c_yDelta.csv", sep = ",", row.names = FALSE, col.names = TRUE)
write.table(numericalDelta(numerical_sbp, finalStepBins), file = "~/R/_workingDirectory/dataClean/high_f_dataFiles/numerical_sbp_yDelta.csv", sep = ",", row.names = FALSE, col.names = TRUE)
write.table(numericalDelta(numerical_bmi, finalStepBins), file = "~/R/_workingDirectory/dataClean/high_f_dataFiles/numerical_bmi_yDelta.csv", sep = ",", row.names = FALSE, col.names = TRUE)

# word drug data
write.table(numericalDelta(words_hba1c, finalStepBins), file = "~/R/_workingDirectory/dataClean/high_f_dataFiles/words_hba1c_yDelta.csv", sep = ",", row.names = FALSE, col.names = TRUE)
write.table(numericalDelta(words_sbp, finalStepBins), file = "~/R/_workingDirectory/dataClean/high_f_dataFiles/words_sbp_yDelta.csv", sep = ",", row.names = FALSE, col.names = TRUE)
write.table(numericalDelta(words_bmi, finalStepBins), file = "~/R/_workingDirectory/dataClean/high_f_dataFiles/words_bmi_yDelta.csv", sep = ",", row.names = FALSE, col.names = TRUE)

## zero out final section of datafiles for input into RNN
finalZero <- function(inputFrame, finalStepBins) {
  finalCol = ncol(inputFrame)
  inputFrame[, ((finalCol - finalStepBins) + 1) : finalCol] <- 0
  return(inputFrame)
}

## pad with zeros and save out
# numerical drug data
write.table(finalZero(numerical_hba1c, finalStepBins), file = "~/R/_workingDirectory/dataClean/high_f_dataFiles/numerical_hba1c_X.csv", sep = ",", row.names = FALSE, col.names = TRUE)
write.table(finalZero(numerical_sbp, finalStepBins), file = "~/R/_workingDirectory/dataClean/high_f_dataFiles/numerical_sbp_X.csv", sep = ",", row.names = FALSE, col.names = TRUE)
write.table(finalZero(numerical_bmi, finalStepBins), file = "~/R/_workingDirectory/dataClean/high_f_dataFiles/numerical_bmi_X.csv", sep = ",", row.names = FALSE, col.names = TRUE)

# word drug data
write.table(finalZero(words_hba1c, finalStepBins), file = "~/R/_workingDirectory/dataClean/high_f_dataFiles/words_hba1c_X.csv", sep = ",", row.names = FALSE, col.names = TRUE)
write.table(finalZero(words_sbp, finalStepBins), file = "~/R/_workingDirectory/dataClean/high_f_dataFiles/words_sbp_X.csv", sep = ",", row.names = FALSE, col.names = TRUE)
write.table(finalZero(words_bmi, finalStepBins), file = "~/R/_workingDirectory/dataClean/high_f_dataFiles/words_bmi_X.csv", sep = ",", row.names = FALSE, col.names = TRUE)

## save out age, sex and duration information in expanded form for RNN import
numerical_ageSex = read.csv("~/R/_workingDirectory/dataClean/high_f_dataFiles/numericalDrug_ageSex.csv", header = T)
words_ageSex = read.csv("~/R/_workingDirectory/dataClean/high_f_dataFiles/drugWords_ageSex.csv", header = T)

# need to write a function to expand the age and sex parameters to 96 identical cols


