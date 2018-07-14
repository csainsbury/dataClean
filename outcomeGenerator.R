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

## find deltas
# numerical drug data
numerical_hba1c_yDelta <- numericalDelta(numerical_hba1c, finalStepBins)
numerical_sbp_yDelta <- numericalDelta(numerical_sbp, finalStepBins)
numerical_bmi_yDelta <- numericalDelta(numerical_bmi, finalStepBins)

# word drug data
words_hba1c_yDelta <- numericalDelta(words_hba1c, finalStepBins)
words_sbp_yDelta <- numericalDelta(words_sbp, finalStepBins)
words_bmi_yDelta <- numericalDelta(words_bmi, finalStepBins)

