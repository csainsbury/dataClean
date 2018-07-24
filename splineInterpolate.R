library(data.table)
returnUnixDateTime<-function(date) {
  returnVal<-as.numeric(as.POSIXct(date, format="%Y-%m-%d", tz="GMT"))
  return(returnVal)
}

splineInterpol <- function(inputFrame, startDate, endDate, nBins, minTestsN, minimumAcceptableValue, maximumAcceptableValue, filename) {
  # inputFrame = hba1cLimited; startDate = "2013-01-01"; endDate = "2016-01-01";durationMonths = 30; nBins = 30; filename = "hba1cSpline"; minTestsN = 3; minimumAcceptableValue = 20; maximumAcceptableValue = 200
  # inputFrame = inputFrame[1:1000, ]
  
  nBins = (nBins - 1)
  
  startDate_unix <- returnUnixDateTime(startDate)
  endDate_unix <- returnUnixDateTime(endDate)
  
  # drop all measures outwith window
  inputFrame <- inputFrame[dateplustime1 >= startDate_unix & dateplustime1 <= endDate_unix]
  
  # count individual measures
  inputFrame[, c("testN") := seq(1, .N, 1) , by=.(LinkId)]
  # ensure that more than n measures per ID
  inputFrame[, c("moreThanN_flag") := ifelse(max(testN) >= minTestsN, 1, 0) , by=.(LinkId)]
  inputFrame <- inputFrame[moreThanN_flag == 1]
  
  
  #setup the outputframe
  IDvec <- unique(inputFrame$LinkId)
  binLengthSeconds = (endDate_unix - startDate_unix) / nBins
  
  generateOutputFrame <- as.data.frame(matrix(0, nrow = length(IDvec), ncol = length(seq(startDate_unix, endDate_unix, binLengthSeconds)) + 1))
  
  # spline function
  splineOutput <- function(x, y, LinkId) {
    
    # testID = 2147483780; x = inputFrame[LinkId == testID]$dateplustime1; y = inputFrame[LinkId == testID]$numericValue; IDsequenceN = inputFrame[LinkId == testID]$IDsequenceN[1]; LinkId = inputFrame[LinkId == testID]$LinkId[1]
    
  func = splinefun(x=x, y=y, method="fmm",  ties = mean)
  output = func(seq(startDate_unix, endDate_unix, binLengthSeconds))
  return(output)
  
  }
  
  print(length(IDvec))
  for (q in seq(1, length(IDvec), 1)) {
    
    if (q%%1000 == 0) {print(q)}
    
    interestSub <- inputFrame[LinkId == IDvec[q]]
    
    output <- splineOutput(interestSub$dateplustime1, interestSub$numericValue, interestSub$LinkId)
    
    generateOutputFrame[q, 2:ncol(generateOutputFrame)] <- output
    generateOutputFrame[q, 1] = interestSub$LinkId[1]
    
  }
  
  # write out whole output frame
  write.table(generateOutputFrame, file = paste("~/R/_workingDirectory/dataClean/", filename, ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = TRUE)
  
  # generate a tuncated output that spans the start time to end of period of interest (minus the LinkId)
  truncatedOutput = generateOutputFrame[2: ncol(generateOutputFrame)]
  
  # apply limits to values
  truncatedOutput[truncatedOutput < minimumAcceptableValue] <- minimumAcceptableValue
  truncatedOutput[truncatedOutput > maximumAcceptableValue] <- maximumAcceptableValue
  
  # truncated output with LinkId
  truncOP_withID <- cbind(generateOutputFrame[, 1], truncatedOutput)
  colnames(truncOP_withID)[1] <- c("LinkId")
  
  write.table(truncOP_withID, file = paste("~/R/_workingDirectory/dataClean/", filename, "_limitedValues.csv", sep = ""), sep = ",", row.names = FALSE, col.names = TRUE)
  
  
}

cleanHbA1cData <- read.csv("~/R/GlCoSy/SD_workingSource/hba1cDTclean.csv", sep=",", header = TRUE, row.names = NULL)
  hba1cLimited <- data.frame(cleanHbA1cData$LinkId, cleanHbA1cData$hba1cNumeric, cleanHbA1cData$dateplustime1); colnames(hba1cLimited) <- c("LinkId", "numericValue", "dateplustime1")
  hba1cLimited <- data.table(hba1cLimited)
  
cleanSBPData <- read.csv("~/R/GlCoSy/SD_workingSource/SBPsetDTclean.csv", sep=",", header = TRUE, row.names = NULL)
  sbpLimited <- data.frame(cleanSBPData$LinkId, cleanSBPData$sbpNumeric, cleanSBPData$dateplustime1); colnames(sbpLimited) <- c("LinkId", "numericValue", "dateplustime1")
  sbpLimited <- data.table(sbpLimited)

cleanDBPData <- read.csv("~/R/GlCoSy/SD_workingSource/DBPsetDTclean.csv", sep=",", header = TRUE, row.names = NULL)
  dbpLimited <- data.frame(cleanDBPData$LinkId, cleanDBPData$dbpNumeric, cleanDBPData$dateplustime1); colnames(dbpLimited) <- c("LinkId", "numericValue", "dateplustime1")
  dbpLimited <- data.table(dbpLimited)

cleanBMIData <- read.csv("~/R/GlCoSy/SD_workingSource/BMIsetDTclean.csv", sep=",", header = TRUE, row.names = NULL)
  bmiLimited <- data.frame(cleanBMIData$LinkId, cleanBMIData$bmiNumeric, cleanBMIData$dateplustime1); colnames(bmiLimited) <- c("LinkId", "numericValue", "dateplustime1")
  bmiLimited <- data.table(bmiLimited)

splineInterpol(hba1cLimited, "2011-01-01", "2017-01-01", n_bins, 8, 20, 200, "hba1c_6_144")
splineInterpol(sbpLimited, "2012-01-01", "2017-01-01", n_bins, 8, 80, 240, "sbp_6y_144")
splineInterpol(dbpLimited, "2012-01-01", "2017-01-01", n_bins, 8, 40, 160, "dbp_6y_144")
splineInterpol(bmiLimited, "2012-01-01", "2017-01-01", n_bins, 6, 16, 100, "bmi_6y_144")

