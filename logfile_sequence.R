1.
splineInterpolate.R
this takes the clean HbA1c/BP etc files and bins/interpolates the data for later use

2.
highFrequencyDrugData_for_RNN.R
generates the matching drug data for input to RNN
currently outputs both numerical (tokenised) and word outputs
need to set nBins in this file to match values in splineInterpolate.R

3.
combineAndTruncate.R
generate a set with data for all parameters.

4.
outcomeGenerator.R
generate the outcome (hba1c/sbp diff files etc)
extract the central data (4y from a 6y set etc) to minimise artefact from interpolation