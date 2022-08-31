# Functions

residTest  <- function(modelName, dataset) {
  attach(dataset)
  #Testing normal distribution and independence assumptions
  test1 <- jarqueberaTest(modelName$resid) #Test residuals for normality
  #Null Hypothesis: Skewness and Kurtosis are equal to zero
  test2 <- dwtest(modelName) #Test for independence of residuals
  #Null Hypothesis: Errors are serially Uncorrelated
  #Simple Regression Residual Plots
  cat("Jarque - Bera Normality Test to check if residuals are normally distributed\nWe want a non-significant result so as not to reject the null hypothesis that skewness and kurtosis are equal to zero\n")
  print(test1)
  if (test1@test[["p.value"]] < 0.05) { testN1 <- "The Jarque - Bera Normality Test is significant.\nThis suggests the data is NOT normally distributed, and violates\nthe assumptions of the regression\n\n"}
  else {testN1 <- "The Jarque - Bera Normality Test is non-significant.\nThis suggests the data is normally distributed\n\n" }
  if (test2[["p.value"]] < 0.05) {testN2 <- "The Durbin-Watson Test is significant.\nThis suggests the residuals are NOT independent.\nIt is likely your data violates the assumptions of the regression test\n\n"}
  else {testN2 <- "The Durbin-Watson Test is non-significant.\nThis suggests the independence of residuals\n\n"}
  cat(testN1)
  cat("Durbin-Watson Test to check for independence of residuals:\nWe want a non-significant result so as not to reject the null hypothesis:\nThe errors are serially uncorrelated\n")
  print(test2)
  cat(testN2)
}

# End Functions
