ProbabilityMarginK <- function(kMargin, model.classification, test.set){
  # Calculates the percenteage of data that will be classified, and the accuracy if we only
  # classify the data with probability higher than 0.5 + kMargin or lower than
  # 0.5 - kMargin.
  # Args:
  #  kMargin: The space from probability 0.5 that conditions wether a value is
  #           or not classified.
  #  model.classification: The regression model used.
  #  test.set: The set provided to check the errors. Ideally this set is not used
  #            to build model.classification.
  prob.test <- predict(model.classification,
                       test.set,
                       type  = "response")
  is.classifiable <- (prob.test < 0.5 - kMargin)|(prob.test > 0.5 + kMargin)
  classifiable.test.set <- test.set[is.classifiable,]
  classifiable.prob.test <- prob.test[is.classifiable]
  to.return <- c(mean((classifiable.prob.test > 0.5) == (classifiable.test.set$Direction == "Up")),
                 mean(is.classifiable))
  names(to.return) <- c("Test Accuracy","Classified values")
  return(to.return)
}