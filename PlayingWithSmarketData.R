index <- seq(from = 0, to = 1,by = 0.0005)
values <- sapply(index, 
                 ProbabilityMarginK,
                 model.classification = log.reg2,
                 test.set = test.set)

valuest <- t(values)
head(valuest)
valuesEnd <- data.frame(valuest, row.names = index)
myData <- valuesEnd[(valuesEnd$Classified.values != 0),]
