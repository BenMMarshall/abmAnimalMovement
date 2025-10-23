corrFactor <- 0.9
shape <- 10
rate <- 0.5


testDist <- rgamma(100, shape = shape, rate = rate)
hist(testDist)

lastValue <- 0

allValues <- vector()
for(i in 1:1000) {
  lastValue <- corrFactor * lastValue + (1 - corrFactor) * rgamma(1, shape = shape, rate = rate)
  allValues[i] <- lastValue
}
hist(allValues)
plot(allValues, type = "l")

acf(allValues)
