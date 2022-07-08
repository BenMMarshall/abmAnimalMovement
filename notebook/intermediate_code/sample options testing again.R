
ndraws <- 100000
W <- c(
  1.11487,
  1e-05,
  -3.43936,
  1.45606,
  -3.3031,
  1.14072,
  1.23658,
  -3.10818,
  -2.24707,
  -1.9031,
  1.75243,
  1.93727,
  -1.86032)

# W <- c(1,0)
# W <- W + abs(min(W))
testSampleFunc <- Rcpp::cppFunction(
  'int testSampleFunc(NumericVector W, int SEED) {
  int out;
  out = Rcpp::sample(W,
       1,
       FALSE,
       W)[0];
  return out-1;
}'
)

set.seed(2022)
testSampleFunc(W, SEED = 1)

sample(x = 1:length(W),
       size = 1,
       replace = FALSE,
       prob = W + abs(min(W)))


# W <- W + abs(min(W))

sampleOut <- NULL
for(i in 1:ndraws){
  # sampleOut[i] <- sample_options(c(2, -0.2, 5, 0.5, 0.05, -99.9), get_seed())
  sampleOut[i] <- sample_options(W,
                                 2020)
}
hist(sampleOut)

sampleOut_R <- NULL
for(i in 1:ndraws){
  # sampleOut[i] <- sample_options(c(2, -0.2, 5, 0.5, 0.05, -99.9), get_seed())
  sampleOut_R[i] <- sample(length(W),
                           1,
                           FALSE,
                           ((W - min(W)) /
                              (max(W) - min(W))))-1
}
hist(sampleOut_R)

table(sampleOut)


data.frame(
  "index" = as.character(1:length(W)-1),
  "weight" = W) %>%
  left_join(as.data.frame(table(sampleOut)) %>%
              rename("index" = sampleOut) %>%
              mutate(index = as.character(index))) %>%
  left_join(as.data.frame(table(sampleOut_R)) %>%
              rename("index" = sampleOut_R, "Freq_R" = Freq) %>%
              mutate(index = as.character(index))) %>%
  mutate(Freq = ifelse(is.na(Freq), 0, Freq)) %>%
  ggplot() +
  geom_col(aes(x = index, y = Freq), alpha = 0.5,
           position = position_nudge(x = 0.25),
           width = 0.25) +
  geom_col(aes(x = index, y = Freq_R), alpha = 0.5,
           position = position_nudge(x = -0.25),
           width = 0.25, fill = "red") +
  geom_text(aes(x = index, y = Freq, label = W))


W2 <- rep(1,12)

sampleOut <- NULL
for(i in 1:ndraws){
  # sampleOut[i] <- sample_options(c(2, -0.2, 5, 0.5, 0.05, -99.9), get_seed())
  sampleOut[i] <- sample_options(W2,
                                 2020)
}
hist(sampleOut)
