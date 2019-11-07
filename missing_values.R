#libraries used
library(dplyr)
library(FactoMineR)
library(outliers)
library(ggplot2)

# ------------- Helpter Functions ----------------
basic_categorical_replacer <- function(x) {
  for (i in 1:length(x)) {
    if(is.na(x[i])) {
      x[i] = x[i-1]
    }
  }
  x
}

median_replacer <- function(x) {
  med <- median(x, na.rm = TRUE)
  x[is.na(x)] <- med
  x
}

mode_replacer <- function(x) {
  mde <- getmode(x)
  x[is.na(x)] <- mde
  x
}

exp_fit_replacer <- function(v) {
  x <- 1:length(v)
  fit <- lm(log(v) ~ x)
  for(i in 1:length(v)) {
    if(is.na(v[i]))
      v[i] <- exp(predict(fit, data.frame(x = c(i))))
  }
  v
}

#Get the mode for a numeric vector
getmode <- function(v) {
  un <- unique(v)
  un[which.max(table(match(v, un)))]
}

#for testing only
stats <- function(x) {
  plot(x)
  print(mean(x, na.rm = TRUE))
  print(median(x, na.rm = TRUE))
  print(sd(x, na.rm = TRUE))
  print(max(x, na.rm = TRUE))
  print(min(x, na.rm = TRUE))
}

# ----------- End helper Functitons ----------

#load dateset
dataset <- read.table("5402_dataset.csv", sep=",", header = TRUE)

#Remove inconsistant values for is_attack
dataset[, 2][dataset[, 2] == 'N'] <- 0
dataset[, 2][dataset[, 2] == 'Y'] <- 1

#Attempt to fill NA values for data sett

#------------ Categorical Data ------------

#The Pumps
for(i in 3:21) {
  dataset[, i] <- basic_categorical_replacer(dataset[, i])
}

#MV Group 1 -- replace NA with most frequent value
for(i in 22:24) {
  dataset[, i][is.na(dataset[, i])] <- names(which.max(table(dataset[, i])))
  #print(sum(is.na(dataset[, i])))
}

#UV Group
dataset[, 25][is.na(dataset[, 25])] <- names(which.max(table(dataset[, 25])))

#MV Group 2
for(i in 26:28) {
  dataset[, i][is.na(dataset[, i])] <- names(which.max(table(dataset[, i])))
}
#------------ Done with Categorical Data ------------

#------------ Fill in NA  values for Numerical Data ------------

#FIT - Replace with the mode, which will be 0, since there are for more 0's than any other element
dataset[, 29][is.na(dataset[, 29])] <- getmode(dataset[, 29])

#Columns 30 through 36 all have high standard devation, replace NA values with medians
for(i in 30:36) {
  dataset[, i] <- median_replacer(dataset[, i])
}

#Excluding the NA values, the only value of this column is zero. Replace NA with the mode (0)
dataset[, 37] <- mode_replacer(dataset[, 37])

#This vector stronglt resembles exponential decay, replace NA values by fitting an exponential decay function.
dataset[, 38] = exp_fit_replacer(dataset[, 38])

#Columns 39 through 41 should replace missing values with the median
for(i in 39:41) {
  dataset[, i] <- median_replacer(dataset[, i])
}

#this vector also has some strong resemelance to exponental decay, fit this one as well
dataset[, 42] = exp_fit_replacer(dataset[, 42])

#Replace witth median since the standard devation is high
dataset[, 43] <- median_replacer(dataset[, 43])

#This vector looks like exponential decay, fit a curve
dataset[, 44] = exp_fit_replacer(dataset[, 44])

#Replace with median
for(i in 45:53) {
  dataset[, i] <- median_replacer(dataset[, i])
}

# --- Finialize and write ---

write.csv(dataset, file="Processed_data.csv", row.names = FALSE)

