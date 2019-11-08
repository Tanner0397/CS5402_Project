#Libs used 
library(caret)
library(corrplot)

zeroVar <- function(x) {
  suppressWarnings(as.numeric(which(apply(x, 2, var) == 0)))
}

singleUnique <- function(x) {
  as.numeric(which(apply(x, 2, function(x) {length(unique(x))}) == 1))
}

df <- read.table("data_missing_filled.csv", sep=',', header = TRUE)

#Remove attributes with zero variance
df.non_zero <- df[, -zeroVar(df)]

#Remove attribues that only ahve a sigle unique value
df.non_zero <- df.non_zero[, -singleUnique(df.non_zero)]

#Create a copy with only the numeric attributes to run a corration test on
df.numeric <- df.non_zero[,sapply(df.non_zero, is.numeric)]


#Determine corrations between numeric attributes of the data
cor.mat.spearman <- cor(df.numeric, method = "spearman")
corrplot(cor.mat.spearman)

#Remove highly correlated values
cor.mat.spearman[upper.tri(cor.mat.spearman)] <- 0
diag(cor.mat.spearman) <- 0

df.non_zero <- df.non_zero[, !apply(cor.mat.spearman, 2, function(x) any(abs(x) > 0.90))]
 
#Remove timestamps
df.non_zero[, 1] <- NULL

#Write data
write.csv(df.non_zero, file="processed.csv", row.names = FALSE)

#There is no strong correlation between any of the attributes. Linear regression between attributes to determine
#what instanced are noise won't be very useful

