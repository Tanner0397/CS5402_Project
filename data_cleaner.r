#libraries used
library(dplyr)
library(FactoMineR)
library(outliers)
library(ggplot2)

#Functions used while preprocessing

#Since many of the missing NA values for the pumps are in "blocks" since the pumps are not alternationg on and off very frequently
#This is used to put NA values to the value above them
basic_categorical_replacer <- function(x) {
  for (i in 1:length(x)) {
    if(is.na(x[i])) {
      x[i] = x[i-1]
    }
  }
  x
}

#Replaces NA values of a vector based on the median
median_replacer <- function(x) {
  med <- median(x, na.rm = TRUE)
  x[is.na(x)] <- med
  x
}

#Replaced NA values of a vector based on the mode
mode_replacer <- function(x) {
  mde <- getmode(x)
  x[is.na(x)] <- mde
  x
}

#used linearization to fit a curve to fill in  NA values
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

#returns a list of elements of indicies that have a zero variance
zeroVar <- function(x) {
  suppressWarnings(as.numeric(which(apply(x, 2, var) == 0)))
}

#returns a list of element indicies that only have one unique value, essentially having a variance of 0
singleUnique <- function(x) {
  as.numeric(which(apply(x, 2, function(x) {length(unique(x))}) == 1))
}

#A pairwise chi sqquared test, shows  the p.vvalue calculated
pairwise.chi.square.test <- function(x) {
  names = colnames(x)
  n = length(names)
  m = matrix(nrow=n,ncol=n,dimnames=list(names, names))
  for(i in 1:(n-1)) {
    for(j in (i+1):n) {
      m[i, j] <- chisq.test(x[, i], x[, j], correct = FALSE)$p.value
    }
  }
  m
}

#A pairwise Chi squared test, shows the X^2 value
pairwise.chi.square.test.stat <- function(x) {
  names = colnames(x)
  n = length(names)
  m = matrix(nrow=n,ncol=n,dimnames=list(names, names))
  for(i in 1:(n-1)) {
    for(j in (i+1):n) {
      m[i, j] <- chisq.test(x[, i], x[, j], correct = FALSE)$statistic
    }
  }
  m
}

#Calculate degrees of freedom
degree_freedom_calc <- function(x, y) {
  (length(unique(x))-1)*(length(unique(y))-1)
}

#Replaced values based on 33% quantiles
quantile_three_replacer <- function(x) {
  #Remove outliers
  v = x[!x %in% boxplot.stats(x)$out]
  quan = as.numeric(quantile(v, seq(0, 1, 0.33)))
  n = length(x)
  for(i in 1:n) {
    value = as.numeric(x[i])
    if (value<= quan[2]) {
      x[i] <- "low"
    } else if (value <= quan[3]) {
      x[i] <- "med"
    }
    else
      x[i] <- "high"
  }
  x
}

#Replaces values based on 50% quantiles
quantile_two_replacer <- function(x) {
  #Remove outliers
  v = x[!x %in% boxplot.stats(x)$out]
  quan <- quantile(v, seq(0, 1, 0.25))
  n = length(x)
  for(i in 1:n) {
    value = as.numeric(x[i])
    if(value > quan[2])
      x[i] <- "high"
    else
      x[i] <- "low"
  }
  x
}

#Function to gives tha variance after using a boxplot to remove outliers
var.outliers.rm <- function(x) {
  v = x[!x %in% boxplot.stats(x)$out]
  var(v)
}

nearZeroVar <- function(x) {
  suppressWarnings(as.numeric(which(apply(x, 2, var.outliers.rm) < 0.0001)))
}

#First we will attempt to fill in all the missing values in the entire df

#load df
df <- read.table("5402_dataset.csv", sep=",", header = TRUE)

#Remove inconsistant values
df[, 2][df[, 2] == 0] <- "N"
df[, 2][df[, 2] == 1] <- "Y"


#Attempt to fill NA values for data set
#------------ Categorical Data ------------

#The Pumps
for(i in 3:21) {
  #Make data nominal and not numeric
  df[, i][df[, i] == 2] <- "On"
  df[, i][df[, i] == 1] <- "Off"
  df[, i] <- basic_categorical_replacer(df[, i])
}

#MV Group 1 -- replace NA with most frequent value
for(i in 22:24) {
  df[, i][is.na(df[, i])] <- names(which.max(table(df[, i])))
  #print(sum(is.na(df[, i])))
}

#UV Group, make data nominal
df[, 25][df[, 25] == 2] <- "On"
df[, 25][df[, 25] == 1] <- "Off"
df[, 25][is.na(df[, 25])] <- names(which.max(table(df[, 25])))

#MV Group 2
for(i in 26:28) {
  df[, i][is.na(df[, i])] <- names(which.max(table(df[, i])))
}
#------------ Done with Categorical Data ------------

#------------ Fill in NA  values for Numerical Data ------------

#FIT - Replace with the mode, which will be 0, since there are for more 0's than any other element
df[, 29][is.na(df[, 29])] <- getmode(df[, 29])

#Columns 30 through 36 all have high standard devation, replace NA values with medians
for(i in 30:36) {
  df[, i] <- median_replacer(df[, i])
}

#Excluding the NA values, the only value of this column is zero. Replace NA with the mode (0)
df[, 37] <- mode_replacer(df[, 37])

#This vector stronglt resembles exponential decay, replace NA values by fitting an exponential decay function.
df[, 38] = exp_fit_replacer(df[, 38])

#Columns 39 through 41 should replace missing values with the median
for(i in 39:41) {
  df[, i] <- median_replacer(df[, i])
}

#this vector also has some strong resemelance to exponental decay, fit this one as well
df[, 42] = exp_fit_replacer(df[, 42])

#Replace witth median since the standard devation is high
df[, 43] <- median_replacer(df[, 43])

#This vector looks like exponential decay, fit a curve
df[, 44] = exp_fit_replacer(df[, 44])

#Replace with median
for(i in 45:53) {
  df[, i] <- median_replacer(df[, i])
}

#-------------------------------------------------- Done filling --------------------------------------------------
df.filled <- df.filled
#-------------------------------------------------- Eliminate Attributes ------------------------------------------

#Remove attributes with zero variance
df <- df[, -zeroVar(df)]

#Remove attribues that only ahve a sigle unique value
df <- df[, -singleUnique(df)]

#Create a copy with only the numeric attributes to run a corration test on
df.numeric <- df[,sapply(df, is.numeric)]

#Determine corrations between numeric attributes of the data
cor.mat.spearman <- cor(df.numeric, method = "spearman")

#Remove highly correlated values
cor.mat.spearman[upper.tri(cor.mat.spearman)] <- 0
diag(cor.mat.spearman) <- 0
df <- df[, !apply(cor.mat.spearman, 2, function(x) any(abs(x) > 0.90))]

#--------------- Chi Squared Test ------------------

#Create a subset of the nominal attributes
df.nominal <- df[, !sapply(df, is.numeric)]

#Create chi squared test pairwise matricies to see what attributes are related
chi.matrix <- pairwise.chi.square.test(df.nominal)
chi.stat.matrix <- pairwise.chi.square.test.stat(df.nominal)

#Upon looking at the following matricies for the chi squared test, we have determined that the following attribues can be dropped
#reasons will be commented

#P101, P103, and P205 all have a strong evidence of depenedence. Drop P103 and P205
df$P203 <- NULL
df$P205 <- NULL

#P301, MV304, has strong evidence to show that of dependence, MV304,
df$MV304 <- NULL
df$MV201 <- NULL

#MV301 strong correlated with the following
df$MV303 <- NULL

#MV302 strong correlated with MV101
df$MV101 <- NULL

#---------- End Chi Squared Test ----------

#Using Boxplots of attributes to determine the variance of attributes without outliers present

#AIT503 has a very low variance, can probably be removed
df$AIT503 <- NULL

#Near zero variance after removing outliers from the attributtes, using the var.outlier.rm function
df$FIT501 <- NULL
df$FIT503 <- NULL
df$FIT504 <- NULL
df$FIT401 <- NULL

# --------- Outlier Analysis ----------
#Use Kmeans clusterding with 3 clusters
kmeans.result <- kmeans(df.numeric, centers = 3)
centers <- kmeans.result$centers[kmeans.result$cluster, ]
distances <- sqrt(rowSums((df.numeric - centers)^2))
outliers <- order(distances, decreasing = TRUE)
#plot(outliers)

#Remove outliers, from the result we say roughly 5000 rows can be removed
df <- df[-outliers[1:5000], ]
df.numeric <- df.numeric[-outliers[1:5000], ]


# ------------------------------------------ End Eliminated Attributes ------------------------------------
# ------------------------------------------ Begin Discreation -------------------------------------------

#Replace the values in LIT with low med and high classifers
df$LIT101 <- quantile_three_replacer(df$LIT101)
df$LIT301 <- quantile_three_replacer(df$LIT301)
df$LIT401 <- quantile_three_replacer(df$LIT401)
#
#Replace values in FIT101,201,301, with low and high classifiers
df$FIT101 <- quantile_two_replacer(df$FIT101)
df$FIT201 <- quantile_two_replacer(df$FIT201)
df$FIT301 <- quantile_two_replacer(df$FIT301)

#Replace FITS with low, medium, high
df$FIT601 <- quantile_three_replacer(df$FIT601)

#Replace DPIT with Low and High
df$DPIT301 <- quantile_two_replacer(df$DPIT301)

#Set AIT201 to categorical data
df$AIT201 <- quantile_three_replacer(df$AIT201)

#Write processed data
write.csv(df, file="processed.csv", row.names = FALSE)