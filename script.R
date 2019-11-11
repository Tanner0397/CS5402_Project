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

degree_calc <- function(x, y) {
  (length(unique(x))-1)*(length(unique(y))-1)
}

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

real_plot <- function(x) {
  v = x[!x %in% boxplot.stats(x)$out]
  print(var(v))
  plot(v)
}

outlier_remove_cor <- function(x, y) {
  v = x[!x %in% boxplot.stats(x)$out]
  u = x[!y %in% boxplot.stats(y)$out]
  cor(u, v)
}

df <- read.table("processed.csv", sep=',', header = TRUE)
df.nominal <- df[, !sapply(df, is.numeric)]

chi.matrix <- pairwise.chi.square.test(df.nominal)
chi.stat.matrix <- pairwise.chi.square.test.stat(df.nominal)

#Upon looking at the following matricies for the chi squared test, we have determined that the following attribues can be dropped
#reasons will be commented

#P101, P103, and P205 all have a strong evidence of depenedence. Drop P103 and P205
df$P203 <- NULL
df$P205 <- NULL

#P301 and MV302, MV304, UV401 has strong evidence to show that of dependence, drop MV302, MV304, UV401
df$MV302 <- NULL
df$MV304 <- NULL
df$UV401 <- NULL
df$MV201 <- NULL

#Many of the attribues have a strong resembplace to a stepping function, we shall be using quantile to detect "low", "med" and "high"
#values and will replace the numeric data to be nominal since the values are all very

#Replace the values in LIT with low med and high classifers
df$LIT101 <- quantile_three_replacer(df$LIT101)
df$LIT301 <- quantile_three_replacer(df$LIT301)
df$LIT401 <- quantile_three_replacer(df$LIT401)

#Replace values in FIT101,201,301, with low and high
df$FIT101 <- quantile_two_replacer(df$FIT101)
df$FIT201 <- quantile_two_replacer(df$FIT201)
df$FIT301 <- quantile_two_replacer(df$FIT301)

#Replace FIT601 with low, medium, high
#df$FIT601 <- quantile_three_replacer(df$FIT601)

#Variance is near zero once outliers are removed
df$FIT501 <- NULL
df$FIT503 <- NULL
df$FIT504 <- NULL

#Replace DPIT with Low and High
df$DPIT301 <- quantile_two_replacer(df$DPIT301)

#Set AIT201 to categorical data
df$AIT201 <- quantile_three_replacer(df$AIT201)


df.numeric <- df[,sapply(df, is.numeric)]
df.test <- df

n = length(df.numeric)

for(i in 1:n) {
  keep <- !df.numeric[, i] %in% boxplot.stats(df.numeric[, i])$out
  df.numeric <- df.numeric[keep, ]
  df.test <- df.test[keep, ]
}



