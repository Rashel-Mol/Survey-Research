# (Install and) load required packages
library(texreg)       # Nice tables (mostly for regression)
library(rstudioapi)   # Change directories easily
library(haven)        # Read and write Stata, SPSS, or SAS data sets 
library(psych)        # Procedures for Psychological/Metric Research
library(dplyr)        # Data manipulation tools
library(robustbase)   # Basic robust statistics (here: robust regression)
library(stargazer)    # well-formatted tables, such as for summary statistics
library(GGally)       # Data diagnostics and graphics
library(summarytools) # Data summary tools
library(lavaan)       # LAtent VAriable ANalysis 
library(semPlot)      # Diagrams of models estimated with lavaan

# Set working directory
setwd("C:/Users/Rashel/Dropbox/Studie/Survey-Research")

# Read data
dir.create('data')
dir.create('src')
data82total<-read.csv("data/rawdata.csv")

# Start sending analysis output to text-file with "name.txt" and to screen (split = TRUE)
dir.create('gen')
dir.create('gen/temp')
sink("gen/temp/temp_output.txt") 

# Start sending plotting output to pdf file with "name.pdf"
pdf("gen/temp/temp_output.pdf")

# --- Data cleaning --- #

data82total$weird1 <- 0
data82total$weird2 <- 0

data82total$missing <- is.na(data82total$v11)
data82total$missing.num <- as.numeric(data82total$missing)

model.probit <- glm(missing.num ~ v15 + v16 + v17, 
                    family = binomial          # Distribution-family is binomial
                    (link = "probit"),         # Link function is probit
                    data = data82total)        # Indicates which data frame is used
summary(model.probit)

# Recode values that are not possible on a Likert scale. Since I am not sure it was a mis-type and that it was actually the highest value (5), I assigned all the impossible values as missing.  
data82total$v1[data82total$v1 > 5] <- NA
data82total$v2[data82total$v2 > 5] <- NA
data82total$v3[data82total$v3 > 5] <- NA
data82total$v4[data82total$v4 > 5] <- NA
data82total$v5[data82total$v5 > 5] <- NA
data82total$v6[data82total$v6 > 5] <- NA
data82total$v7[data82total$v7 > 5] <- NA
data82total$v8[data82total$v8 > 5] <- NA
data82total$v9[data82total$v9 > 5] <- NA
data82total$v10[data82total$v10 > 5] <- NA
data82total$v11[data82total$v11 > 5] <- NA
data82total$v12[data82total$v12 > 5] <- NA
data82total$v13[data82total$v13 > 5] <- NA
data82total$v14[data82total$v14 > 5] <- NA

# Check if there are no out-of-range values in the Likert variables anymore. 
dfSummary(data82total)

data82total$missing2 <- 0
data82total$missing2[data82total$id == 104] <- 1
data82total$missing2[data82total$id == 701] <- 1
data82total$missing2[data82total$id == 553] <- 1

model.probit2 <- glm(missing2  ~ v15+ v16 + v17, 
                     family = binomial          # Distribution-family is binomial
                     (link = "probit"),         # Link function is probit
                     data = data82total)        # Indicates which data frame is used
summary(model.probit2)

dim(data82total)
dim(na.omit(data82total))

data82 <- na.omit(data82total)


# Add sd
data82$sd <- apply(data82[2:15], 1 , sd)

# Assign variable weird = 1 for straight liners
data82$weird1[data82$sd == 0] <- 1
data82 <- data82[order(data82$weird1),]

# Identify duplicate cases (using R-package dplyr)
# Count n-times duplicate cases using specified variables 
data82 <- add_count(data82, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17,
                    name = "duplicate") # Name of count variable is "duplicate"            

# Sort data on "duplicate" with highest count first. Respondents 111 and 321 are probably duplicates. 
data82 <- data82[order(-data82$duplicate),]

# Mark one of the duplicates as weird
data82$weird1[data82$id == 111] <- 1

# Assess skewness of v16 and v17 (using R-package psych)
# No skew: skew = 0. Right skewed: skew > 0. Left skewed: skew < 0
# Skew v16
skew_v16 <- round(skew(data82$v16),2)               
skew_v16

# Skew v17
skew_v17 <- round(skew(data82$v17),2)               
skew_v17

# Check whether there are outliers (|z| > 3.33)
# Make Z-scores from V17, using the function scale(), and add new variable to data82
data82$Z_v17 <- scale(data82$v17, center=TRUE, scale=TRUE)
data82 <- data82[order(-data82$Z_v17),]
hist(data82$Z_v17)
boxplot(data82$Z_v17)

# Transform y into log(y)
data82$v17.t <- log(data82$v17)                            

# Check skewness and outliers now
skew_v17.t <- round(skew(data82$v17.t),2) 
skew_v17.t

data82$Z_v17.t <- scale(data82$v17.t, center=TRUE, scale=TRUE)
data82 <- data82[order(-data82$Z_v17.t),]
hist(data82$Z_v17.t)
boxplot(data82$Z_v17.t)

data82$weird2[data82$Z_v17.t > 3.33] <- 1
data82$weird2[data82$Z_v17.t < -3.33] <- 1


write.csv(data82, "gen/temp/clean_data.csv", row.names = FALSE)

dir.create('gen/output')


