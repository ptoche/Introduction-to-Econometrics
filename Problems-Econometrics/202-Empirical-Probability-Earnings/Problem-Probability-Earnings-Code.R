# Set directory
if(.Platform$OS.type == "windows"){
    setwd("c:/R/workspace")
} else {
    setwd("~/R/workspace")
}

# Set options
options(digits=3)  # I don't like too many digits

# Load dataset
library(readxl)
df1 <- read_xlsx("Age_HourlyEarnings.xlsx",
                  col_names=TRUE,
                  skip=1,
                  trim_ws=TRUE)
View(df1)

# view data selectively in the console:
df1[ncol(df1)]  # last column
df1[,(ncol(df1)-4):ncol(df1)]  # last 4 columns
df1[6:7]  # if you know exactly which columns to select

# with tidyverse pipe | displays in reverse order:
library(tidyverse)
df1 %>% select(last_col(offset=0:4), everything()) %>% head(5)

# if you want the more compact display of dataframe, rather than tibble:
df1[ncol(df1)] %>% data.frame %>% head(5)

# check the column names
colnames(df1)
## "AHE" "25"  "26"  "27"  "28"  "29"  "30"  "31"  "32"  "33"  "34"


# see if columns have missing values:
complete.cases(df1)
# returns FALSE for every column!

# remove empty columns
df1 <- Filter(function(x)!all(is.na(x)), df1)
View(df1)

# remove empty columns, including empty strings and 0 values
Filter(function(x)!all(is.na(x) || is.null(x) || x == "" || x == 0), df1)

# see if any rows have missing values:
complete.cases(df1)
# the last two rows have missing values

# remove empty rows by subsetting with the boolean returned by complete.cases
df1 <- df1[complete.cases(df1), ] 
View(df1)

# clean up missing values with dplyr
df1 <- read_xlsx("Age_HourlyEarnings.xlsx", col_names=TRUE, skip=1, trim_ws=TRUE)
# remove empty columns:
df1 %>% select_if(~!(all(is.na(.)))) -> df1
View(df1)
# remove empty rows:
df1 %>% filter_all(any_vars(!is.na(.))) -> df1
View(df1)

# save cleaned data
save(df1, file = "Age_HourlyEarnings_clean_wide.RData")

# reshape data from wide to long form
library("reshape2")
df2 <- melt(df1, id="AHE")
View(df2)

# Rename the variables to display nicely in plots:
df2 <- melt(df2, id="AHE", variable.name="Age", value.name="Probability")
View(df2)

# Oh wait, the reshape2 package is deprecated! Let's use tidyr instead
library("tidyr")
df2 <- gather(df1, Age, Probability, -AHE)
# make sure Age is numeric or integer:
df2$Age <- as.integer(df2$Age)
View(df2)

head(df2, 2)
# # A tibble: 6 × 3
#     AHE   Age Probability
#   <dbl> <int>       <dbl>
# 1     5    25     0.00298
# 2     6    25     0.00116

# save data as xlsx for reference
library("writexl")
write_xlsx(df2, "Age_HourlyEarnings_clean_long.xlsx")

# Marginal Distribution
# add up the probabilities by Age

# using data in long format | copy-paste for all ages
sum(df2[df2$Age == 25,]$Probability)
# 0.0849
sum(df2[df2$Age == 26,]$Probability)
## 0.0922
sum(df2[df2$Age == 27,]$Probability)
# 0.0855

# split/apply to compute by Age
sapply(split(df2, df2$Age), function(x) sum(x$Probability))

# you can always write a loop
for (age in 25:34){
    print(sum(df2[df2$Age == age,]$Probability))
}

# using data in wide format is convenient to compute row/column sums:
colSums(df1[,names(df1) != "AHE"])
##     25     26     27     28     29     30     31 
## 0.0849 0.0922 0.0855 0.0934 0.1035 0.1047 0.1039 
## 32     33     34 
## 0.1081 0.1088 0.1150 

rowSums(df1[,names(df1) != "AHE"])
## 0.01708 0.01192 0.02202 0.02115 0.02784 0.04572
## 0.03474 0.06985 0.03743 0.06301 0.04041 0.03249
## 0.05669 0.02842 0.05909 0.03590 0.05233 0.06766
## 0.03649 0.04150 0.04237 0.04862 0.03125 0.02144
## 0.01773 0.00858 0.00974 0.00443 0.01410


# create a spreadsheet with the sums:
d <- df1  # make a copy to preserve original dataframe
# append as column:
d$Total <- rowSums(df1[,names(df1) != "AHE"])
# append as row: add NA to the first column, for alignment
d <- rbind(d, c(NA, colSums(d[,names(d) != "AHE"])))
View(d)
# save data
library("writexl")
write_xlsx(df2, "Age_HourlyEarnings_clean_wide_sums.xlsx")

# Compute means by age

# base R with aggregate | See below for other methods
# using the long-form dataframe:
df2$Index <- 1:nrow(df2) # a trick
dm <- aggregate(Index ~ Age, data=df2, function(i) weighted.mean(df2$AHE[i], df2$Probability[i]))
dm <- setNames(dm, c("Age", "AHE"))
dm
#    Age   AHE
# 1   25  17.6
# 2   26  19.0
# 3   27  19.7
# 4   28  20.2
# 5   29  21.2
# 6   30  21.8
# 7   31  22.6
# 8   32  23.7
# 9   33  23.3
# 10  34  24.1

# add the Probability for each Age group
dm$Probability <- aggregate(df2$Probability, list(df2$Age), FUN=sum)[,2]
dm <- setNames(dm, c("Age", "AHE", "Probability"))
dm
# Age  AHE Probability
# 1   25 17.6      0.0849
# 2   26 19.0      0.0922
# 3   27 19.7      0.0855
# 4   28 20.2      0.0934
# 5   29 21.2      0.1035
# 6   30 21.8      0.1047
# 7   31 22.6      0.1039
# 8   32 23.7      0.1081
# 9   33 23.3      0.1088
# 10  34 24.1      0.1150

# mean AHE
sum(dm$AHE * dm$Probability)
# 21.5

# with split/apply
sapply(split(df2, df2$Age), function(x) weighted.mean(x$AHE, x$Probability))
# 25   26   27   28   29   30   31   32   33   34 
# 17.6 19.0 19.7 20.2 21.2 21.8 22.6 23.7 23.3 24.1 

# with dplyr
df2 %>%
    group_by(Age) %>%
    summarise(AHE = weighted.mean(AHE, Probability))
# # A tibble: 10 × 2
# Age   AHE
# <int> <dbl>
#     1    25  17.6
# 2    26  19.0
# 3    27  19.7
# 4    28  20.2
# 5    29  21.2
# 6    30  21.8
# 7    31  22.6
# 8    32  23.7
# 9    33  23.3
# 10    34  24.1


# with ddply
# plyr has now been superseded by the ``dplyr`` package
# be careful, both plyr and dplyr have a summarise function!
library(plyr)
ddply(df2, .(Age), summarise, AHE = weighted.mean(AHE, Probability))

library(data.table)
setDT(df2)[, .(AHE = weighted.mean(AHE, Probability)), Age]

# define a weighted variance function | there is no way to unbias it because the information about the sample size is not available
weighted.variance <- function(x,w) sum(w*(x-weighted.mean(x,w))^2)/sum(w)

# with the apply/split approach:
dv <- sapply(split(df2, df2$Age), function(x) weighted.variance(x$AHE, x$Probability))
#   25    26    27    28    29    30    31    32    33    34 
# 95.9 129.0 128.8 142.7 152.2 162.8 169.7 208.6 190.0 197.1 


# if you need a data.frame:
dv <- data.frame(Age = names(dv), Variance = dv)
rownames(dv) <- NULL  # to keep things pretty

# variance for all ages:
options(digits=6)
d = merge(dm, dv)
sum(d$Probability * d$Variance)/sum(d$Probability)
# 160.689

# equivalently, since sum(d$Probability)=1
sum(d$Probability * d$Variance)

# expected value of square
weighted.squared.exp <- function(x,w) sum(w*x^2)/sum(w)

de2 <- sapply(split(df2, df2$Age), function(x) weighted.squared.exp(x$AHE, x$Probability))
de2 <- data.frame(Age = names(de2), AHE2 = de2)
rownames(de2) <- NULL  # to keep things pretty

d = merge(dm, de2)
# variance, again
sum((d$AHE2-(d$AHE)^2)*d$Probability)
# 160.689

# mean
sum(d$AHE*d$Probability)
# 21.510

# square of mean
sum(d$AHE*d$Probability)^2
# 462.705

# expectation of square 
sum((d$AHE)^2*d$Probability)
# 466.836


# same, but with dplyr:
library("dplyr")
df2 %>% group_by(Age) %>%
    summarise(Probability = sum(Probability)) -> df
df2 %>% group_by(Age) %>%
    summarise(Mean = weighted.mean(AHE, Probability)) %>%
    left_join(df, by = "Age") -> df
df2 %>% group_by(Age) %>%
    summarise(Var = weighted.variance(AHE, Probability)) %>%
    left_join(df, by = "Age") -> df
df %>% summarize(Mean = weighted.mean(Mean, Probability)) 
# 21,5
df %>% summarize(Var = weighted.mean(Var, Probability)/sum(Probability)) 
# 161

# The sum of the probabilities is 1, so this could be simplified
df %>% summarize(Var = weighted.mean(Var, Probability)) 
# 161


library("ggplot2")
library("scales")
dm <- df[c("Age", "Mean")]
ggplot(data=dm, aes(x=Age, y=Mean)) +
    geom_point(color="blue") +
    ggtitle("Mean Earnings by Age") +
    theme_bw()

# with barchart instead:
ggplot(data=df, aes(x=Age, y=Mean)) +
    geom_bar(stat="identity", fill="cornflowerblue") +
    ggtitle("Mean Earnings by Age") +
    theme_bw()

dv <- df[c("Age", "Var")]
ggplot(data=dv, aes(x=Age, y=Var)) +
    geom_point(color="red") +
    ggtitle("Variance of Earnings by Age") +
    theme_bw()


# Compute means by age | plyr
library(plyr)
ddply(df2, ~Age, summarize, Mean = weighted.mean(AHE, Probability))

# with data.table | messes up with df "in-place"
library(data.table)
DT = setDT(df2)
DT[, Mean := weighted.mean(AHE, Probability), by=Age]


### Our results:

# E[AHE]
sum(d1$AHE*d1$Probability)

# E[AHE^2]
sum(d1$AHE2*d1$Probability)

# E[AHE^2]-(E[AHE])^2
sum(d1$AHE2*d1$Probability) - sum((d1$AHE)^2*d1$Probability)


# E[Age]
sum(d2$Age*d2$Probability)

# E[Age^2]
sum(d2$Age2*d2$Probability)

# E[Age^2]-(E[Age])^2
sum(d2$Age2*d2$Probability) - sum((d2$Age)^2*d2$Probability)

