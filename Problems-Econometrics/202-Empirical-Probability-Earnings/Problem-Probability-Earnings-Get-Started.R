# Set directory
# setwd("c:/R/workspace")  # Windows
# setwd("~/R/workspace")   # MacOS

# Load dataset
library(readxl)
df1 <- read_xlsx("Age_HourlyEarnings.xlsx",
                 col_names=TRUE,
                 skip=1,
                 trim_ws=TRUE)
View(df1)

# remove empty columns
df1 <- Filter(function(x)!all(is.na(x)), df1)
View(df1)

# remove empty rows by subsetting with the boolean returned by complete.cases
df1 <- df1[complete.cases(df1), ] 
View(df1)

# save cleaned data
save(df1, file = "Age_HourlyEarnings_clean_wide.RData")


# reshape data from wide to long form
library("tidyr")
df2 <- gather(df1, Age, Probability, -AHE)

# make sure Age is numeric or integer
df2$Age <- as.integer(df2$Age)


# Marginal Distribution
# add up the probabilities by Age

# using data in long format | copy-paste for all ages
sum(df2[df2$Age == 25,]$Probability)
sum(df2[df2$Age == 26,]$Probability)
sum(df2[df2$Age == 27,]$Probability)

# split/apply to compute by Age
sapply(split(df2, df2$Age), function(x) sum(x$Probability))

# using data in wide format is convenient to compute row/column sums:
colSums(df1[,names(df1) != "AHE"])
rowSums(df1[,names(df1) != "AHE"])

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

# define a weighted variance function | there is no way to unbias it because the information about the sample size is not available
weighted.variance <- function(x,w) sum(w*(x-weighted.mean(x,w))^2)/sum(w)

# Compute means by age | with dplyr:
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

df %>% summarize(Var = weighted.mean(Var, Probability)/sum(Probability)) 


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

