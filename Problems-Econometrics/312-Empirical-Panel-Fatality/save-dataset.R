# load data from package AER
library(AER)
data(Fatalities)
df <- Fatalities
rm(Fatalities)

# save data as xlsx
library(writexl)
write_xlsx(df, "Fatalities.xlsx")

# read data again
library(readxl)
df <- read_xlsx("Fatalities.xlsx", col_names=TRUE)