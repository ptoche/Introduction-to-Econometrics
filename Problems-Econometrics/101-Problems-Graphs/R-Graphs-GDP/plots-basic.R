# Directory
if(.Platform$OS.type == "windows"){
    setwd("c:/R/workspace")
} else { 
    setwd("~/R/workspace")
}

# Data
df <- read.csv("GDP.csv", stringsAsFactors=FALSE)

# Looksee
View(df)

# Make Dates
# Examples of format: %y -> 21 | %Y -> 2021 | %m -> 12 | %b -> Dec |
as.Date("2021-12-01", format="%Y-%m-%d")
# "2021-12-01"
df$date = as.Date(df$DATE, format="%Y-%m-%d")

# Check: DATE is a string (chr stands for character) | Date is a Date
str(df)
# 'data.frame':	300 obs. of  3 variables:
#     $ DATE: chr  "1947-01-01" "1947-04-01" "1947-07-01" "1947-10-01" ...
# $ GDP : num  243 246 250 260 266 ...
# $ date: Date, format: "1947-01-01" ...

# We can start making plots now
plot(df$date, df$GDP)  # Wow that's ugly!

# We can be a little fancy
pdf("plot-gdp-nominal-logscale-basic.pdf", width=7, height=5)
plot(data=df, GDP~date, log='y', pch='.', col='blue', cex=2, main='Nominal GDP for the United States', xlab='Year', ylab='Billions of USD')
dev.off() # needed after call to pdf() function

# Better plot with ggplot2
# install.packages('gglot2', 'scales')
library('ggplot2')
library('scales')  # provides label=comma

# first pass
ggplot(data=df, aes(x=date, y=GDP)) + geom_point()

# improved version
ggplot(data=df, aes(x=date, y=GDP)) + geom_line() + theme_classic()

# customized version:
ggplot(data=df, aes(x=date, y=GDP)) + 
    geom_line(color='firebrick') + 
    scale_y_continuous(trans='log10', label=comma) +
    labs(y='GDP (Billions of US $)', x='Year', title='Nominal GDP in the United States') +
    theme_classic()
ggsave("plot-gdp-nominal-logscale-ggplot.pdf", width=7, height=5)

# to set defaults for ggsave | disables the ability to tweak width/height!
ggsave <- function(..., width=7, height=5) ggplot2::ggsave(..., width=width, height=height)

# Easily updated plot with recession lines
library('fredr')  # install.packages('fredr')
api_key = 'copy your private key here'  # or store your key in a file and do not share it
source('fred.R')  # file contains the single line: api_key = 'my private key'
fredr_set_key(api_key)

# function to get data via FRED api
get_data <- function(series, start="1947-01-01", end="2022-01-01", frequency="q", units="lin"){
    # create an empty dataframe with correct dates for merging several series
    d0 = as.Date(start)
    d1 = as.Date(end)
    date = seq.Date(d0, d1, by=paste("1", frequency))
    df = data.frame(date = date)
    for (id in series){
        # query data via fred api
        d <- fredr(
            series_id=id,
            observation_start=d0,
            observation_end=d1,
            frequency=frequency,
            units=units
        )
        # rename value column with series id | or d$id <- d$value
        names(d)[names(d) == 'value'] <- id
        # keep only the date and value
        d <- d[, c("date", id)]
        # merge with previous series
        df <- merge(df, d, by="date")
    }
    # return merged dataframe
    df
}

series = list("GDP", "GDPC1", "USREC")
df <- get_data(series)

# save the data locally and reuse it
# write.csv(df, file="GDP-USREC.csv", row.names = FALSE)
# df <- read.csv("GDP-USREC.csv", stringsAsFactors=FALSE)
# df$date <- as.Date(df$date, format="%Y-%m-%d")  # must have Date object!

# Transform recession data into start/end format
# Note: to get useful ymin, ymax, reapply format_nber for each subset
format_nber <- function(df, ymin, ymax){
    # convert series to boolean
    y = as.logical(df$USREC)
    # get initial state
    x1 = y[1]
    # identify switches
    x = c(x1, diff(y) != 0)
    # create start/end of recessions
    xmin = df$date[with(df, ifelse(x, y, FALSE))]
    xmax = df$date[with(df, ifelse(x, !y, FALSE))]
    # if mismatched pair, extend period to add matching end date
    if (length(xmax) < length(xmin)){
        delta <- df$date[2]-df$date[1]
        xmax <- c(xmax, max(df$date)+delta)
    }
    # output a dataframe with xmin, xmax and (optional) ymin,ymax
    dr = data.frame(xmin = xmin, xmax = xmax, ymin=ymin, ymax=ymax)
}

# recreate the dataframe every time to get correct limits
dr = format_nber(df, ymin=min(df$GDP), ymax=max(df$GDP))


# Compute growth rate of GDP

# with base-R
f <- function(x, lag=1) c(rep(NA,lag), exp(diff(log(x),lag=lag))-1)
df$g1 <- f(df$GDP, lag=1)
df$rg1 <- f(df$GDPC1, lag=1)

# with the dplyr function
# Warning: base-R has a lag function - it's different!
library(dplyr)  # <- needed to access a convenient lag function
f_dplyr <- function(x) (x/lag(x,4)-1)
df$g4 <- f_dplyr(df$GDP)
df$rg4 <- f_dplyr(df$GDPC1)

# save for reference
# write.csv(df, file="GDP-USREC-Growth.csv", row.names = FALSE)


# Subset data to period of interest
limits = as.Date(c("1970-01-01", "1980-01-01"))
df1 <- df[with(df, date >= limits[1] & date <= limits[2]),]
limitz = as.Date(c("1969-07-01", "1980-04-01")) # extend recession data
dfz <- df[with(df, date >= limitz[1] & date <= limitz[2]),] # temp use
dr1 = format_nber(df=dfz, ymin=min(dfz$GDP), ymax=max(dfz$GDP))


# plot nominal GDP with NBER Recession shaded areas
ggplot(data=df1, aes(x=date, y=GDP)) + 
    geom_line(color='firebrick') + 
    scale_y_continuous(trans='log10', label=comma, n.breaks = 8) +
    scale_x_date(date_breaks="1 year", date_labels="%Y") +
    labs(y='GDP (Billions of US $)', x='Year', title='Nominal GDP in the United States') +
    geom_rect(data=dr1, inherit.aes=FALSE,
              aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=Inf), 
              fill='gray', alpha=0.3) +
    theme_classic() + 
    theme(panel.grid.major.y=element_line(size=.5, color="gray", linetype="dotted"))
ggsave("plot-gdp-nominal-logscale-nber.pdf")

# plot quarterly growth rate with NBER Recession shaded areas
ggplot(data=df1, aes(x=date, y=g1)) + 
    geom_line(color='firebrick') + 
    scale_x_date(date_breaks="1 year", date_labels="%Y") +
    labs(y='Quarterly Percentage Change (%)', x='Year', title='Nominal GDP Growth in the United States') +
    geom_rect(data=dr1, inherit.aes=FALSE,
              aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf), 
              fill='gray', alpha=0.3) +
    theme_classic() + 
    theme(panel.grid.major.y=element_line(size=.5, color="gray", linetype="dotted"))
ggsave("plot-gdp-nominal-growth-q-nber.pdf")

# plot year-on-year growth rate with NBER Recession shaded areas
ggplot(data=df1, aes(x=date, y=g4)) + 
    geom_line(color='firebrick') + 
    scale_x_date(date_breaks="1 year", date_labels="%Y") +
    labs(y='Year-On-Year Percentage Change (%)', x='Year', title='Nominal GDP Growth in the United States') +
    geom_rect(data=dr1, inherit.aes=FALSE,
              aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf), 
              fill='gray', alpha=0.3) +
    theme_classic() + 
    theme(panel.grid.major.y=element_line(size=.5, color="gray", linetype="dotted"))
ggsave("plot-gdp-nominal-growth-yoy-nber.pdf")

# To plot real and nominal GDP together, we "align" the real GDP series
# using first values in the series
df1$`Real GDP` <- df1$GDPC1 * head(df1$GDP,1)/head(df1$GDPC1,1)

# To plot several series, it is more convenient to have the data in long form
library("reshape2")
dm1 <- melt(df1[c("date", "GDP", "Real GDP")], id="date")  # subsetting data

# plot real and nominal GDP together
ggplot(data=dm1, aes(x=date, y=value, color=variable)) + 
    geom_line() + 
    scale_x_date(date_breaks="1 year", date_labels="%Y") +
    scale_y_continuous(trans="log10", label=comma, n.breaks = 8) +
    scale_color_manual(name="", values=c("firebrick", "darkblue"), labels=c("Nominal GDP", "Real GDP")) +
    labs(y='GDP (Billions of US $)', x='Year', title='Nominal and Real GDP in the United States') +
    geom_rect(data=dr1, inherit.aes=FALSE,
              aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=Inf), 
              fill="gray", alpha=0.3) +
    theme_classic() + 
    theme(panel.grid.major.y=element_line(size=.5, color="gray", linetype="dotted")) +
    theme(legend.position=c(0.75, 1))
ggsave("plot-gdp-nominal-real-logscale-nber.pdf")


# To plot several series, it is more convenient to have the data in long form
library("reshape2")
dm2 <- melt(df1[c("date", "g4", "rg4")], id="date")  # subsetting data

# plot real and nominal year-on-year GDP growth rates together
ggplot(data=dm2, aes(x=date, y=value, color=variable)) + 
    geom_line() + 
    scale_x_date(date_breaks="1 year", date_labels="%Y") +
    scale_color_manual(name="", values=c("firebrick", "darkblue"), labels=c("Nominal GDP", "Real GDP")) +
    labs(y='Year-On-Year Percentage Change (%)', x='Year', title='Nominal and Real GDP in the United States') +
    geom_rect(data=dr1, inherit.aes=FALSE,
              aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf), 
              fill="gray", alpha=0.3) +
    theme_classic() + 
    theme(panel.grid.major.y=element_line(size=.5, color="gray", linetype="dotted")) +
    theme(legend.position=c(0.68, 0.98))
ggsave("plot-gdp-nominal-real-growth-nber.pdf")


# plot real GDP with NBER Recession shaded areas | theme experiments
# recession of March-May 2021 does not appear! too short? Fix it!
df[with(df, date == as.Date("2021-04-01")), "USREC"] <- TRUE
# get recession bars | notice the extra think bar in 2020!
drr = format_nber(df, ymin=min(df$GDPC1), ymax=max(df$GDPC1))

ggplot(data=df, aes(x=date, y=GDPC1)) + 
    geom_line(color='cornflowerblue') + 
    scale_y_continuous(trans='log10', label=comma, n.breaks = 8) +
    scale_x_date(breaks = seq(as.Date("1950-01-01"), as.Date("2020-01-01"), by="10 year"), date_labels="%Y") +
    labs(y='Real GDP (Billions of Chained 2012 US $)', x='Year', title='Real GDP in the United States') +
    geom_rect(data=drr, inherit.aes=FALSE,
              aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=Inf), 
              fill='red', alpha=0.2) +
    theme_classic() + 
    theme(panel.grid.major.y=element_line(size=.5, color="gray", linetype="dotted"))

p <- last_plot()  # to use in experiments
ggsave(plot=p, "plot-gdp-real-logscale-nber-classic.pdf")

ggsave(plot=p+theme_bw(), "plot-gdp-real-logscale-nber-bw.pdf")
ggsave(plot=p+theme_gray(), "plot-gdp-real-logscale-nber-gray.pdf")
ggsave(plot=p+theme_dark(), "plot-gdp-real-logscale-nber-dark.pdf")
ggsave(plot=p+theme_minimal(), "plot-gdp-real-logscale-nber-minimal.pdf")

# Go crazy | You would really need to tweak colors for line and areas too!
library("ggthemes")
ggsave(plot=p+theme_wsj(), "plot-gdp-real-logscale-nber-wsj.pdf")
ggsave(plot=p+theme_hc(), "plot-gdp-real-logscale-nber-hc.pdf")
ggsave(plot=p+theme_fivethirtyeight(), "plot-gdp-real-logscale-nber-538.pdf")
ggsave(plot=p+theme_economist(), "plot-gdp-real-logscale-nber-economist.pdf")
ggsave(plot=p+theme_excel(), "plot-gdp-real-logscale-nber-excel.pdf")

