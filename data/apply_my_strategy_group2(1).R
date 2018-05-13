# setting the working directory if needed
# setwd("...")

library(xts)
library(chron)
library(TTR)
library(knitr) # for nicely looking tables in html files
library(kableExtra) # for even more nicely looking tables in html files
library(quantmod) # for PnL graphs

# lets change the LC_TIME option to English
Sys.setlocale("LC_TIME", "English")

# mySR function
mySR <- function(x, scale) {
  sqrt(scale) * mean(coredata(x), na.rm = T) / 
                sd(coredata(x), na.rm = T)
  } 


# lets define the system time zone as America/New_York (used in the data)
Sys.setenv(TZ = 'America/New_York')

# do it simply in a loop on quarters

for (selected_quarter in c("2011Q1", "2011Q2", "2011Q4")) {

message(selected_quarter)
# loading the data for a selected quarter from a subdirectory "data""

filename_ <- paste0("data/data_all_", selected_quarter, ".RData")

load(filename_)

# create index of times for this quarter

times_ <- substr(index(data.US), 12, 19)

#---------------------------------------------------------------------------
# 2. asset group2 - AAPL, MSFT, NQ
data.group2 <- data.US[, c("AAPL", "MSFT", "NQ")]

# the following common assumptions were defined:
# 1.	do not use in calculations the data from the first and last 10 minutes of the session (9:31--9:40 and 15:51--16:00) – put missing values there,

# lets put missing values ofr these periods
data.group2["T09:31/T09:40",] <- NA 
data.group2["T15:51/T16:00",] <-NA

# lets calculate EMA10 and EMA60 for all series
data.group2$NQ_EMA10 <- EMA(na.locf(data.group2$NQ), 10)
data.group2$NQ_EMA60 <- EMA(na.locf(data.group2$NQ), 60)
data.group2$MSFT_EMA10 <- EMA(na.locf(data.group2$MSFT), 10)
data.group2$MSFT_EMA60 <- EMA(na.locf(data.group2$MSFT), 60)
data.group2$AAPL_EMA10 <- EMA(na.locf(data.group2$AAPL), 10)
data.group2$AAPL_EMA60 <- EMA(na.locf(data.group2$AAPL), 60)
# put missing value whenever the original price is missing
data.group2$NQ_EMA10[is.na(data.group2$NQ)] <- NA
data.group2$NQ_EMA60[is.na(data.group2$NQ)] <- NA
data.group2$MSFT_EMA10[is.na(data.group2$MSFT)] <- NA
data.group2$MSFT_EMA60[is.na(data.group2$MSFT)] <- NA
data.group2$AAPL_EMA10[is.na(data.group2$AAPL)] <- NA
data.group2$AAPL_EMA60[is.na(data.group2$AAPL)] <- NA

# lets calculate the position for the MOMENTUM strategy
# for each asset separately
# if fast MA(t-1) > slow MA(t-1) => pos(t) = 1 [long]
# if fast MA(t-1) <= slow MA(t-1) => pos(t) = -1 [short]
#  caution! this strategy is always in the market !

data.group2$position.NQ.mom <- ifelse(lag.xts(data.group2$NQ_EMA10) >
                                        lag.xts(data.group2$NQ_EMA60),
                                      1, -1)

data.group2$position.MSFT.mom <- ifelse(lag.xts(data.group2$MSFT_EMA10) >
                                        lag.xts(data.group2$MSFT_EMA60),
                                      1, -1)

data.group2$position.AAPL.mom <- ifelse(lag.xts(data.group2$AAPL_EMA10) >
                                        lag.xts(data.group2$AAPL_EMA60),
                                      1, -1)


# lets apply the remaining assumptions
# - exit all positions 15 minutes before the session end, i.e. at 15:45
# - do not trade within the first 20 minutes of stocks quotations (until 9:50)

data.group2$position.NQ.mom[times(times_) <= times("9:50:00") | 
                              times(times_) > times("15:45:00")] <- 0

data.group2$position.MSFT.mom[times(times_) <= times("9:50:00") | 
                                times(times_) > times("15:45:00")] <- 0

data.group2$positionAAPLQ.mom[times(times_) <= times("9:50:00") | 
                                times(times_) > times("15:45:00")] <- 0


# lets also change every missing position to flat (0)
data.group2$position.NQ.mom[is.na(data.group2$position.NQ.mom)] <- 0
data.group2$position.MSFT.mom[is.na(data.group2$position.MSFT.mom)] <- 0
data.group2$position.AAPL.mom[is.na(data.group2$position.AAPL.mom)] <- 0

# calculating gross pnl - for NQ remember to multiply by the point value !!!!
data.group2$pnl_gross.NQ.mom <- ifelse(is.na(data.group2$position.NQ.mom * diff.xts(data.group2$NQ) * 25),
                                       0,
                                       data.group2$position.NQ.mom * diff.xts(data.group2$NQ) * 25)

data.group2$pnl_gross.MSFT.mom <- ifelse(is.na(data.group2$position.MSFT.mom * diff.xts(data.group2$MSFT)),
                                         0,
                                         data.group2$position.MSFT.mom * diff.xts(data.group2$MSFT))

data.group2$pnl_gross.AAPL.mom <- ifelse(is.na(data.group2$position.AAPL.mom * diff.xts(data.group2$AAPL)),
                                         0,
                                         data.group2$position.AAPL.mom * diff.xts(data.group2$AAPL))


# number of transactions

data.group2$ntrans.NQ.mom <- abs(diff.xts(data.group2$position.NQ.mom))
data.group2$position.NQ.mom[1] <- 0

data.group2$ntrans.MSFT.mom <- abs(diff.xts(data.group2$position.MSFT.mom))
data.group2$position.MSFT.mom[1] <- 0

data.group2$ntrans.AAPL.mom <- abs(diff.xts(data.group2$position.AAPL.mom))
data.group2$position.AAPL.mom[1] <- 0


# net pnl
data.group2$pnl_net.NQ.mom <- data.group2$pnl_gross.NQ.mom  -
  data.group2$ntrans.NQ.mom * 4 # 4$ per transaction

data.group2$pnl_net.MSFT.mom <- data.group2$pnl_gross.MSFT.mom  -
  data.group2$ntrans.MSFT.mom * 0.2 # 0.2$ per transaction

data.group2$pnl_net.AAPL.mom <- data.group2$pnl_gross.AAPL.mom  -
  data.group2$ntrans.AAPL.mom * 1 # 1$ per transaction


# aggregate pnls and number of transactions to daily
my.endpoints <- endpoints(data.group2, "days")

data.group2.daily <- period.apply(data.group2[,c(grep("pnl", names(data.group2)),
                                                 grep("ntrans", names(data.group2)))],
                            INDEX = my.endpoints, 
                            FUN = function(x) colSums(x, na.rm = T))

# aggregate the summary over the portfolio - here I assume
# the weights: 34% for NQ, 33% for MSFT and 33% for AAPL

# lets SUM gross and net pnls

data.group2.daily$pnl_gross.mom <- 
  0.34 * data.group2.daily$pnl_gross.NQ.mom +
  0.33 * data.group2.daily$pnl_gross.MSFT.mom +
  0.22 * data.group2.daily$pnl_gross.AAPL.mom
  
data.group2.daily$pnl_net.mom <- 
  0.34 * data.group2.daily$pnl_net.NQ.mom +
  0.33 * data.group2.daily$pnl_net.MSFT.mom +
  0.22 * data.group2.daily$pnl_net.AAPL.mom

# lets SUM number of transactions (with the same weights)

data.group2.daily$ntrans.mom <- 
  0.34 * data.group2.daily$ntrans.NQ.mom +
  0.33 * data.group2.daily$ntrans.MSFT.mom +
  0.22 * data.group2.daily$ntrans.AAPL.mom


# summarize the strategy for this quarter

# SR
grossSR = mySR(x = data.group2.daily$pnl_gross.mom, scale = 252)
netSR = mySR(x = data.group2.daily$pnl_net.mom, scale = 252)
# average number of transactions
av.daily.ntrades = mean(data.group2.daily$ntrans.mom, na.rm = T)
# PnL
grossPnL = sum(data.group2.daily$pnl_gross.mom)
netPnL = sum(data.group2.daily$pnl_net.mom)
# stat
stat = max(-1, log(1 + av.daily.ntrades)) * abs(netSR) * sign(netSR) * sqrt(abs(netPnL))/10 

# collecting all statistics for a particular quarter

 quarter_stats <- data.frame(quarter = selected_quarter,
                             assets.group = 2,
                             grossSR,
                             netSR,
                             av.daily.ntrades,
                             grossPnL,
                             netPnL,
                             stat,
                             stringsAsFactors = F
                             )

 # collect summaries for all quarters
 if(!exists("quarter_stats.all.group2")) quarter_stats.all.group2 <- quarter_stats else
   quarter_stats.all.group2 <- rbind(quarter_stats.all.group2, quarter_stats)
 
 # create a plot of gros and net pnl and save it to png file
 
 png(filename = paste0("pnl_group2_", selected_quarter, ".png"),
     width = 1000, height = 600)
 print( # when plotting in a loop you have to use print()
 plot(cbind(cumsum(data.group2.daily$pnl_gross.mom),
            cumsum(data.group2.daily$pnl_net.mom)),
      multi.panel = F,
      main = paste0("Gross and net PnL for asset group 2, quarter ", selected_quarter), 
      col = c("#377EB8", "#E41A1C"),
      major.ticks = "weeks", 
      grid.ticks.on = "weeks",
      grid.ticks.lty = 3,
      legend.loc = "topleft",
      cex = 1)
 )
 dev.off()
 
 # remove all unneeded objects for group 2
 rm(data.group2, my.endpoints, grossSR, netSR, av.daily.ntrades,
    grossPnL, netPnL, stat, quarter_stats, data.group2.daily)

 gc()
} # end of the loop

write.csv(quarter_stats.all.group2, 
          "quarter_stats.all.group2.csv",
          row.names = F)

