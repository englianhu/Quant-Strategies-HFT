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
# 1. asset group1 - ES
data.group1 <- data.US[, "ES"]

# the following common assumptions were defined:
# 1.	do not use in calculations the data from the first and last 10 minutes of the session (9:31--9:40 and 15:51--16:00) – put missing values there,

# lets put missing values ofr these periods
data.group1["T09:31/T09:40",] <- NA 
data.group1["T15:51/T16:00",] <-NA

# lets calculate EMA10 and EMA60
data.group1$ES_EMA10 <- EMA(na.locf(data.group1$ES), 10)
data.group1$ES_EMA60 <- EMA(na.locf(data.group1$ES), 60)
# put missing value whenever the original price is missing
data.group1$ES_EMA10[is.na(data.group1$ES)] <- NA
data.group1$ES_EMA60[is.na(data.group1$ES)] <- NA

# lets calculate the position for the MOMENTUM strategy
# if fast MA(t-1) > slow MA(t-1) => pos(t) = 1 [long]
# if fast MA(t-1) <= slow MA(t-1) => pos(t) = -1 [short]
#  caution! this strategy is always in the market !
data.group1$position.mom <- ifelse(lag.xts(data.group1$ES_EMA10) >
                                   lag.xts(data.group1$ES_EMA60),
                                   1, -1)

# lets apply the remaining assumptions
# - exit all positions 15 minutes before the session end, i.e. at 15:45
# - do not trade within the first 20 minutes of stocks quotations (until 9:50)
data.group1$position.mom[times(times_) <= times("9:50:00") | 
                         times(times_) > times("15:45:00")] <- 0

# lets also change every missing position to flat (0)
data.group1$position.mom[is.na(data.group1$position.mom)] <- 0

# calculating gross pnl
data.group1$pnl_gross.mom <- ifelse(is.na(data.group1$position.mom * diff.xts(data.group1$ES) * 50),
                                    0,
                                    data.group1$position.mom * diff.xts(data.group1$ES) * 50)
# number of transactions
data.group1$ntrans.mom <- abs(diff.xts(data.group1$position.mom))
data.group1$position.mom[1] <- 0

# net pnl
data.group1$pnl_net.mom <- data.group1$pnl_gross.mom  -
  data.group1$ntrans.mom * 4 # 4$ per transaction


# aggregate pnls and number of transactions to daily
my.endpoints <- endpoints(data.group1, "days")

data.group1.daily <- period.apply(data.group1[,c(grep("pnl", names(data.group1)),
                                                 grep("ntrans", names(data.group1)))],
                            INDEX = my.endpoints, 
                            FUN = function(x) colSums(x, na.rm = T))

# summarize the strategy for this quarter

# SR
grossSR = mySR(x = data.group1.daily$pnl_gross.mom, scale = 252)
netSR = mySR(x = data.group1.daily$pnl_net.mom, scale = 252)
# average number of transactions
av.daily.ntrades = mean(data.group1.daily$ntrans.mom, na.rm = T)
# PnL
grossPnL = sum(data.group1.daily$pnl_gross.mom)
netPnL = sum(data.group1.daily$pnl_net.mom)
# stat
stat = max(-1, log(1 + av.daily.ntrades)) * abs(netSR) * sign(netSR) * sqrt(abs(netPnL))/10 

# collecting all statistics for a particular quarter

 quarter_stats <- data.frame(quarter = selected_quarter,
                             assets.group = 1,
                             grossSR,
                             netSR,
                             av.daily.ntrades,
                             grossPnL,
                             netPnL,
                             stat,
                             stringsAsFactors = F
                             )

 # collect summaries for all quarters
 if(!exists("quarter_stats.all.group1")) quarter_stats.all.group1 <- quarter_stats else
   quarter_stats.all.group1 <- rbind(quarter_stats.all.group1, quarter_stats)
 
# create a plot of gros and net pnl and save it to png file
png(filename = paste0("pnl_group1_", selected_quarter, ".png"),
    width = 1000, height = 600)

 print( # when plotting in a loop you have to use print()
 plot(cbind(cumsum(data.group1.daily$pnl_gross.mom),
            cumsum(data.group1.daily$pnl_net.mom)),
      multi.panel = F,
      main = paste0("Gross and net PnL for asset group 1, quarter ", selected_quarter), 
      col = c("#377EB8", "#E41A1C"),
      major.ticks = "weeks", 
      grid.ticks.on = "weeks",
      grid.ticks.lty = 3,
      legend.loc = "topleft",
      cex = 1)
 )
 # closing the png device (and file)
 dev.off()
 
 # remove all unneeded objects for group 1
 rm(data.group1, my.endpoints, grossSR, netSR, av.daily.ntrades,
    grossPnL, netPnL, stat, quarter_stats, data.group1.daily)

 gc()
} # end of the loop

write.csv(quarter_stats.all.group1, 
          "quarter_stats.all.group1.csv",
          row.names = F)
