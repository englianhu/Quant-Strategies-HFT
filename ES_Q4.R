######## PROJECT HFT ##############
### Nijat Aliyev and Umid Suleymanli, ES  #############

library(zoo)
library(xts)
library(fBasics)
library(quantreg)
library(urca)   # Cointegration
library(MSBVAR) # Granger causality
library(TTR)    # Efficient for SMA(), EMA() functions
library(caTools) # Efficient for runmean(), runsd(), runquantile()
library(chron)  # For times() function
library(tseries) 
library(PerformanceAnalytics)

source("function_SR.R")
# setting the working directory

setwd("...")
Sys.setenv(TZ = 'America/New_York')


load("data_all_2011Q4.RData")

# loading additional function

source("function_SR.R")
source("function_positionR.R") # single Vol breakout

#View(data.US)

# First lets select ES

ES <- data.US$ES
names(ES) <- "ES"

head(ES)
tail(ES)

data.US["T09:31/T09:40",] <- NA 
data.US["T15:51/T16:00",]  <-NA
ES["T09:31/T09:50",] <- NA 
ES["T15:41/T16:00",]  <-NA

# lets create an xts object "pos_flat" 
# = 1 if position has to be = 0 
# = 0 otherwise

pos_flat <- xts(rep(0, nrow(ES)),index(ES),tzone = 'America/New_York')
pos_flat["T15:45/T09:30"] <- 1

# we also need to add 1s for weekends 
# lets save the day of the week (0=Sun, 1=Mon, 2=Tue,...,6=Sat)

dweek_ <- as.POSIXlt(index(ES))$wday
head(dweek_)
table(dweek_)

# lets create a vector of times in a character format

time_ <- substr(index(ES), 12, 19)

pos_flat[(dweek_ == 5 & times(time_) > times("16:00:00")) |   # end of Friday
           (dweek_ == 6) |                                      # whole Saturday
           (dweek_ == 0 & times(time_) <= times("23:59:59")),] <- 1 # whole Sunday

################################################################################################################
# strategy: volatility breakout model
################################################################################################################


# save time index for the data processed
# (not to generate it in every iteration of the loop)
index_ <- index(ES)

# system.time() function measures time of execution
+
  system.time(
    for(signalEMA in 30)
    {
      for(slowEMA in 70)
      {
        for(volat.sd in 90)
        {
          for(m_ in 2)
          {
            print(paste("signalEMA = ", signalEMA,
                        ", slowEMA = ", slowEMA,
                        ", volat.sd = ", volat.sd,
                        ", m_ = ", m_, sep = "")) 
            
            # calculating elements of the strategy
            signalEMA.values <- EMA(coredata(na.locf(ES$ES)), signalEMA)
            slowEMA.values <- EMA(coredata(na.locf(ES$ES)), slowEMA)
            volat.sd.values <- runsd(coredata(na.locf(ES$ES)), volat.sd, 
                                     endrule = "NA", align = "right")
            
            # put missing values whenever the original price is missing
            signalEMA.values[is.na(ES$ES)] <- NA
            slowEMA.values[is.na(ES$ES)] <- NA
            volat.sd.values[is.na(ES$ES)] <- NA
            
            # position for momentum strategy
            pos.mom <- positionR(signal = signalEMA.values,
                                 lower = slowEMA.values - m_ * volat.sd.values,
                                 upper = slowEMA.values + m_ * volat.sd.values,
                                 pos_flat = coredata(pos_flat),
                                 strategy = "mom" # important !!!
            )
            
            # position for mean-rev strategy is just a reverse of pos.mom
            pos.mr <- (-pos.mom)
            
            # gross pnl
            pnl.gross.mom <- ifelse(is.na(pos.mom * diff.xts(coredata(ES$ES))),
                                    0, pos.mom * diff.xts(coredata(ES$ES))*50 )
            pnl.gross.mr <- (-pnl.gross.mom)
            
            # nr of transactions - the same for mom and mr
            ntrans <- abs(diff.xts(pos.mom))
            ntrans[1] <- 0
            
            # net pnl
            pnl.net.mom <- pnl.gross.mom - ntrans * 4 # 1$ per transaction of ES
            pnl.net.mr <- pnl.gross.mr - ntrans * 4 # 1$ per transaction of ES
            
            # aggregate to daily
            ends_ <- endpoints(data.US, "days")
            
            pnl.gross.mom.d <- period.apply(pnl.gross.mom, INDEX = ends_, 
                                            FUN = function(x) sum(x, na.rm = T))
            pnl.gross.mr.d <- period.apply(pnl.gross.mr, INDEX=ends_, 
                                           FUN = function(x) sum(x, na.rm = T))
            pnl.net.mom.d <- period.apply(pnl.net.mom, INDEX = ends_,
                                          FUN = function(x) sum(x, na.rm = T))
            pnl.net.mr.d <- period.apply(pnl.net.mr, INDEX = ends_, 
                                         FUN = function(x) sum(x, na.rm = T))
            ntrans.d <- period.apply(ntrans,INDEX = ends_, 
                                     FUN = function(x) sum(x, na.rm = T))
            
            # calculate summary measures
            gross.SR.mom <- mySR(pnl.gross.mom.d, scale = 252)
            gross.SR.mr <- mySR(pnl.gross.mr.d, scale = 252)
            net.SR.mom <- mySR(pnl.net.mom.d, scale = 252)
            net.SR.mr <- mySR(pnl.net.mr.d, scale = 252)
            
            days_ <- index_[ends_]
            av.daily.ntrans <- mean(ntrans.d[as.POSIXlt(days_)$wday != 6], na.rm=T) # excluding Saturdays
            
            # summary of a particular strategy
            summary_ <- data.frame(signalEMA = signalEMA,
                                   slowMA = slowEMA,
                                   volat.sd = volat.sd,
                                   m = m_,
                                   period = "2011Q4",
                                   gross.SR.mom,
                                   gross.SR.mr,
                                   net.SR.mom,
                                   net.SR.mr,
                                   av.daily.ntrans,
                                   stringsAsFactors = F
            )
            
            # putting all summaries together
            if(!exists("summary.all.breakout")) summary.all.breakout <- summary_ else
              summary.all.breakout <- rbind(summary.all.breakout, summary_)
            
            # deleting working files not needed any more
            rm(gross.SR.mom, gross.SR.mr, net.SR.mom, net.SR.mr,
               av.daily.ntrans,
               pnl.gross.mom.d, pnl.gross.mr.d, pnl.net.mom.d, pnl.net.mr.d,ntrans.d,
               ntrans,
               pos.mr, pos.mom, ends_, summary_,
               signalEMA.values, slowEMA.values, volat.sd.values
            )
            
          } # end of loop for m_
        } # end of loop for volatility  
      } # end of loop for slowEMA
    })  # end of loop for signal

# lets order strategies according to decreasing net.SR.mr
summary.all.breakout_mr <- summary.all.breakout[order(-summary.all.breakout$net.SR.mr),]

# "-" is used for the decreasing order 
head(summary.all.breakout_mr <- summary.all.breakout[order(-summary.all.breakout$net.SR.mr),])


# lets order strategies according to decreasing net.SR.mr
summary.all.breakout_mom <- summary.all.breakout[order(-summary.all.breakout$net.SR.mom),]

# "-" is used for the decreasing order 
head(summary.all.breakout_mom <- summary.all.breakout[order(-summary.all.breakout$net.SR.mom),])

pnl.gross.mr.2011Q <- sum(coredata(pnl.gross.mr))
pnl.net.mr.2011Q <- sum(coredata(pnl.net.mr))
pnl.gross.mom.2011Q <- sum(coredata(pnl.gross.mom))
pnl.net.mom.2011Q <- sum(coredata(pnl.net.mom))



write.csv(summary.all.breakout_mr, file="sum_mr_ES_Q4.csv", row.names = F)
write.csv(summary.all.breakout_mom, file="sum_mom_ES_Q4.csv", row.names = F)


summary.pnl.2011q <- cbind(pnl.gross.mom.2011Q,pnl.net.mom.2011Q,pnl.gross.mr.2011Q,pnl.net.mr.2011Q)
summary.pnl.2011q
