########         High Frequency Trading            ##############
#######     Nijat Aliyev and Umid Suleymanli       ##############
#########     NQ - MSFT, Mean reverting            #############


library(zoo)
library(xts)

library(chron)
library(TTR)
library(caTools)

# setting working directory

setwd("...")

load("data_all_2011Q4.RData")

head(data.US)
tail(data.US)


data.US.r <- 10000*diff.xts(log(data.US))

head(data.US.r)
tail(data.US.r)


data.US.2 <- merge(data.US[, c("NQ", "MSFT")],
                  data.US.r[, c("NQ", "MSFT")])

head(data.US.2)

names(data.US.2)[3:4] <- c("NQ.return","MSFT.return")

# lets see the basic plot
plot(as.zoo(data.frame(data.US.2)))

############################################################################

data.US.2["T09:31/T09:40",]  <- NA 
data.US.2["T15:51/T16:00",]  <- NA


plot(as.zoo(coredata(data.US.2)))


my.endpoints <- endpoints(data.US.2, "days")

US.av.ratio <- period.apply(data.US.2,
                            INDEX = my.endpoints,
                            function(x) mean(x$NQ/x$MSFT, na.rm = T)
)

names(US.av.ratio) <- "av.ratio"

plot(US.av.ratio)

# about 90 MSFT stocks per each unit of NQ stock
# It seems to revert back to the mean whichh is 90

head(US.av.ratio)


index(US.av.ratio) <- index(US.av.ratio) + 60*60*17.5

head(US.av.ratio)

data.US.3 <- merge(data.US.2, US.av.ratio)


data.US.3$av.ratio <- na.locf(data.US.3$av.ratio)

dweek_ <- as.POSIXlt(index(data.US.3))$wday

table(dweek_)


data.US.3$spread <- data.US.3$NQ - data.US.3$av.ratio * data.US.3$MSFT

plot(coredata(data.US.3$spread), type = "l")
abline(h = 0, lty = 2, col = "gray")


data.US.3$spread_rollsd120 <- runsd(data.US.3$spread, 120, 
                                   endrule = "NA", align = "right")

# lets put missings whenever NQE and MSFT price is missing

data.US.3$spread_rollsd120[is.na(data.US.3$NQ)] <- NA
data.US.3$spread_rollsd120[is.na(data.US.3$MSFT)] <- NA

# sample upper and lower bounds
data.US.3$upper <- 3 * data.US.3$spread_rollsd120
data.US.3$lower <- (-3 * data.US.3$spread_rollsd120)

# lets see it on the plot
plot(coredata(data.US.3$spread), type = "l")
lines(coredata(data.US.3$upper), col = "red")
lines(coredata(data.US.3$lower), col = "red")
abline(h = 0, lty = 2, col = "gray")


### position based on relation of the spread to volatlity bands

# lets assume we do not trade within the first 15-mins of the day
# and exit all positions 15 minutes before the end of quotations

# lets create a pos_flat vector and fill it with 0s
pos_flat <- xts(rep(0, nrow(data.US.3)), index(data.US.3))

# we do not trade within the first 20 mins (9:30-9:50) 
# but also before that time since midnight
# and last 20 mins of the session (16:10-16:30)
# but also after this time until midnight

pos_flat["T00:00/T09:50"] <- 1
pos_flat["T16:10/T23:59"] <- 1

# lets use the positionR function from previous labs

source("function_positionR.R")

data.US.3$pos_strategy <- positionR(signal = coredata(data.US.3$spread),
                                   lower = coredata(data.US.3$lower),
                                   upper = coredata(data.US.3$upper),
                                   pos_flat = coredata(pos_flat),
                                   strategy = "mr" # important !!!
)

# lets create a vector of number of transactions

data.US.3$ntrans <- abs(diff(data.US.3$pos_strategy))

# caution !!!
# our strategy pnl would be position*(pnl of the spread)
# pnl of the spread = pos*[diff(NQ)*25$ - m*diffMSFT)]

data.US.3$gross.pnl <- (data.US.3$pos_strategy) *
  (diff.xts(data.US.3$NQ)*25  -
     data.US.3$av.ratio * diff.xts(data.US.3$MSFT))
# pnl after  costs
# costs = 1$ for NQ and 0.2$ for MSFT = (4+m*1) in total
# there is NO minus "-" in the costs - they are always positive !!!

data.US.3$net.pnl <- data.US.3$gross.pnl -
  data.US.3$ntrans * (4 + data.US.3$av.ratio * 0.2)


# lets calculate daily summary

my.endpoints <- endpoints(data.US.3, "days")

pnl_daily <- period.apply(data.US.3,
                          INDEX = my.endpoints,
                          function(x) colSums(x[,c("gross.pnl","net.pnl")],
                                              na.rm = T))

# lets see iy on the graph
# assuming that we trade every day

plot(coredata(cumsum(pnl_daily$gross.pnl)), type = "l")
lines(coredata(cumsum(pnl_daily$net.pnl)), col = "blue")
abline(h=0, lty = 2, col = "gray")


my.endpoints <- endpoints(data.US.2, "days")

####################################################################
# regression coefficient between P1 and P2

US.reg <- period.apply(data.US.2,
                       INDEX = my.endpoints,
                       function(x) coef(lm(NQ ~ MSFT - 1,
                                           data = x,
                                           na.action = na.exclude))
)

names(US.reg) <- "reg.b"

####################################################################
# t-stat of regression coefficient between P1 and P2

# for the whole sample
summary(lm(NQ ~ MSFT - 1, 
           data = data.US.2,
           na.action = na.exclude))$coefficients

# day by day
US.reg.t <- period.apply(data.US.2,
                         INDEX = my.endpoints,
                         function(x) summary(lm(NQ~MSFT - 1,
                                                data = x,
                                                na.action = na.exclude))$coefficients[1,3]
)

names(US.reg.t) <- "reg.t"

####################################################################
# correlation coefficient between P1 and P2

US.cor <- period.apply(data.US.2,
                       INDEX = my.endpoints,
                       function(x) cor(x$NQ, x$MSFT,
                                       use = "complete.obs")
)

names(US.cor) <- "cor"


####################################################################
# cointegration test between P1 and P2
# lets use KPSS test

library(urca)

model.coint <- lm(NQ ~ MSFT - 1, 
                  data = data.US.2,
                  na.action = na.exclude)


kpss.test <- ur.kpss(model.coint$residuals, 
                     type = c("mu"))

summary(kpss.test)

kpss.test@teststat

kpss.test@cval
kpss.test@cval[1, 2]


US.KPSS.stat <- period.apply(data.US.2,
                             INDEX = my.endpoints,
                             function(x) ur.kpss(lm(NQ ~ MSFT - 1,
                                                    data = x,
                                                    na.action = na.exclude
                             )$residuals,
                             type = c("mu"))@teststat
)

names(US.KPSS.stat) <- "KPSS.stat"


# 5% critical value
US.KPSS.cval <- period.apply(data.US.2,
                             INDEX = my.endpoints,
                             function(x) ur.kpss(lm(NQ ~ MSFT - 1,
                                                    data = x,
                                                    na.action = na.exclude
                             )$residuals,
                             type = c("mu"))@cval[1,2]
)

names(US.KPSS.cval) <- "KPSS.cval"


US.daily.calc <- cbind(US.reg, US.reg.t, 
                       US.cor, 
                       US.KPSS.stat, US.KPSS.cval)

US.daily.calc


US.daily.calc[US.daily.calc$KPSS.stat < US.daily.calc$KPSS.cval,]


index(US.daily.calc) <- index(US.daily.calc) + 60*60*17.5
  
rm(US.reg, US.reg.t, US.cor, US.KPSS.stat, US.KPSS.cval)


pnl_daily2 <- merge(pnl_daily, US.daily.calc)

head(pnl_daily2)

pnl_daily2$gross.pnl_reg <- pnl_daily2$gross.pnl * (pnl_daily2$reg.t >= 2) 
pnl_daily2$net.pnl_reg <- pnl_daily2$net.pnl * (pnl_daily2$reg.t >= 2)


pnl_daily2$gross.pnl_cor <- pnl_daily2$gross.pnl * (pnl_daily2$cor >= 0.7)
pnl_daily2$net.pnl_cor <- pnl_daily2$net.pnl * (pnl_daily2$cor >= 0.7)

pnl_daily2$gross.pnl_coint <- pnl_daily2$gross.pnl * (pnl_daily2$KPSS.stat < pnl_daily2$KPSS.cval)
pnl_daily2$net.pnl_coint <- pnl_daily2$net.pnl * (pnl_daily2$KPSS.stat < pnl_daily2$KPSS.cval)


names(pnl_daily2)

colSums(pnl_daily2[, grep("pnl", names(pnl_daily2))], na.rm = T)
.

source("function_SR.R")

apply(pnl_daily2[,grep("pnl", names(pnl_daily2))], 2, 
      function(x) mySR(x, 252))


