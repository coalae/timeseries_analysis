# Figure 1.2-1.8
# Datensatz: average annual temperature in central England (1723-1970)

# Package XLConnect verwenden:
#install.packages("XLConnect")
library(XLConnectJars)
library(XLConnect)
require(XLConnectJars)
require(XLConnect)

# install.packages("mFilter")
library(mFilter)

# Vorbereitung:
rm(list=ls())
setwd("C:/Users/Coala/Desktop/project1/zeitreihe2") 
list.files()
file_temperatures <- "avgtemperatures_england.xls"

#------------------------------------------------------------------------------------------------------
### FIGURE 1.2 ###
#------------------------------------------------------------------------------------------------------

# Daten (Avg. Temperaturs in Central England) aus XLS-File lesen:
temperatures <- readWorksheetFromFile(file=file_temperatures, sheet=1, header=TRUE, 
                                      startCol=1, startRow=1, endCol=2)  # endRow default value is number of rows where data entries available
years <- temperatures[,1]

head(temperatures, n=10)

# a.) polynomial trend of degree 1: 
log_temp <- log(temperatures[,2])
y.lm1 <- lm(log_temp ~ years)
y.lm1

# b.) polynomial trend of degree 2: 
y.lm2 <- lm(log_temp ~ years + I(years^2)) # ODER: log_temp ~ poly(years,2)
y.lm2

# c.) polynomial trend of degree 3:
y.lm3 <- lm(log_temp ~ years + I(years^2) + I(years^3))
y.lm3

# d.) polynomial trend of degree 4:
y.lm4 <- lm(log_temp ~ years + I(years^2) + I(years^3) + I(years^4))
y.lm4


# PLOT: 
par(mfrow=c(2,2), mar=c(2,2,1,1))
# a.)
plot(years, log_temp, type="l")
mtext("(a)", side=3, cex=0.80)
lines(years, y.lm1$fitted.values, col="orange", lwd=3)
# b.)
plot(years, log_temp, type="l")
mtext("(b)", side=3, cex=0.80)
lines(years, y.lm2$fitted.values, col="red", lwd=3)
# c.)
plot(years, log_temp, type="l")
mtext("(c)", side=3, cex=0.80)
lines(years, y.lm3$fitted.values, col="darkviolet", lwd=3)
# d.)
plot(years, log_temp, type="l")
mtext("(d)", side=3, cex=0.80)
lines(years, y.lm4$fitted.values, col="violetred4", lwd=3)


#------------------------------------------------------------------------------------------------------
### FIGURE 1.3 ###
#------------------------------------------------------------------------------------------------------

# PLOT (residuals der trends vom grad 1-4): 
par(mfrow=c(2,2), mar=c(2,2,1,1))
# a.)
plot(years, y.lm1$residuals, type="p", pch=20, col="gray50")
mtext("(a)", side=3, cex=0.80)
# b.)
plot(years, y.lm2$residuals, type="p", pch=20 , col="gray50")
mtext("(b)", side=3, cex=0.80)
# c.)
plot(years, y.lm3$residuals, type="p", pch=20 , col="gray50")
mtext("(c)", side=3, cex=0.80)
# d.)
plot(years, y.lm4$residuals, type="p", pch=20 , col="gray50")
mtext("(d)", side=3, cex=0.80)


#------------------------------------------------------------------------------------------------------
### FIGURE 1.4 ###
#------------------------------------------------------------------------------------------------------

# Strukturbruch in linearem Trend
log_temp
years
N <- length(log_temp)

# a.) 1 Strukturbruch in der Steigung
y.lm_break1 <- lm(log_temp ~ years + I(years>1770))
y.lm_break1

# b.) 2 Strukturbrueche in der Steigung
y.lm_break2 <- lm(log_temp ~ years + I(years>1800) + I(years>1920))
y.lm_break2

# c.) 1 Strukturbruch in der Steigung, 1 im Intercept
z <- rep(0,238)
w <- c(z, rep(1,N-238))
y.lm_break3 <- lm(log_temp ~ w + years + I(years>1800) + I(years>1920))
y.lm_break3


# PLOT: 
par(mfrow=c(3,1))
# a.)
plot(years, log_temp, type="l")
mtext("(a)", side=4, cex=0.80)
lines(years, y.lm_break1$fitted.values, col="orange", lwd=3)
abline(v=1770, lwd=1, col="violetred3")
# b.)
plot(years, log_temp, type="l")
mtext("(b)", side=4, cex=0.80)
lines(years, y.lm_break2$fitted.values, col="red", lwd=3)
abline(v=1800, lwd=1, col="springgreen3")
abline(v=1920, lwd=1, col="tan1")
# c.)
plot(years, log_temp, type="l")
mtext("(c)", side=4, cex=0.80)
lines(years, y.lm_break3$fitted.values, col="darkviolet", lwd=3)
abline(v=1800, lwd=1, col="springgreen3")
abline(v=1920, lwd=1, col="tan1")


#------------------------------------------------------------------------------------------------------
### FIGURE 1.5 ###
#------------------------------------------------------------------------------------------------------

# PLOT (residuen der trends mit strukturbruch): 
par(mfrow=c(3,1))
# a.)
plot(years, y.lm_break1$residuals, type="p", pch=20, col="gray50")
mtext("(a)", side=4, cex=0.80)
abline(v=1770, lwd=1, col="violetred3")

# b.)
plot(years, y.lm_break2$residuals, type="p", pch=20 , col="gray50")
mtext("(b)", side=4, cex=0.80)
abline(v=1800, lwd=1, col="springgreen3")
abline(v=1920, lwd=1, col="tan1")
# c.)
plot(years, y.lm_break3$residuals, type="p", pch=20 , col="gray50")
mtext("(c)", side=4, cex=0.80)
abline(v=1800, lwd=1, col="springgreen3")
abline(v=1920, lwd=1, col="tan1")


#------------------------------------------------------------------------------------------------------
### FIGURE 1.6 ###
#------------------------------------------------------------------------------------------------------

# Hodrick-Prescott (HP) filter angewandt auf log_temp
log_temp
years

# a.) lambda = 50mio.
hp1 <- hpfilter(log_temp, type="lambda", freq=50000000) # grosses lambda -> fast linearer trend
# b.) lambda = 1mio.
hp2 <- hpfilter(log_temp, type="lambda", freq=1000000)
# c.) lambda = 500000
hp3 <- hpfilter(log_temp, type="lambda", freq=500000)
# d.) lambda = 50000
hp4 <- hpfilter(log_temp, type="lambda", freq=50000) # kleines lambda -> abweichung vom linearen wenig bestraft


# PLOT: 
par(mfrow=c(2,2), mar=c(2,2,1,1))
# a.)
plot(years, log_temp, type="l")
mtext("(a)", side=3, cex=0.80)
lines(years, hp1$trend, col="orange", lwd=3)
# b.)
plot(years, log_temp, type="l")
mtext("(b)", side=3, cex=0.80)
lines(years, hp2$trend, col="red", lwd=3)
# c.)
plot(years, log_temp, type="l")
mtext("(c)", side=3, cex=0.80)
lines(years, hp3$trend, col="darkviolet", lwd=3)
# d.)
plot(years, log_temp, type="l")
mtext("(d)", side=3, cex=0.80)
lines(years, hp4$trend, col="violetred4", lwd=3)


#------------------------------------------------------------------------------------------------------
### FIGURE 1.7 ###
#------------------------------------------------------------------------------------------------------

# Abweichung des log_temp von den HP filter Trends

# a.) lambda = 50mio.
# b.) lambda = 1mio.
# c.) lambda = 500000
# d.) lambda = 50000

# PLOT
par(mfrow=c(2,2), mar=c(2,2,1,1))
# a.)
plot(years, (log_temp - hp1$trend), type="p", pch=20, col="gray50")
mtext("(a)", side=3, cex=0.80)
# b.)
plot(years, (log_temp - hp2$trend), type="p", pch=20 , col="gray50")
mtext("(b)", side=3, cex=0.80)
# c.)
plot(years, (log_temp - hp3$trend), type="p", pch=20 , col="gray50")
mtext("(c)", side=3, cex=0.80)
# d.)
plot(years, (log_temp - hp4$trend), type="p", pch=20 , col="gray50")
mtext("(d)", side=3, cex=0.80)


#------------------------------------------------------------------------------------------------------
### FIGURE 1.8 ###
#------------------------------------------------------------------------------------------------------

# first differences of log_temp

temp.ts <- ts(temperatures[,2], start=1723) # erstelle zeitreihe 
R <- (temp.ts - lag(temp.ts, k=-1)) / lag(temp.ts, k=-1)

temp.logts <- ts(log(temperatures[,2]), start=1723) # erstelle zeitreihe (log.)
r <- (temp.logts - lag(temp.logts, k=-1)) 

# PLOT
par(mfrow=c(1,1))
plot(R, type="o", col="midnightblue", pch=20)
lines(r, col="darkslategrey")

#------------------------------------------------------------------------------------------------------

