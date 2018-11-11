# Figure 1.1 
# Datensatz: Historical Daily Bitcoin Market Price (in USD, 16.08.2010-07.07.2016)

# Package XLConnect verwenden:
install.packages("XLConnect")
library(XLConnectJars)
library(XLConnect)
require(XLConnectJars)
require(XLConnect)

# Vorbereitung:
rm(list=ls())
setwd("C:/Users/Coala/Desktop/project1/zeitreihe1") 
list.files()
file_bitcoin <- "bitcoin.xls"

#------------------------------------------------------------------------------------------------------
### Berechnungen ###
#------------------------------------------------------------------------------------------------------

# Daten (Bitcoin Market Price in USD) aus XLS-File lesen:
bitcoin_kurs <- readWorksheetFromFile(file=file_bitcoin, sheet=1, header=TRUE, 
                                      startCol=1, startRow=1, endCol=2)  # endRow default value is number of rows where data entries available

is.data.frame(bitcoin_kurs)
head(bitcoin_kurs, n=10)

dates <- as.Date(bitcoin_kurs[,1])

# a.) log adjusted prices:
log_bitcoin <- bitcoin_kurs[,2]

# b.) log returns:
log_returns <- log_bitcoin[2:nrow(bitcoin_kurs)] - log_bitcoin[1:(nrow(bitcoin_kurs)-1)]

# c.) squared log returns:
log_returns_squared <- log_returns^2

# d.) fourth power of log returns:
log_returns_power4 <- log_returns^4


#------------------------------------------------------------------------------------------------------
### PLOT ###
#------------------------------------------------------------------------------------------------------

par(mfrow=c(2,2), mar=c(2,2,1,1))

# plot log adjusted prices
plot(dates, log_bitcoin, type="l")
mtext("(a)", side=3, cex=0.80)

# plot log returns
plot(dates[-1], log_returns, type="l")
mtext("(b)", side=3, cex=0.80)

# plot log returns (squared)
plot(dates[-1], log_returns_squared, type="l")
mtext("(c)", side=3, cex=0.80)

# plot log returns (fourth power)
plot(dates[-1], log_returns_power4, type="l")
mtext("(d)", side=3, cex=0.80)

#------------------------------------------------------------------------------------------------------







