###########################################################################################################################################################
# Install all Libraries and activate them
install.packages('BatchGetSymbols')
install.packages('pdfetch')
install.packages('MASS')
install.packages('calibrate')
install.packages('zoom')
install.packages('metRology')
install.packages('fitdistrplus')
install.packages("tidyr")
install.packages("dplyr")
install.packages('compare')

library(BatchGetSymbols)
library(pdfetch)
library(MASS)
library(calibrate)
library(zoom)
library(metRology)
library(fitdistrplus)
library(tidyr)
library(compare)
library(dplyr)


###########################################################################################################################################################
# 1. + 2. Aufgabe
###########################################################################################################################################################

#Set-Up of the Project
set.seed(8335819)
first.date <- as.Date("2019-03-31") - 365 - sample(1:20, 1)

set.seed(8335726)
last.date <- as.Date("2019-03-31") + sample(1:20, 1)

tickers <- GetSP500Stocks(TRUE, getwd())$company                        #Get all SP500 Company Tickers
tickers <- append("^GSPC", tickers)                                     #Add the SP500 Index to the list of Tickers (it is now the first ticker)

tickers[tickers=="BRK.B"]="BRK-B"
tickers[tickers=="BF.B"]="BF-B"

#Download Data from Yahoo Finance from first date to last date
df.prices <- as.data.frame(pdfetch_YAHOO(tickers, fields = "adjclose", from = first.date, to = last.date+1))

#Download the Data using BatchGetSymbols
l.out <- BatchGetSymbols(tickers = tickers,
                         first.date = first.date,
                         last.date = last.date)

#Remove Stocks with at least 20% of price values missing to ensure returns and vola is correctly measured; update the ticker list accordingly
#Count the number of available values in the data frame
prices_count <- dim(df.prices)[1]

#Drop Stock Price Data's where more than 20% of values are NA's
df.price_data = df.prices[,colSums(is.na(df.prices)) <= as.integer(prices_count*0.2)]

#Which Stock needs to be dropped?
tickers[colSums(is.na(df.prices)) >= as.integer(prices_count*0.2)]

#Drop the Stock's Ticker from the Ticker list
tickers <- tickers[colSums(is.na(df.prices)) <= as.integer(prices_count*0.2)]


#*********************************************************************************************************************************************************
#Compare the two data tables from both sources

#First we collected all values in one column as that is the way BatchGetSymbols stores its data
batch_adjprices = as.data.frame(l.out$df.tickers)
pdfetch_adjprices <- gather(df.price_data)

#Then I compare the two resulting dataframe (with the one column they each have)
#..Round the values
batch_adjprices$price.adjusted = round(batch_adjprices$price.adjusted, 2)
pdfetch_adjprices$test = round(pdfetch_adjprices$value, 2)
length(batch_adjprices$price.adjusted) == length(pdfetch_adjprices$test)

#Compute the differences
differences <- as.data.frame(batch_adjprices$price.adjusted - pdfetch_adjprices$test)
colnames(differences) <- "Dif"

#Select values that are not equal to zero
vector_diff <- differences[differences['Dif'] != 0]

#Sum of deviations
sum(vector_diff, na.rm=TRUE)

#Sum of deviations as a percentage of the sum of total prices
sum(vector_diff, na.rm=TRUE) / sum(pdfetch_adjprices$test, na.rm=TRUE)
#----> Difference is very very small
#End Comparing the Two Data Frames: We only us the pdFetch Data Frame from here on
#*********************************************************************************************************************************************************

#Rename the first column from "^GSPC" to "SP500" for the reader's better understanding of the data
names(df.price_data)[1] <- "SP500"

#Save values in a csv file
write.csv(df.prices, "sp500.csv")


###########################################################################################################################################################
#3. Aufgabe
###########################################################################################################################################################

# Calculate Matrix of log Returns
df.log_returns <- as.data.frame(apply(df.price_data,2,function(x) diff(log(x), lag=1)))


#Write it in a csv table
write.csv(df.log_returns, "log_returns.csv")

# Mean Matrix + SD Matrix
#Number of trading days in a year to compute annual returns
days <- 252

#Average log return in 252 trading days (one year)
means <- as.data.frame(apply(df.log_returns, 2, mean, na.rm=TRUE)) * days

#Average standard deviation in 252 trading days (one year)
std_devs <- as.data.frame(apply(df.log_returns, 2, sd, na.rm=TRUE)) * sqrt(days)

#Rename the columns in the newly created data frame
names(means) <- c("mean_return")
names(std_devs) <- ("std_dev_return")

#*********************************************************************************************************************************************************
# Mean Plot and fitted to Normal Distribution 
#Compute normal distribution parameters for the mean_returns and save its parameters
para.1 <- fitdistr(means$mean_return, "normal")$estimate 

#Draw histogramm of mean returns
hist(means$mean_return, prob = TRUE, breaks=100)

#Add the fitted normal distribution curve to the diagram
curve(dnorm(x, para.1[1], para.1[2]), col = 2, add = TRUE)

#Add a legend
legend("topleft", inset=0.05, legend=c("Normal Distr. Fit"), col=c("red"), lty=1, cex=0.8, box.lty=1, box.lwd=1, box.col="black")

#*********************************************************************************************************************************************************
#Standard Deviations Plot and fitted to Gamma distribution
#Compute gamma distribution parameters for the standard deviation of returns and save its parameters
para.2 <- fitdistr(std_devs$std_dev_return, "gamma")$estimate

#Draw the histogramm of the standard deviations
hist(std_devs$std_dev_return, prob= TRUE, breaks=100)

#Add the fitted normal distribution curve to the histogramm
curve(dgamma(x, para.2[1], para.2[2]), col = 2, add = TRUE)

#Add a legend
legend("topright", inset=0.01, legend=c("Gamma Distr. Fit"), col=c("red"), lty=1, cex=0.8, box.lty=1, box.lwd=1, box.col="black")

#*********************************************************************************************************************************************************
#Plot Means and Variances (SIMPLE RETURN)
plot(x=std_devs$std_dev_return[-(1)], y=exp(means$mean_return[-(1)])-1, col="black", main="SP500 Stocks:Simple Returns and Std's")

#Plot the Index
points(x=std_devs$std_dev_return[1], y=exp(means$mean_return[1])-1, col="red")

#Add Labels to the Stock Points
textxy(std_devs$std_dev_return[-(1)], exp(means$mean_return[-(1)])-1, tickers[-(1)], offset=0.8, col="black")

#Add label to the Index data point
textxy(std_devs$std_dev_return[1], exp(means$mean_return[1])-1, tickers[1], offset=-0.6, col="red")

#Zoooom in
zm()

#*********************************************************************************************************************************************************
#Plot Means and Variances (LOG RETURNS)
plot(x=std_devs$std_dev_return[-(1)], y=means$mean_return[-(1)], col="black", main="SP500 Stocks: Log Returns and Std's")

#Plot the Index
points(x=std_devs$std_dev_return[1], y=means$mean_return[1], col="red")

#Add Labels to the Stock Points
textxy(std_devs$std_dev_return[-(1)], means$mean_return[-(1)], tickers[-(1)], offset=0.8, col="black")

#Add label to the Index data point
textxy(std_devs$std_dev_return[1], means$mean_return[1], tickers[1], offset=-0.6, col="red")

#Zoooom in
zm()

#*********************************************************************************************************************************************************
#Summary for Vector of Means and Standard Deviations
summary(means$mean_return[-(1)])
summary(std_devs$std_dev_return[-(1)])

###########################################################################################################################################################
# 4. Aufgabe
###########################################################################################################################################################

#Run the regression for all stocks
#Count the number of stocks, minus 1 which is the SP500 Index we are regressing on
stocks <- dim(df.log_returns)[2] -1 

#initialize empty data frame (number of rows is equal to the number of stocks)
df.regression <- data.frame(matrix(ncol = 3, nrow = stocks))

#Name Rows of the empty data frame (labels according to the tickers without the Index)
rownames(df.regression) <- tickers[-(1)]

#Name Columns of the empty data frame 
names(df.regression) <- c("Alpha", "Beta", "Alpha_p_value")       

#Run Regression on SP500 for all stocks
regression <- apply(df.log_returns, 2, function(x){lm(x~df.log_returns$SP500)}) 

#"Kick-Out" regression for SP500 on SP500
regression <- regression[-(1)]                                                         

#Save Alphas in Data Frame
df.regression[,1] <-  as.data.frame(sapply(regression, function(x){summary(x)$coefficients[,1][1]}))*days  

#Save Betas in Data Frame
df.regression[,2] <-  as.data.frame(sapply(regression, function(x){summary(x)$coefficients[,1][2]}))           

#Save P-Values for Alphas in Data Frame
df.regression[,3] <-  as.data.frame(sapply(regression, function(x){summary(x)$coefficients[,4][1]}))            

#**********************************************************************************************************************************************************
#Plot Vector of Betas + its density fit
#Histogramm of Betas
hist(df.regression$Beta, prob = TRUE, breaks=100, main="Histogramm of the Stock's Betas")

#Compute the Normal Fit Parameters using library MASS
fit_b = fitdistr(df.regression$Beta, "normal")$estimate

#Draw the density fit line on the histogramm of betas
lines(density(df.regression$Beta))

#Add the normal fit with the calculated parameters
curve(dnorm(x, fit_b[1], fit_b[2]), col = 2, add = TRUE)

#Add a legend
legend("topright", inset=0.01, legend=c("Normal Distr. Fit", "Density Fit"), col=c("red", "black"), lty=1, cex=0.8, box.lty=1, box.lwd=1, box.col="black")

#**********************************************************************************************************************************************************
#Plot Vector of Alphas + its densitiy fit
#Plot the Histogramm of Alphas
hist(df.regression$Alpha, prob = TRUE, breaks=100, main="Histogramm of the Stock's Alphas")

#Calculate the Normal fit parameters using library MASS
fit_a = fitdistr(df.regression$Alpha, "normal")$estimate

#Draw the density fit line on the histogramm of Alphas
lines(density(df.regression$Alpha))

#Add the normal fit with the calculated parameters
curve(dnorm(x, fit_a[1], fit_a[2]), col = 2, add = TRUE)

#Add a legend
legend("topleft", inset=0.01, legend=c("Normal Distr. Fit", "Density Fit"), col=c("red", "black"), lty=1, cex=0.8, box.lty=1, box.lwd=1, box.col="black")
#**********************************************************************************************************************************************************

#Order Dataframe Regression by Beta and select x smallest/largest ones
df.regression <- df.regression[with(df.regression, order(Beta)),]
small_Betas <- head(df.regression, 10)
large_Betas <- tail(df.regression, 10)

#Select Stocks with Statistically Significant Alphas
significantAlphas <-subset(df.regression, Alpha_p_value < 0.05)

#Plot Amazon against SP500 with regression line
plot(x=df.log_returns$SP500, y=df.log_returns$AMZN)
regressAMZN <- lm(AMZN ~ SP500, data=df.log_returns)
grid()
abline(regressAMZN)
summary(regressAMZN)

###########################################################################################################################################################
# 5. Aufgabe
###########################################################################################################################################################

#Fit to normal distribution and save the parameters
para_n <- fitdistr(df.log_returns$SP500, "normal")$estimate

#Fit to Student t distribution and save parameters
para_t <- fitdistr(df.log_returns$SP500, "t")$estimate

#Print parameters (for both distributions)
print(para_n)                                                                                        
print(para_t)

#Draw histogram of SP500 Log Returns
hist(df.log_returns$SP500, breaks=100, prob=TRUE, main="Histogramm of SP500 Log Returns") 

#Add Normal Distribution Fit to the Plot
curve(dnorm(x, para_n[1], para_n[2]), col = 2, add = TRUE)   

#Add student t Distribution Fit to the Plot
curve(dt.scaled(x, df=para_t[3], mean=para_t[1], sd=para_t[2]), add = TRUE)        

#Add a legend to the plot
legend("topright", inset=0.01, legend=c("Norm Distr. Fit", "Student t Distr. Fit"), col=c("red", "black" ), lty=1, cex=0.8, box.lty=1, box.lwd=1, box.col="black")


