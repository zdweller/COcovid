#Load R packages
library(readr)
library(stringr)
library(lubridate)
library(png)
library(grid)
library(gridExtra)
library(plotrix)
library(tidyverse)
library(readxl)
library(stringr)
library(splines)

#Stay at home order started Thur, March 26
#Safer at home order started Mon, Apri 27

# x-day average function
xday.avg = function(y, days){
	nobs = length(y)
	nmeans = nobs - days + 1
	if(nmeans < 1){
		message("Error! not enough data")
		return(0)
	}
	window = 1:days
	xdaymean = rep(NA, nmeans)
	count = 1
	while(max(window) <= nobs )
	{
		xdaymean[count] = mean(y[window])
		window = window + 1
		count = count + 1
	}
	return(xdaymean)
}

# x-day sum function
xday.sum = function(y, days){
	nobs = length(y)
	nsums = nobs - days + 1
	if(nsums < 1){
		message("Error! not enough data")
		return(0)
	}
	window = 1:days
	xdaysum = rep(NA, nsums)
	count = 1
	while(max(window) <= nobs )
	{
		xdaysum[count] = sum(y[window])
		window = window + 1
		count = count + 1
	}
	return(xdaysum)
}


#Set working directory
setwd("/Users/zacharyweller/Google Drive/COcovid")
#load twitter image
tw <- readPNG("twitter.png")
twg <- rasterGrob(tw, interpolate = T)
#load and process covid data 
files <- list.files("Case data")
files <- files[-length(files)]
#remove duplicated files
bad <- which( str_detect(files, "\\)") | str_detect(files, "xlsx") )
files <- files[-bad]

state_data <- data.frame()
tested <- c()
cases <- c()
deaths <- c()
fdates <- c()
for(f in files){
	#read data -- some are xlsx
	if(str_detect(f, "csv")){
		tmp <-  read_csv(paste0("Case data/", f) )
	}else{
		tmp <- read_excel(paste0("Case data/", f))
		}
	#get dates
	fdates <- c(fdates, substr(f, 22, 31) )
	#get tests
	tested <- c(tested, as.numeric(tmp$value[4]))
	#get cases
	cases <- c(cases, as.numeric(tmp$value[1]))
	#get state data
	state_data <-  data.frame(rbind(state_data, as.numeric(tmp$value[1:6])) )
}
#tidy the data
varnames <- tmp$metric[1:6]
names(state_data) <- varnames
names(state_data)[4] <- "PeopleTested"
report_dates = fdates
report_dates = date(report_dates)
state_data$ReportDate <- report_dates
ReportDate <- state_data$ReportDate
#compute daily data
daily_cases = diff(state_data$Cases)
daily_cases = c(state_data$Cases[1], daily_cases)
daily_tests = diff(state_data$PeopleTested)
daily_tests = c(state_data$PeopleTested[1], daily_tests)
pct_pos = daily_cases/daily_tests
state_data$daily_cases <- daily_cases
state_data$daily_tests <- daily_tests
state_data$pct_pos <- pct_pos
names(state_data)[5] <- "deathacases"
names(state_data)[6] <- "covid19deaths"
daily_dac= diff(state_data$deathacases)
daily_dac = c(state_data$deathacases[1], daily_dac)
state_data$daily_dac = daily_dac

#compute first day, last day for plotting purposes
totalreports <- length(pct_pos)
lastd = as.numeric(report_dates[totalreports])
firstd = as.numeric(report_dates[1])


#######################
####Surveillance plot: ggplot
######################
leftpt <- round(dim(state_data)[1]/5)

gsurv <- ggplot(aes(x = ReportDate, y = pct_pos*100), data = state_data) + geom_line( col = "red", size = 1.0) + geom_point(col = "red", size = 2) + ylim(c(0, 100)) + xlab("Date of Report") + ylab("Percent Positive Tests")
rect_shift = 12
gsurv <-  gsurv + geom_rect(xmin = mean(ReportDate) - rect_shift, xmax = mean(ReportDate)+rect_shift, ymin = 83, ymax = 103, color = "black", fill = "white")
bsize = 2.8
tshift = 0
gsurv <- gsurv + geom_text( aes(x = mean(ReportDate) - tshift, y = 100, label = paste("Total Tests:    ", sum(daily_tests))), size = bsize)
gsurv <- gsurv + geom_text( aes(x = mean(ReportDate) - tshift, y = 93, label = paste("Total Positive: ", sum(daily_cases))), size = bsize)
gsurv <- gsurv + geom_text( aes(x = mean(ReportDate) - tshift, y = 86, label = paste0("Pct Positive:      ", round(sum(daily_cases)/sum(daily_tests)*100,1),"%" ) ), size = bsize )
gsurv <- gsurv + geom_text(aes(x = ReportDate, y = 75, label = as.character(daily_tests), angle = 55), size = 1.25) + geom_text(aes(x =ReportDate[leftpt], y = 83, label = "Number of Daily Tests"), size = 1.5)
gsurv <- gsurv + ggtitle("Surveillance")
gsurv <- gsurv + scale_x_date(date_labels = "%b %d", date_breaks = "9 days")

#gsurv <- gsurv + annotate(geom = "rect", xmin = as.Date("2020-03-26"), xmax = as.Date("2020-04-26"), ymin = -Inf, ymax = 62.5, fill = "pink", alpha = 0.3)
#gsurv <- gsurv + annotate(geom = "rect", xmin = as.Date("2020-04-26"), xmax = max(state_data$ReportDate), ymin = -Inf, ymax = 62.5, fill = "yellow", alpha = 0.3)


#gsurv

####################
### Total cases plot: ggplot
####################
shift = 700
maxc <- max(state_data$Cases) - shift
gtotal <- ggplot(aes(x = ReportDate, y = Cases), data = state_data) + geom_bar(stat = "identity", col = "black") + xlab("Date of Report") + ylab("Total Confirmed Cases") + ggtitle("Total Confirmed Cases")
gtotal <- gtotal + stat_smooth(aes(x = ReportDate, y = Cases), col = "red", se = F, data = state_data)
gtotal <- gtotal + scale_x_date(date_labels = "%b %d", date_breaks = "9 days")  + geom_text(aes(x = report_dates[7],y = maxc+shift/2 , label = "@wellerstats"), col = "deepskyblue2", size = 3.0)

#gtotal


####################
### Total deaths among cases plot: ggplot
####################

maxdac = max(state_data$daily_dac) - 0.1*max(state_data$daily_dac)

gdeath<- ggplot(aes(x = ReportDate, y = daily_dac), data = state_data) + geom_bar(stat = "identity", col = "black") + xlab("Date of Report") + ylab("Daily Deaths Among Cases") + ggtitle("Daily Reported Deaths Among Cases") + stat_smooth(aes(x = ReportDate, y = daily_dac), col = "red", se = F, data = state_data) + scale_x_date(date_labels = "%b %d", date_breaks = "9 days") + geom_text(aes(x = report_dates[8],y = maxdac , label = "@wellerstats"), col = "deepskyblue2", size = 5.0)
gdeath
#gtotal


####################
### Daily cases plot: ggplot
####################
gdaily <- ggplot(aes(x = ReportDate, y = daily_cases), data = state_data) + geom_bar(stat = "identity", col = "black") + xlab("Date of Report") + ylab("Daily Confirmed Cases") + ggtitle("Daily Confirmed Cases") + stat_smooth(aes(x = ReportDate, y = daily_cases), col = "red", se = F, data = state_data) + scale_x_date(date_labels = "%b %d", date_breaks = "9 days")

#gdaily

####################
### Trajectory plot: ggplot
####################
nday = 7
dailycases_xday = xday.sum(state_data$daily_cases, nday)
plot(log(state_data$Cases[-(1:nday-1)], base = 10), log(dailycases_xday, base = 10), type = "b", xlab = "Total Confirmed Cases) ", ylab = "New Confirmed Cases in Last Week")
#create new data set
growthdata <- data.frame("ReportDate" = state_data$ReportDate[-(1:nday-1)], "Cases" = state_data$Cases[-(1:nday-1)],  "lastweektotal" = dailycases_xday)
ggrow <- ggplot(data = growthdata, aes(x = Cases, y = lastweektotal)) + geom_line(col = "red", size = 1) + geom_point(col = "red", size = 2) + xlab("Total Confirmed Cases (log)") + ylab("New Confimed Cases in Last Week (log)") + scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + ggtitle("Case Trajectory")

#ggrow 

####################
### Derivative plot: ggplot
####################
nday = 7
avgdailycases_xday = xday.avg(state_data$daily_cases, nday)
daytmp = 1: length(avgdailycases_xday)
fx = spline(daytmp, avgdailycases_xday, method = "natural",)
plot(fx, type = "l")
points(daytmp, avgdailycases_xday, col = "red", pch = 16)
fxd = splinefun(daytmp, avgdailycases_xday, method = "natural")
der = fxd(daytmp, deriv = 1)
plot(daytmp, der, type = "b")
abline(h = 0)
derivdata = data.frame(ReportDate = state_data$ReportDate[-(1:6)], der)

gderiv<- ggplot(data = derivdata, aes(x = ReportDate, y = der)) + geom_point()+  geom_smooth(col = "red", size = 1, method = "lm", formula = "y ~ bs(x, 6)", se = F) + geom_abline(intercept = 0, slope = 0, color = "blue") + xlab("Report Date") + ylab("Derivative of Average Daily Cases in Last Week") + ggtitle("Derivative of Average Daily Cases in Last Week")

gderiv

#ggrow 

#####################
#Stitch figures together, save plot
#####################

today_long <- format(Sys.time(), form = "%b %d, %Y")
gall <- grid.arrange(gdeath, gdaily, ggrow, gsurv, nrow = 2, top = textGrob(paste("Colorado Covid-19 Reporting: ", today_long), gp = gpar(fontsize = 20, font = 3)) ) 
today <- format(Sys.time(), form = "%b%d")

ggsave(file = paste0("Figures/co_data_",today,".jpeg"), gall, width = 10, height = 8 )
ggsave(file = paste0("Figures/co_data_",today,".pdf"), gall, width = 10, height = 8 )