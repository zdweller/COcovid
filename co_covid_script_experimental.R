#Load R packages
library(readr)
library(stringr)
library(lubridate)
library(png)
library(grid)
library(gridExtra)
library(plotrix)
library(tidyverse)

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
files <- list.files("COVID-19 Website Data Files")
files <- files[-1]
state_data <- data.frame()
tested <- c()
cases <- c()
deaths <- c()
fdates <- c()
for(f in files){
	#read data
	tmp <-  read_csv(paste0("COVID-19 Website Data Files/", f) )
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
#compute daily data
daily_cases = diff(state_data$Cases)
daily_cases = c(state_data$Cases[1], daily_cases)
daily_tests = diff(state_data$PeopleTested)
daily_tests = c(state_data$PeopleTested[1], daily_tests)
pct_pos = daily_cases/daily_tests
state_data$daily_cases <- daily_cases
state_data$daily_tests <- daily_tests
state_data$pct_pos <- pct_pos

#compute first day, last day for plotting purposes
totalreports <- length(pct_pos)
lastd = as.numeric(report_dates[totalreports])
firstd = as.numeric(report_dates[1])


#######################
####Surveillance plot: ggplot
######################
leftpt <- round(dim(state_data)[1]/5)
gsurv <- ggplot(aes(x = ReportDate, y = pct_pos*100), data = state_data) + geom_line( col = "red", size = 1.0) + geom_point(col = "red", size = 2) + ylim(c(0, 100)) + xlab("Date of Report") + ylab("Percent Positive Tests")
gsurv <-  gsurv + geom_rect(xmin = max(state_data$ReportDate) - 10, xmax = max(state_data$ReportDate), ymin = 83, ymax = 103, color = "black", fill = "white")
bsize = 2.8
gsurv <- gsurv + geom_text( aes(x = max(state_data$ReportDate) - 5, y = 100, label = paste("Total Tests:    ", sum(state_data$daily_tests))), size = bsize)
gsurv <- gsurv + geom_text( aes(x = max(state_data$ReportDate) - 5, y = 93, label = paste("Total Positive:   ", sum(state_data$daily_cases))), size = bsize)
gsurv <- gsurv + geom_text( aes(x = max(state_data$ReportDate) - 5, y = 86, label = paste0("Pct Positive:      ", round(sum(state_data$daily_case)/sum(state_data$daily_tests)*100,1),"%" ) ), size = bsize )
gsurv <- gsurv + geom_text(aes(x = ReportDate, y = 75, label = as.character(daily_tests), angle = 45), size = 2) + geom_text(aes(x =ReportDate[leftpt], y = 87, label = "Number of Daily Tests"), size = 3.5)
gsurv <- gsurv + ggtitle("Surveillance")
gsurv <- gsurv + scale_x_date(date_labels = "%b %d", date_breaks = "5 days")
gsurv

####################
### Total cases plot: ggplot
####################
shift = 700
maxc <- max(state_data$Cases) - shift
gtotal <- ggplot(aes(x = ReportDate, y = Cases), data = state_data) + geom_bar(stat = "identity", col = "black") + xlab("Date of Report") + ylab("Total Confirmed Cases") + ggtitle("Total Confirmed Cases")
gtotal <- gtotal + stat_smooth(aes(x = ReportDate, y = Cases), col = "red", se = F, data = state_data)
gtotal <- gtotal + scale_x_date(date_labels = "%b %d", date_breaks = "5 days")  + geom_text(aes(x = report_dates[7],y = maxc+shift/2 , label = "@wellerstats"), col = "deepskyblue2", size = 3.0)
gtotal
#+ annotation_custom(twg, xmin = report_dates[1], xmax = report_dates[3], ymin = maxc, ymax = maxc + shift)

####################
### Daily cases plot: ggplot
####################
gdaily <- ggplot(aes(x = ReportDate, y = daily_cases), data = state_data) + geom_bar(stat = "identity", col = "black") + xlab("Date of Report") + ylab("Daily Confirmed Cases") + ggtitle("Daily Confirmed Cases") + stat_smooth(aes(x = ReportDate, y = daily_cases), col = "red", se = F, data = state_data) + scale_x_date(date_labels = "%b %d", date_breaks = "5 days")
gdaily


####################
### Trajectory plot: ggplot
####################
nday = 7
dailycases_xday = xday.sum(state_data$daily_cases, nday)
plot(log(state_data$Cases[-(1:nday-1)], base = 10), log(dailycases_xday, base = 10), type = "b", xlab = "Total Confirmed Cases) ", ylab = "New Confirmed Cases in Last Week")
#create new data set
growthdata <- data.frame("ReportDate" = state_data$ReportDate[-(1:nday-1)], "Cases" = state_data$Cases[-(1:nday-1)],  "lastweektotal" = dailycases_xday)
ggrow <- ggplot(data = growthdata, aes(x = Cases, y = lastweektotal)) + geom_line(col = "red", size = 1) + geom_point(col = "red", size = 2) + xlab("Total Confirmed Cases (log)") + ylab("New Confimed Cases in Last Week (log)") + scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") + ggtitle("Case Trajectory")
ggrow 

#####################
#Stitch figures together, save plot
#####################

today_long <- format(Sys.time(), form = "%b %d, %Y")

gall <- grid.arrange(gtotal, gdaily, ggrow, gsurv, nrow = 2, top = textGrob(paste("Colorado Covid-19 Reporting: ", today_long), gp = gpar(fontsize = 20, font = 3)) ) 

today <- format(Sys.time(), form = "%b%d")

ggsave(file = paste0("Figures/co_data_",today,".jpeg"), gall, width = 10, height = 8 )
ggsave(file = paste0("Figures/co_data_",today,".pdf"), gall, width = 10, height = 8 )


#################################################################

#par("usr")
#rasterImage(tw, 2.8, 3.2, 3 , 3.4, xpd = T)
#mtext("hi", side = 2, line = 3)
#gsurv + annotation_custom(twg, xmin = report_dates[1], xmax = report_dates[2], ymin = 90, ymax = 100)

#######################
####Surveillance plot: base R
######################
today <- format(Sys.time(), form = "%b%d")

pdf(file = paste0("Figures/co_positive_",today,".pdf"), width = 10, height = 8 )

#jpeg(file = paste0("Figures/co_positive_",today,".jpeg"))
plot(report_dates, pct_pos, type = "n", ylim = c(0, 100), xlab = "", ylab = "", xaxt = "n", xlim = c(min(report_dates) - 1, max(report_dates)+1))
datelabs = format(report_dates, format = "%b %d")
#datelabs[1] = paste("pre", datelabs[1])
axis(side = 1, at = report_dates, labels =  datelabs)
mtext("(and previous)", side = 1, line = 1.8, at = report_dates[1], cex = 0.85)
mtext("Date of Report", side = 1, line = 2.75, cex = 1.5)
mtext("Percent Positive Tests", side = 2, line = 2.5, cex = 1.5)
mtext("Colorado: COVID-19 Testing", side = 3, line = 1.5, cex = 2)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray")
abline(h = seq(0, 100, by = 10), col = "white", lwd = 1.25)
abline(v = seq(firstd - 1 , lastd + 1, by = 1), col = "white", lwd = 1.25)
lines(report_dates, pct_pos*100, col = "red", lwd = 2, type = "b")
points(report_dates, pct_pos*100, col = "red", pch = 16)
text(report_dates+0.35, 63, as.character(daily_tests), srt = 45)
text(report_dates[10] , 75, "Number of Tests", cex = 1.25, font = 2)
#lines(report_dates, daily_tests/max(daily_tests))
rect(max(report_dates - 6.5),85,par("usr")[2],par("usr")[4],col = "white")
text(max(report_dates - 1.2), 99, paste("Total Tests: ", sum(daily_tests)) )
text(max(report_dates - 2.35), 95, paste("Total Positive Tests: ", sum(daily_cases)) )
text(max(report_dates - 2), 91, paste0("Total Pct Positive: ", round(100*sum(daily_cases)/sum(daily_tests),1) ,"%") )
rasterImage(tw, min(report_dates)-2, 105, min(report_dates)-1 , 110, xpd = T)
mtext("@wellerstats", at = min(report_dates)+1.5,  side = 3, line = 0.4, col = "deepskyblue2")

dev.off()

####################
### Total cases plot: base R
####################
plot(state_data$ReportDate, state_data$Cases, type = "n", xlab = "", ylab = "", xaxt = "n", xlim = c(min(report_dates) - 1, max(report_dates)+1), ylim = c(0, max(state_data$Cases)+1200))
datelabs = format(report_dates, format = "%b %d")
#datelabs[1] = paste("pre", datelabs[1])
axis(side = 1, at = report_dates, labels =  datelabs)
mtext("(and previous)", side = 1, line = 1.8, at = report_dates[1], cex = 0.85)
mtext("Date of Report", side = 1, line = 2.75, cex = 1.5)
mtext("Total Cases", side = 2, line = 2.5, cex = 1.5)
mtext("Total Cases", side = 3, line = 1.5, cex = 2)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray")
abline(h = seq(0, 20000, by = 500), col = "white", lwd = 1.25)
abline(v = seq(firstd - 1 , lastd + 1, by = 1), col = "white", lwd = 1.25)
lines(state_data$ReportDate, state_data$Cases, col = "red", lwd = 2, type = "b")
#lines(state_data$ReportDate, state_data$PeopleTested)
points(state_data$ReportDate, state_data$Cases, col = "red", pch = 16)

