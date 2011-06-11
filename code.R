require(ggplot2)

# utility functions
summarize.contributions <- function(x) {
  xo <- x[order(x$contribution_date),]
  dtx <- as.integer(diff(x$contribution_date))
 
  return(data.frame(
		first.contribution=xo$contribution_date[1], 
		num.contributions = nrow(xo),
		dt1=dtx[1],
		dt2=dtx[2],
		dt3=dtx[3],
		am1=xo$amount[1],
		am2=xo$amount[2],
		am3=xo$amount[3],
		total.value=sum(x$amount)
	))
}

formatBack <- function(x) paste(round(10^x, 2), "$", sep=' ') 


# latest smith for congress data as of this writing is March 23 2011.
cd <- read.csv("smithforcongress-03232011.csv")
#subset the data to just the 2010 cycle
cd0 <- cd[cd$cycle == 2010,]
# clean up a date variable, and drop amounts < $1. 
cd$contribution_date <- as.Date(cd$contribution_date,format="%m/%d/%Y")
cd0 <- cd0[-which(cd0$amount < 1),]

# summarize the contributions
cd0s <- ddply(cd0, "personid", summarize.contributions)

# plot giving levels
qplot(total.value,data=cd0s,geom="histogram",binwidth=50)
nrow(cd0[cd0$amount<250,]) / nrow(cd0)
summary(cd0s$total.value)

# plot giving frequency
qplot(num.contributions,data=cd0s,geom="histogram",binwidth=1)
table(cd0s$num.contributions)

# plot giving levels by giving frequency
qplot(factor(num.contributions),log10(total.value),data=cd0s[cd0s$num.contributions < 8,],geom="boxplot",ylab="Total Value (log)",
xlab="Giving Frequency",main="Giving Levels by Giving Frequency, Smith for Congress 2010") + 
scale_y_continuous(formatter=formatBack)
# same data, but in table format 
ddply(cd0s,"num.contributions",function(x) { data.frame(total=sum(x$total.value),n=nrow(x), 
	min=min(x$total.value),mean=mean(x$total.value), median=median(x$total.value),std=sd(x$total.value),max=max(x$total.value))})

# maxed out donors
# how many individuals gave the max for one election
nrow(cd0s[cd0s$total.value == 2400,])
nrow(cd0s[cd0s$total.value == 4800,])

