EPI_2010data <- read.csv("/Users/cindykwok/Desktop/CSCI 4600 Data Analytics /labs and assignments/2010EPI_data.csv")
EPI_2010data
View(EPI_2010data)
names(EPI_2010data) <- as.matrix(EPI_2010data[1, ])
EPI_2010data <- EPI_2010data[-1, ]
EPI_2010data[] <- lapply(EPI_2010data, function(x) type.convert(as.character(x)))
EPI_2010data
View(EPI_2010data)
attach(EPI_2010data)
names(EPI_2010data)
dim(EPI_2010data)
head(EPI_2010data)
str(EPI_2010data)
fix(EPI_2010data)
EPI
EPI_2010data$EPI
View(EPI_2010data)
tf <- is.na(EPI)
E <- EPI[!tf]
summary(EPI)
fivenum(EPI, na.rm=TRUE)

#plotting
stem(EPI)
hist(EPI)
hist(EPI, seq(30.,95.,1.0), prob = TRUE)
lines(density(EPI,na.rm=TRUE,bw=1.))
rug(EPI)
help(stem)

#exercise fitting a dist beyond hist
#cumulative density func
plot(ecdf(EPI),do.points=FALSE,verticals = TRUE) 

#quantile-quantile
par(pty="s") 
qqnorm(EPI)
qqline(EPI)

#Q-Q plot against generating dist
x<-seq(30,95,1) 
qqplot(qt(ppoints(250),df = 5),x,xlab="Q-Q plot tdsn")
qqline(x)

#doing same stuff with 2 other vars [DALY]
tfdaly <- is.na(DALY)
D <- EPI[!tf]
summary(DALY)
fivenum(DALY, na.rm=TRUE)
stem(DALY)
hist(DALY)
hist(DALY, seq(0,100.,2.0), prob = TRUE)
lines(density(DALY,na.rm=TRUE,bw=1.))
rug(DALY)

#doing same stuff with 2 other vars [WATER_H]
tfwater <- is.na(WATER_H)
W <- WATER_H[!tf]
summary(WATER_H)
fivenum(WATER_H, na.rm=TRUE)
stem(WATER_H)
hist(WATER_H)
hist(WATER_H, seq(0,100.,5.0), prob = TRUE)
lines(density(WATER_H,na.rm=TRUE,bw=1.))
rug(WATER_H)
