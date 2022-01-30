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
D <- DALY[!tfdaly]
summary(DALY)
fivenum(DALY, na.rm=TRUE)
stem(DALY)
hist(DALY)
hist(DALY, seq(0,100.,2.0), prob = TRUE)
lines(density(DALY,na.rm=TRUE,bw=1.))
rug(DALY)
plot(ecdf(DALY),do.points=FALSE,verticals = TRUE) 

#doing same stuff with 2 other vars [WATER_H]
tfwater <- is.na(WATER_H)
W <- WATER_H[!tfwater]
summary(WATER_H)
fivenum(WATER_H, na.rm=TRUE)
stem(WATER_H)
hist(WATER_H)
hist(WATER_H, seq(0,100.,5.0), prob = TRUE)
lines(density(WATER_H,na.rm=TRUE,bw=1.))
rug(WATER_H)
plot(ecdf(WATER_H),do.points=FALSE,verticals = TRUE) 

#comparing distributions
boxplot(EPI,DALY,WATER_H)

#more filtering for NA
tfenvhealth <- is.na(ENVHEALTH)
EH <- ENVHEALTH[!tfenvhealth]

tfecosystem <- is.na(ECOSYSTEM)
EC <- ECOSYSTEM[!tfecosystem]

tfairh <- is.na(AIR_H)
AH <- AIR_H[!tfairh]

tfaire <- is.na(AIR_E)
AE <- ENVHEALTH[!tfaire]

tfwatere <- is.na(WATER_E)
WE <- WATER_E[!tfwatere]

boxplot(EPI, DALY, WATER_H,WATER_E, ENVHEALTH,ECOSYSTEM,AIR_H,AIR_E)

help(distributions)

#filtering conditional 
EPILand <- EPI[!Landlock]
ELand <- EPILand[!is.na(EPILand)]
hist(ELand)
hist(ELand, seq(30., 95., 1.0), prob=TRUE)

EPINoWat <- EPI[!No_surface_water]
ENW <- EPINoWat[!is.na(EPINoWat)]
hist(ENW)
hist(ENW, seq(30., 95., 1.0), prob=TRUE)

EPIDesert <- EPI[!Desert]
EDesert <- EPIDesert[!is.na(EPIDesert)]
hist(EDesert)
hist(EDesert, seq(30., 95., 1.0), prob=TRUE)

EPIHighDensity <- EPI[!High_Population_Density]
EHighDen <- EPIHighDensity[!is.na(EPIHighDensity)]
hist(EHighDen)
hist(EHighDen, seq(30., 95., 1.0), prob=TRUE)

EPI_South_Asia <- EPI[EPI_regions == "South Asia"]
ESA <- EPI_South_Asia[!is.na(EPI_South_Asia)]

#same with GPW3_GRUMP data 
GPW3_Sum <- read.csv("/Users/cindykwok/Desktop/CSCI 4600 Data Analytics /labs and assignments/GPW3_GRUMP_SummaryInformation_2010.csv")
View(GPW3_Sum)

str(GPW3_Sum)
names(GPW3_Sum)
attach(GPW3_Sum)
tfpop <- is.na(PopulationPerUnit)
P <- PopulationPerUnit[!tfpop]
summary(PopulationPerUnit)
summary(P) #using filtered out ver w/o NA
fivenum(P)
fivenum(PopulationPerUnit,na.rm=TRUE)
stem(P)
hist(P)
hist(P, seq(0., 3000., 10.0), prob=TRUE)
lines(density(P,bw=1.)) 
rug(P)
plot(ecdf(P), do.points=FALSE, verticals=TRUE)
qqnorm(P)
qqline(P)

tfnum <- is.na(NumUnits)
NU <- NumUnits[!tfnum]

boxplot(PopulationPerUnit,NumUnits)

#conditional filterin GPW3
GPW3Res <- PopulationPerUnit[!Resolution]
GRes <- GPW3Res[!is.na(GPW3Res)]
hist(GRes)
