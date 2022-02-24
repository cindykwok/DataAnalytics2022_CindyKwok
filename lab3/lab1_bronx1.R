library(gdata) 
#faster xls reader but requires perl!
#bronx1<-read.xls(file.choose(),pattern="BOROUGH",stringsAsFactors=FALSE,sheet=1,perl="<SOMEWHERE>/perl/bin/perl.exe") 
#bronx1<-bronx1[which(bronx1$GROSS.SQUARE.FEET!="0" & bronx1$LAND.SQUARE.FEET!="0" & bronx1$SALE.PRICE!="$0"),]

#alternate
#library("xlsx", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
install.packages("xlsx")
library(xlsx)
bronx1<-read.xlsx("/Users/cindykwok/Desktop/CSCI 4600 Data Analytics /lab3/rollingsales_bronx.xls",pattern="BOROUGH",stringsAsFactors=FALSE,sheetIndex=1,startRow=5,header=TRUE)
View(bronx1)

attach(bronx1) # If you choose to attach, leave out the "data=." in lm regression
SALE.PRICE<-sub("\\$","",SALE.PRICE) 
SALE.PRICE<-as.numeric(gsub(",","", SALE.PRICE)) 
GROSS.SQUARE.FEET<-as.numeric(gsub(",","", GROSS.SQUARE.FEET)) 
LAND.SQUARE.FEET<-as.numeric(gsub(",","", LAND.SQUARE.FEET)) 
plot(log(GROSS.SQUARE.FEET), log(SALE.PRICE)) 

length(SALE.PRICE)
length(GROSS.SQUARE.FEET)

b1 <- bronx1
#View(b1)

b2 <- b1[!(b1$SALE.PRICE== 0  | is.na(log(b1$SALE.PRICE)) | is.nan(log(b1$SALE.PRICE))),]
#View(b2)
b3 <- b2[!(b2$GROSS.SQUARE.FEET== 0  | is.na(log(b2$GROSS.SQUARE.FEET)) | is.nan(log(b2$GROSS.SQUARE.FEET))),]

#View(b3)

length(b3$SALE.PRICE)
length(b3$GROSS.SQUARE.FEET)

m1<-lm(data = b3, log(SALE.PRICE)~log(GROSS.SQUARE.FEET))
summary(m1)
abline(m1,col="red",lwd=2)
plot(resid(m1))

# Model 2
b4 <- b3[!(b3$LAND.SQUARE.FEET== 0  | is.na(log(b3$LAND.SQUARE.FEET)) | is.nan(log(b3$LAND.SQUARE.FEET))),]

m2<-lm(data = b4, log(SALE.PRICE)~log(GROSS.SQUARE.FEET)+log(LAND.SQUARE.FEET)+factor(NEIGHBORHOOD))
summary(m2)
plot(resid(m2))
# Suppress intercept - using "0+ ..."
m2a<-lm(data = b4, log(SALE.PRICE)~0+log(GROSS.SQUARE.FEET)+log(LAND.SQUARE.FEET)+factor(NEIGHBORHOOD))
summary(m2a)
plot(resid(m2a))

# Model 3
m3<-lm(data = b4, log(SALE.PRICE)~0+log(GROSS.SQUARE.FEET)+log(LAND.SQUARE.FEET)+factor(NEIGHBORHOOD)+factor(BUILDING.CLASS.CATEGORY))
summary(m3)
plot(resid(m3))

# Model 4
m4<-lm(data = b4, log(SALE.PRICE)~0+log(GROSS.SQUARE.FEET)+log(LAND.SQUARE.FEET)+factor(NEIGHBORHOOD)*factor(BUILDING.CLASS.CATEGORY))
summary(m4)
plot(resid(m4))

