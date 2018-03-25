
#loading necessary packages

library('ggplot2')
library('ggmap')
qmap(location='melbourne')
library(corrplot)

data=read.csv("~/Aggregated-TAC-Hospitalisation-Stats.csv")

summary(data)

#making table of unique LGA

data_table=data.frame(table(data$LGA))
data_table$City=paste(data_table$Var1,"Australia")


#now to get total number of crashes per LGA, assuming that summing the three genders gives total number


which(data$LGA=="Alpine")
sum(data$male[which(data$LGA=="Alpine")])


#which(data$LGA==data_table$Var1[1])

for(i in seq(1:length(data_table$Var1))){
        
        data_table$Freq[i]=
            sum(data$male[which(data$LGA==data_table$Var1[i])]) +
            sum(data$female[which(data$LGA==data_table$Var1[i])]) +
            sum(data$unknownGender[which(data$LGA==data_table$Var1[i])])
    
        
}

data_table$Freq


#getting the latitude and longitude of LGA

latlon=geocode(data_table$City)

#the google api timed out on a few, so go back and get the ones it missed

length(latlon$lon)
for(i in seq(1:length(data_table$City))){
    if(is.na(latlon$lon[i]) == 'TRUE')
        {
            latlon2=geocode(data_table$City[i])
            latlon$lon[i]=latlon2$lon
            latlon$lat[i]=latlon2$lat
        }
    }


is.na(latlon$lon)

#check to make sure the I got the correct lat/lon for one of the LGAs that I misssed initially.

latlon$lon[which(data_table$City=='Wangaratta Australia')]
geocode('Wangaratta Australia')

data_table$lon=latlon$lon
data_table$lat=latlon$lat



map <- get_map(location = c(lon = mean(data_table$lon), lat = mean(data_table$lat)), zoom = 7
               , source = "google")

ggmap(map) + geom_point(data=data_table,size=data_table$Freq/50,color="red",alpha=0.3) +
#annotate("text", x = data_table$lon[which(data_table$Freq!=1)], y = data_table$lat[which(data_table$Freq!=1)], 
 #        label = data_table$Freq[which(data_table$Freq!=1)]) + 
xlab("") + ylab("")


#alright, Inner Melbourne seems to be the worse off here. The rural locations have far fewer crashes
#Let's find out what correlates with what. 
#It would be helpful to sum up values for individual locations.

newdata <- data[c(-1:-3)]
newdata$City=0
M <- cor(newdata)
corrplot(M, method = "circle")

#this is unhelpful on many levels. It has too much data, everything seems to be correlated.

cor(newdata) #this gets the linear correlation coefficients between all the variables. Its erasing the geo data. We can see basic trends this way. 
#Women are younger then men and are more likely to be Motorcyclists.
#Crashes in Rural areas appear to be fairly random and infrequent

library("Hmisc")
res2 <- rcorr(as.matrix(newdata))
res2
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
    )
}

#This is going to be more easy to digest. Sorted this way, we see the lowest linear correlations. 
#Accidents are much less likely to happen to women in rural locations.
#The p value tells us how much we should actually care about the correlations.
#I'm trying to find the most correlated variables here.

a=flattenCorrMatrix(res2$r, res2$P)

a=as.matrix(a)
#a[order(a[,3]),]

which(a[,4] > 0.9)
a[7,]
b=data.frame(a)
max(which(is.na(as.numeric(b$p))!=TRUE))
a[903,]

b$p=as.numeric(as.character(b$p))
b$cor=as.numeric(as.character(b$cor))

summary(b)

b[order(-b$cor),]
sig_data=b[which(b$p > 0.5),]
high_cor=b[which(b$cor > 0.95),]

sig_data[order(sig_data$cor),]

high_cor[order(high_cor$cor),]

newdata=data[c("userBicyclist","userDriver","userMotorcyclist","userPedestrian","hr0000to0559","hr0600to1159","hr1200to1759","hr1800to2359")]

M <- cor(newdata)
corrplot.mixed(M, lower.col = "black", number.cex = .7)


res2 <- rcorr(as.matrix(newdata))


res2 <- rcorr(as.matrix(newdata))
res2

#Ok, time to get serious. Let's answer this question:
#What variables correlate most strongly with crashes in melbourne?

#cleaning the data, making dates number and assigning a numeric value to each LGA in case we need it later.
#Also finding the variables that correlate most strongly with the number of accidents in Melbourne so we can narrow
#down the variables for the fit.
melbourne_data=data[which(data$locMelbourne > 0),]
melbourne_data$total_accidents=melbourne_data$locMelbourne
melbourne_data$dateFrom=as.numeric(melbourne_data$dateFrom)
melbourne_data$dateTo=as.numeric(melbourne_data$dateTo)
lga_code=c(1:length(unique(melbourne_data$LGA)))
lga_unique=unique(melbourne_data$LGA)
melbourne_data$LGA=as.numeric(melbourne_data$LGA)
res2 <- rcorr(as.matrix(melbourne_data))
a=flattenCorrMatrix(res2$r, res2$P)
b=data.frame(a)
b=b[which(b$column=='total_accidents'),]
b[order(-b$cor),]

#We want something useful. Let's use the hospital stay for a proxy of severity of accident. Let's see what factors contribut
#to the most severe accidents in Melbourne

melbourne_data=data[which(data$locMelbourne > 0),]
melbourne_data$total_accidents=melbourne_data$locMelbourne
melbourne_data$dateFrom=as.numeric(melbourne_data$dateFrom)
melbourne_data$dateTo=as.numeric(melbourne_data$dateTo)
lga_code=c(1:length(unique(melbourne_data$LGA)))
lga_unique=unique(melbourne_data$LGA)
melbourne_data$LGA=as.numeric(melbourne_data$LGA)
res2 <- rcorr(as.matrix(melbourne_data))
a=flattenCorrMatrix(res2$r, res2$P)
b=data.frame(a)
b=b[which(b$row=='stayGreater14'),]
b[order(-b$cor),]
M<-cor(melbourne_data)
corrplot.mixed(M, lower.col = "black", number.cex = .7)

#Definitely during rush hour going the wrong way. This is a definitive course of action: Melbourne needs to improve
#traffic information so that people don't go down the wrong road.

ggplot(data=subset(b,cor>0.65),aes(x=reorder(column,cor),cor ,fill=cor)) + 
geom_col() + coord_flip() + ylab("Linear Correlation Coefficient") +
xlab("")


