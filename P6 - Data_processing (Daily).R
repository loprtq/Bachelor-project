# Basics ------------------------------------------------------------------
options(warn = -1)
library(TSA); library(astsa); library(tseries)
rm(list = ls()) # Removes every variable
graphics.off()  # Clears all plots

# R matrix to Latex bmatrix
Latex <- function(MR, digits){
  rows = apply(round(MR, digits), MARGIN=1, paste, collapse = " & ")
  matrix_string = paste(rows, collapse = "\\\\")
  return(cat(paste("\\begin{bmatrix}", matrix_string, "\\end{bmatrix}")))
}


# Load data ---------------------------------------------------------------
# Prices
Prices2013 = read.csv("Daily/Prices2013.csv"); Prices2013 = Prices2013[-c(1:3),-c(2:7,10:ncol(Prices2013))]
Prices2014 = read.csv("Daily/Prices2014.csv"); Prices2014 = Prices2014[-c(1,2),-c(2:7,10:ncol(Prices2014))]
Prices2015 = read.csv("Daily/Prices2015.csv"); Prices2015 = Prices2015[-c(1,2),-c(2:7,10:ncol(Prices2015))]
Prices2016 = read.csv("Daily/Prices2016.csv"); Prices2016 = Prices2016[-c(1,2),-c(2:7,10:ncol(Prices2016))]
Prices2017 = read.csv("Daily/Prices2017.csv"); Prices2017 = Prices2017[-c(1,2),-c(2:7,10:ncol(Prices2017))]
Prices2018 = read.csv("Daily/Prices2018.csv"); Prices2018 = Prices2018[-c(1,2),-c(2:7,10:ncol(Prices2018))]
Prices2019 = read.csv("Daily/Prices2019.csv"); Prices2019 = Prices2019[-c(1,2),-c(2:7,10:ncol(Prices2019))]
Prices2020 = read.csv("Daily/Prices2020.csv"); Prices2020 = Prices2020[-c(1,2),-c(2:7,10:ncol(Prices2020))]
Prices2021 = read.csv("Daily/Prices2021.csv"); Prices2021 = Prices2021[-c(1,2),-c(2:7,10:ncol(Prices2021))]
Prices2022 = read.csv("Daily/Prices2022.csv"); Prices2022 = Prices2022[-c(1,2),-c(2:7,10:ncol(Prices2022))]
Prices2022[nrow(Prices2022),2:3] = c("569,83","597,67"); P2022 = nrow(Prices2020)+nrow(Prices2021)+nrow(Prices2022)
Prices     = rbind(Prices2013,Prices2014,Prices2015,Prices2016,Prices2017,Prices2018,Prices2019,Prices2020,Prices2021,Prices2022)
rm(Prices2013,Prices2014,Prices2015,Prices2016,Prices2017,Prices2018,Prices2019,Prices2020,Prices2021,Prices2022)
Prices[,2] = as.numeric(gsub(",",".", gsub("\\.","", Prices[,2]))) # Format DK1

# Wind Production
Wind2013 = read.csv("Daily/Wind2013.csv"); Wind2013 = Wind2013[-c(1,2),]
Wind2014 = read.csv("Daily/Wind2014.csv"); Wind2014 = Wind2014[-c(1,2),]
Wind2015 = read.csv("Daily/Wind2015.csv"); Wind2015 = Wind2015[-c(1,2),]
Wind2016 = read.csv("Daily/Wind2016.csv"); Wind2016 = Wind2016[-c(1,2),]
Wind2017 = read.csv("Daily/Wind2017.csv"); Wind2017 = Wind2017[-c(1,2),]
Wind2018 = read.csv("Daily/Wind2018.csv"); Wind2018 = Wind2018[-c(1,2),]
Wind2019 = read.csv("Daily/Wind2019.csv"); Wind2019 = Wind2019[-c(1,2),]
Wind2020 = read.csv("Daily/Wind2020.csv"); Wind2020 = Wind2020[-c(1,2),]
Wind2021 = read.csv("Daily/Wind2021.csv"); Wind2021 = Wind2021[-c(1,2),]
Wind2022 = read.csv("Daily/Wind2022.csv"); Wind2022 = Wind2022[-c(1,2),]
Wind2022[nrow(Wind2022),2:3] = c(49272,28656)
Wind     = rbind(Wind2013,Wind2014,Wind2015,Wind2016,Wind2017,Wind2018,Wind2019,Wind2020,Wind2021,Wind2022)
rm(Wind2013,Wind2014,Wind2015,Wind2016,Wind2017,Wind2018,Wind2019,Wind2020,Wind2021,Wind2022)
Wind[,2] = as.numeric(gsub(",",".", gsub("\\.","", Wind[,2]))) # Format DK1

# Production
Prod2013 = read.csv("Daily/Production2013.csv"); Prod2013 = Prod2013[-c(1,2),-c(5)]
Prod2014 = read.csv("Daily/Production2014.csv"); Prod2014 = Prod2014[-c(1,2),-c(5)]
Prod2015 = read.csv("Daily/Production2015.csv"); Prod2015 = Prod2015[-c(1,2),-c(5)]
Prod2016 = read.csv("Daily/Production2016.csv"); Prod2016 = Prod2016[-c(1,2),-c(5)]
Prod2017 = read.csv("Daily/Production2017.csv"); Prod2017 = Prod2017[-c(1,2),-c(5)]
Prod2018 = read.csv("Daily/Production2018.csv"); Prod2018 = Prod2018[-c(1,2),-c(5)]
Prod2019 = read.csv("Daily/Production2019.csv"); Prod2019 = Prod2019[-c(1,2),-c(5)]
Prod2020 = read.csv("Daily/Production2020.csv"); Prod2020 = Prod2020[-c(1,2),-c(5)]
Prod2021 = read.csv("Daily/Production2021.csv"); Prod2021 = Prod2021[-c(1,2),-c(5)]
Prod2022 = read.csv("Daily/Production2022.csv"); Prod2022 = Prod2022[-c(1,2),-c(5)]
Prod2022[nrow(Prod2022),2:3] = c(61528,39234)
Prod     = rbind(Prod2013,Prod2014,Prod2015,Prod2016,Prod2017,Prod2018,Prod2019,Prod2020,Prod2021,Prod2022)
rm(Prod2013,Prod2014,Prod2015,Prod2016,Prod2017,Prod2018,Prod2019,Prod2020,Prod2021,Prod2022)
Prod[,2] = as.numeric(gsub(",",".", gsub("\\.","", Prod[,2]))) # Format DK1

# Consumption
Cons2013 = read.csv("Daily/Consumption2013.csv"); Cons2013 = Cons2013[-c(1,2),-5]
Cons2014 = read.csv("Daily/Consumption2014.csv"); Cons2014 = Cons2014[-c(1,2),-5]; colnames(Cons2014) = colnames(Cons2013)
Cons2015 = read.csv("Daily/Consumption2015.csv"); Cons2015 = Cons2015[-c(1,2),-5]; colnames(Cons2015) = colnames(Cons2013)
Cons2016 = read.csv("Daily/Consumption2016.csv"); Cons2016 = Cons2016[-c(1,2),-5]
Cons2017 = read.csv("Daily/Consumption2017.csv"); Cons2017 = Cons2017[-c(1,2),-5]
Cons2018 = read.csv("Daily/Consumption2018.csv"); Cons2018 = Cons2018[-c(1,2),-5]
Cons2019 = read.csv("Daily/Consumption2019.csv"); Cons2019 = Cons2019[-c(1,2),-5]
Cons2020 = read.csv("Daily/Consumption2020.csv"); Cons2020 = Cons2020[-c(1,2),-5]
Cons2021 = read.csv("Daily/Consumption2021.csv"); Cons2021 = Cons2021[-c(1,2),-5]
Cons2022 = read.csv("Daily/Consumption2022.csv"); Cons2022 = Cons2022[-c(1,2),-5]
Cons2022[nrow(Cons2022),2:4] = c(67667,40060,107726)
Cons     = rbind(Cons2013,Cons2014,Cons2015,Cons2016,Cons2017,Cons2018,Cons2019,Cons2020,Cons2021,Cons2022)
rm(Cons2013,Cons2014,Cons2015,Cons2016,Cons2017,Cons2018,Cons2019,Cons2020,Cons2021,Cons2022)
Cons[,2] = as.numeric(gsub(",",".", gsub("\\.","", Cons[,2]))) # Format DK1


# Data processing ---------------------------------------------------------
# Plot data
png(file.path(getwd(),"Figures",paste("TS-Prices.png",sep="")),
    width=1600,height=800)
plot(Prices[,2],ylab="DKK/MWh",xlab="Days",main="",type="l",
     cex.lab=1.5,cex.axis=1.6,cex.sub=2.5, lwd=1); dev.off()
png(file.path(getwd(),"Figures",paste("TS-Wind_Production.png",sep="")),
    width=1600,height=800)
plot(Wind[,2],ylab="MWh",xlab="Days",main="",type="l",
        cex.lab=1.5,cex.axis=1.6,cex.sub=2.5, lwd=1); dev.off()
png(file.path(getwd(),"Figures",paste("TS-Production.png",sep="")),
    width=1600,height=800)
plot(Prod[,2],ylab="MWh",xlab="Days",main="",type="l",
        cex.lab=1.5,cex.axis=1.6,cex.sub=2.5, lwd=1); dev.off()
png(file.path(getwd(),"Figures",paste("TS-Consumption.png",sep="")),
    width=1600,height=800)
plot(Cons[,2],ylab="MWh",xlab="Days",main="",type="l",
        cex.lab=1.5,cex.axis=1.6,cex.sub=2.5, lwd=1); dev.off()

# Remove outliers
Prices[158,2] = mean(c(Prices[157,2],Prices[159,2]))

# Transform data
Prices[,2] = log(Prices[,2])

# Remove NA
Prices[1456,2] = Prices[1457,2]
for (i in which(is.na(Prices[,2]))){
  Prices[i,2]= mean(c(Prices[i-1,2],Prices[i+1,2]))
}; rm(i)

# Combine data
Data   = data.frame(Prices[,2],Wind[,2],Prod[,2],Cons[,2])
rm(Prices,Wind,Prod,Cons)
colnames(Data) = c("Prices","Wind Production","Production","Consumption") 

# Delete pandemic data
Data = Data[-c((nrow(Data)-P2022):nrow(Data)),]; rm(P2022)

# Convert to yearly time series
tsdata = ts(Data, start=c(2013,1), frequency=365.25); rm(Data)


# De-trend and de-seasonalize ---------------------------------------------
sea = matrix(nrow=nrow(tsdata), ncol=ncol(tsdata))
tre = matrix(nrow=nrow(tsdata), ncol=ncol(tsdata))
for (i in 1:ncol(tsdata)){
  png(filename=file.path(getwd(),"Figures",paste("Decomposition",i,
      ".png",sep="")), width=1500, height=900)
  detsdata = decompose(tsdata[,i]); plot(detsdata,cex.axis=1.5,cex.lab=1.5)
  sea[,i] = detsdata$seasonal
  tre[,i] = detsdata$trend
  dev.off()
}; rm(i, detsdata)

# Seasonal components
t = 1:length(sea[,1])
s1.1 = cos(2^1*pi*1/365.25*t); s1.2 = sin(2^1*pi*1/365.25*t)
s2.1 = cos(2^2*pi*1/365.25*t); s2.2 = sin(2^2*pi*1/365.25*t)
s3.1 = cos(2^3*pi*1/365.25*t); s3.2 = sin(2^3*pi*1/365.25*t)
s4.1 = cos(2^4*pi*1/365.25*t); s4.2 = sin(2^4*pi*1/365.25*t)

# Season models
Sea_Price = lm(sea[,1] ~ s1.1+s2.1+s3.1+s4.1+s1.2+s2.2+s3.2+s4.2)
Sea_WindP = lm(sea[,2] ~ s1.1+s2.1+s3.1+s4.1+s1.2+s2.2+s3.2+s4.2)
Sea_Produ = lm(sea[,3] ~ s1.1+s2.1+s3.1+s4.1+s1.2+s2.2+s3.2+s4.2)
Sea_Consu = lm(sea[,4] ~ s1.1+s2.1+s3.1+s4.1+s1.2+s2.2+s3.2+s4.2)
rm(sea, tre)

# Trend models
Tre_Price = lm(tsdata[,1]-predict.lm(Sea_Price) ~ t+s1.1+s2.1*t+s3.1+s4.1+s1.2*t+s2.2+s3.2+s4.2*t)
Tre_WindP = lm(tsdata[,2]-predict.lm(Sea_WindP) ~ t+s1.1+s2.1*t+s3.1+s4.1+s1.2*t+s2.2+s3.2+s4.2*t)
Tre_Produ = lm(tsdata[,3]-predict.lm(Sea_Produ) ~ t+s1.1+s2.1*t+s3.1+s4.1+s1.2*t+s2.2+s3.2+s4.2*t)
Tre_Consu = lm(tsdata[,4]-predict.lm(Sea_Consu) ~ t+s1.1+s2.1*t+s3.1+s4.1+s1.2*t+s2.2+s3.2+s4.2*t)
rm(t,s1.1,s1.2,s2.1,s2.2,s3.1,s3.2,s4.1,s4.2)

# Transform to stationary data
Statdata = matrix(nrow=nrow(tsdata), ncol=ncol(tsdata))
Statdata[,1] = tsdata[,1]-fitted(Sea_Price)-fitted(Tre_Price)
Statdata[,2] = tsdata[,2]-fitted(Sea_WindP)-fitted(Tre_WindP)
Statdata[,3] = tsdata[,3]-fitted(Sea_Produ)-fitted(Tre_Produ)
Statdata[,4] = tsdata[,4]-fitted(Sea_Consu)-fitted(Tre_Consu)
Statdata = ts(Statdata, start=c(2013,1), frequency=365.25)
plot(Statdata)

# Plot stationary data ACF
par(mfrow=c(ncol(Statdata),1))
for (i in 1:ncol(Statdata)){
  png(filename=file.path(getwd(),"Figures",paste("ACF",i,".png", sep=""))
      , width=1750, height=1000)
  acf(Statdata[,i],xlab="Years",ylab="",main="",lag.max=365/2,
      cex.lab=2.5,cex.axis=2.5,cex.sub=2.5)
  dev.off()
}; rm(i)

# Test if stationary with ADF (p<0.05)
for (i in 1:ncol(tsdata)){
  print(adf.test(Statdata[,i]))
}; rm(i)
