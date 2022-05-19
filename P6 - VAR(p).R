# Basics ------------------------------------------------------------------
library(TSA); library(astsa); library(tseries); library(forecast)
library(vars); library(Hmisc); library(CADFtest)
rm(list = ls()) # Removes every variable
graphics.off()  # Clears all plots


# Load and see data -------------------------------------------------------
source("P6 - Data_processing (Daily).R")


# Modeling ----------------------------------------------------------------
# Data correlation
Latex(rcorr(as.matrix(Statdata), type="pearson")$r, digits=2)

# Order criteria (AIC, HQ, SC, FPE)
pmax = 25
AIC = matrix(nrow=pmax, ncol=2)
for (i in 1:pmax){
  ICrit = VARselect(Statdata,lag.max=i, season=NULL, exogen=NULL)
  AIC[i,1]  = ICrit$selection[1]
  AIC[i,2]  = ICrit$criteria[1]
}; rm(ICrit, i)
png(file.path(getwd(),"Figures",paste("AIC1.png",sep="")),
    width=1600,height=800)
plot(AIC[,1],type="l",xlab="p",ylab="AIC(p) lag suggestion",main="",
     cex.lab=1.6,cex.axis=1.75,cex.sub=2.5, lwd=2); dev.off()
png(file.path(getwd(),"Figures",paste("AIC2.png",sep="")),
    width=1600,height=800)
plot(AIC[,2],type="l",xlab="p",ylab="AIC(p)", main="",
     cex.lab=1.6,cex.axis=1.75,cex.sub=2.5, lwd=2); dev.off()

# Choose order
VARp = 7

# Estimate model
VARest = VAR(Statdata, p=VARp, season=NULL, exogen=NULL)
Acoef(VARest) # Coefficient matrix(es)

# Stationary
CADFtest(Statdata[,1], type="none")

# Residuals 
VARres = ts(resid(VARest), start=c(2013,1), frequency=365.25)
for (i in 1:ncol(VARres)){
  png(file.path(getwd(),"Figures",paste("Res",i,".png",sep="")),
      width=2000,height=800)
  plot(VARres[,i],xlab="Years",main="",
          cex.lab=2.5,cex.axis=2.5,cex.sub=2.5, type="l"); dev.off()
  png(file.path(getwd(),"Figures",paste("Res-ACF",i,".png",sep="")),
      width=1600,height=1000)
  acf(VARres[,i], xlab="Years",main=colnames(tsdata)[i],ylab="",
      cex.lab=2.5,cex.axis=2.5,cex.sub=2.5, cex.main=3); dev.off()
  png(file.path(getwd(),"Figures",paste("Res-Norm",i,".png",sep="")),
      width=1600,height=1000)
  qqnorm(VARres[,1],cex.lab=2.5,cex.axis=2.5,cex.sub=2.5, main="");
  qqline(VARres[,1]); dev.off()
}; rm(i)

# Residual testing
serial.test(VARest, type="BG")
normality.test(VARest,multivariate.only=FALSE) 
arch.test(VARest,lags.multi=VARp,multivariate.only=FALSE,lags.single=VARp)


# Impulse response function -----------------------------------------------
png(filename=file.path(getwd(),"Figures",paste("IRF2-1.png", sep="")),
    width=1600, height=1000)
plot(irf(VARest, response="Series.1", impulse=c("Series.2"),
    runs=100, n.ahead=225),main="",cex.lab=1.5,lwd=2
    ,cex.axis=2,cex.sub=1.5,ylab="Logarithmic price"); dev.off()
png(filename=file.path(getwd(),"Figures",paste("IRF3-1.png", sep="")),
    width=1600, height=1000)
plot(irf(VARest, response="Series.1", impulse=c("Series.3"),
    runs=100, n.ahead=225),main="",cex.lab=1.5,lwd=2
    ,cex.axis=2,cex.sub=1.5,ylab="Logarithmic price"); dev.off()
png(filename=file.path(getwd(),"Figures",paste("IRF4-1.png", sep="")),
    width=1600, height=1000)
plot(irf(VARest, response="Series.1", impulse=c("Series.4"),
    runs=100, n.ahead=375),main="",cex.lab=1.5,lwd=2
    ,cex.axis=2,cex.sub=1.5,ylab="Logarithmic price"); dev.off()


# Forecasting -------------------------------------------------------------
m = 100 # Number of one-step forecasts
DataPred = matrix(nrow=m, ncol=ncol(Statdata))
PredCI   = matrix(nrow=m, ncol=ncol(Statdata))
for (i in 1:(m)){
  VARest_i  = VAR(Statdata[1:(nrow(tsdata)-m+i),], p=VARp)
  VARpred_i = predict(VARest_i, n.ahead=1, ci=0.95)
  DataPred[i,1] = VARpred_i$fcst$Series.1[1]
  PredCI[i,1]   = VARpred_i$fcst$Series.1[1,4]
  DataPred[i,2] = VARpred_i$fcst$Series.2[1]
  PredCI[i,2]   = VARpred_i$fcst$Series.2[1,4]
  DataPred[i,3] = VARpred_i$fcst$Series.3[1]
  PredCI[i,3]   = VARpred_i$fcst$Series.3[1,4]
  DataPred[i,4] = VARpred_i$fcst$Series.4[1]
  PredCI[i,4]   = VARpred_i$fcst$Series.4[1,4]
}; rm(i, VARest_i, VARpred_i)
colnames(DataPred) = colnames(tsdata)
colnames(PredCI)   = colnames(tsdata)

# Add season and trend back
DataPred[,1] = DataPred[,1]+matrix(predict.lm(Sea_Price))[(nrow(tsdata)-m+1):nrow(tsdata),1]+matrix(predict.lm(Tre_Price))[(nrow(tsdata)-m+1):nrow(tsdata),1]
DataPred[,2] = DataPred[,2]+matrix(predict.lm(Sea_WindP))[(nrow(tsdata)-m+1):nrow(tsdata),1]+matrix(predict.lm(Tre_WindP))[(nrow(tsdata)-m+1):nrow(tsdata),1]
DataPred[,3] = DataPred[,3]+matrix(predict.lm(Sea_Produ))[(nrow(tsdata)-m+1):nrow(tsdata),1]+matrix(predict.lm(Tre_Produ))[(nrow(tsdata)-m+1):nrow(tsdata),1]
DataPred[,4] = DataPred[,4]+matrix(predict.lm(Sea_Consu))[(nrow(tsdata)-m+1):nrow(tsdata),1]+matrix(predict.lm(Tre_Consu))[(nrow(tsdata)-m+1):nrow(tsdata),1]

# Intervals
for (i in 1:ncol(DataPred)){
  png(filename=file.path(getwd(),"Figures",paste("Forecast",i,".png",sep=""
                                            )), width=1750, height=1000)
  plot(cbind(ts(tsdata[,i]),ts(DataPred[1:m,i], start=c(nrow(tsdata)-m+1)))
       ,plot.type=c("single"), col=c("grey","red"), 
       xlim=c(nrow(tsdata)-1.25*m,nrow(tsdata)), ylab=colnames(tsdata)[i],
       xlab="Days", lwd=2,ylim=c(mean(DataPred[,i])-mean(PredCI[,i])*2.25,
                                 mean(DataPred[,i])+mean(PredCI[,i])*2.25))
  lines(ts(DataPred[,i]+PredCI[,i], start=c(nrow(tsdata)-m+1)),
        col=c("blue"), lwd=1.25)
  lines(ts(DataPred[,i]-PredCI[,i], start=c(nrow(tsdata)-m+1)),
        col=c("blue"), lwd=1.25)
  legend("topleft", legend=c("Observed", "Forecasted","Confidence interval"),
         col=c("grey", "red","blue"), lwd=3, cex=2.75)
  dev.off()
}; rm(i)

# RMSE of prediction
PredRes = matrix(nrow=m, ncol=ncol(DataPred))
colnames(PredRes) = colnames(tsdata)
for (z in 1:ncol(DataPred)){for (i in 1:m){
  PredRes[i,z] = tsdata[(nrow(tsdata)-m+i),z]-DataPred[i,z]
  }
  cat("The RMSE of", colnames(PredRes)[z], "is",sprintf("%.2f",sqrt(mean(PredRes[,z]^2))),"\n")
}; rm(i,z)


# Model comparison --------------------------------------------------------
for (j in c(2,6,8,12)){ # VAR(j) models
  DataPred2 = matrix(nrow=m, ncol=ncol(Statdata))
  for (i in 1:(m)){
    VARest_i  = VAR(Statdata[1:(nrow(tsdata)-m+i),], p=j)
    VARpred_i = predict(VARest_i, n.ahead=1, ci=0.95)
    DataPred2[i,1] = VARpred_i$fcst$Series.1[1]
    DataPred2[i,2] = VARpred_i$fcst$Series.2[1]
    DataPred2[i,3] = VARpred_i$fcst$Series.3[1]
    DataPred2[i,4] = VARpred_i$fcst$Series.4[1]
  }; rm(i, VARest_i, VARpred_i)
  colnames(DataPred2) = colnames(tsdata)
  
  DataPred2[,1] = DataPred2[,1]+matrix(predict.lm(Sea_Price))[(nrow(tsdata)-m+1):nrow(tsdata),1]+matrix(predict.lm(Tre_Price))[(nrow(tsdata)-m+1):nrow(tsdata),1]
  DataPred2[,2] = DataPred2[,2]+matrix(predict.lm(Sea_WindP))[(nrow(tsdata)-m+1):nrow(tsdata),1]+matrix(predict.lm(Tre_WindP))[(nrow(tsdata)-m+1):nrow(tsdata),1]
  DataPred2[,3] = DataPred2[,3]+matrix(predict.lm(Sea_Produ))[(nrow(tsdata)-m+1):nrow(tsdata),1]+matrix(predict.lm(Tre_Produ))[(nrow(tsdata)-m+1):nrow(tsdata),1]
  DataPred2[,4] = DataPred2[,4]+matrix(predict.lm(Sea_Consu))[(nrow(tsdata)-m+1):nrow(tsdata),1]+matrix(predict.lm(Tre_Consu))[(nrow(tsdata)-m+1):nrow(tsdata),1]
  
  PredRes2 = matrix(nrow=m, ncol=ncol(DataPred2))
  colnames(PredRes2) = colnames(tsdata)
  for (z in 1:ncol(DataPred2)){for (i in 1:m){
    PredRes2[i,z] = tsdata[(nrow(tsdata)-m+i),z]-DataPred2[i,z]
  }}; rm(i,z)
  
  # Testing models
  'Null hypothesis is that they have the same accuracy. Alternative is the 
  alternative hypothesis is that method 2 is more accurate than method 1'
  print(dm.test(PredRes,PredRes2,alternative="less")) # Same accuracy if p>0.05
  print(dm.test(PredRes,PredRes2,alternative="greater")) # Same accuracy if p>0.05
}; rm(j)

