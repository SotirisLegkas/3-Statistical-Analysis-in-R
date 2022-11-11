#Exercise 1
#Read and create the appropriate dataset
datas = read.csv("C:/Users/sotir/Desktop/Data Science/1st Semester/Probability and Statistics for Data Analysis/new HW3/Assignment_3_Data.csv", sep=";",dec=" ",stringsAsFactors=FALSE)
datas[,1]=as.numeric(gsub(",", ".", datas[,1]))
datas[,2]=as.numeric(gsub(",", ".", datas[,2]))
datas[,3]=as.numeric(gsub(",", ".", datas[,3]))
datas[,4]=as.numeric(gsub(",", ".", datas[,4]))
datas[,5]=as.factor(datas[,5])

#A
#Plot the Y on W
plot(datas$W,datas$Y,data=datas,main='Boxplots for Y & W',xlab='W',ylab='Y')
#Plot the X1 on W
plot(datas$W,datas$X1,data=datas,main='Boxplots for X1 & W',xlab='W',ylab='X1')
#Plot the X2 on W
plot(datas$W,datas$X2,data=datas,main='Boxplots for X2 & W',xlab='W',ylab='X2')
#Plot the X3 on W
plot(datas$W,datas$X3,data=datas,main='Boxplots for X3 & W',xlab='W',ylab='X3')

#Anova and tests for assumptions for Y
fitY=aov(Y~W,data=datas)
summary(fitY)
bartlett.test(Y~W,data=datas)
shapiro.test(fitY$residuals)
par(mfrow=c(2,2))	### provide plots in a 2x2 layout
plot(fitY)
mtext("Diagnostic plots", outer=TRUE, line=-2, font=2, cex=1.2)

#Transform Y so the assumptions for normality and homogeneity hold
testY=aov(log(Y)~W,data=datas)
summary(testY)
bartlett.test(log(Y)~W,data=datas)
shapiro.test(testY$residuals)

#Anova and tests for assumptions for X1
fitX1=aov(X1~W,data=datas)
summary(fitX1)
bartlett.test(X1~W,data=datas)
shapiro.test(fitX1$residuals)
par(mfrow=c(2,2))	### provide plots in a 2x2 layout
plot(fitX1)
mtext("Diagnostic plots", outer=TRUE, line=-2, font=2, cex=1.2)

#Anova and tests for assumptions for X2
fitX2=aov(X2~W,data=datas)
summary(fitX2)
bartlett.test(X2~W,data=datas)
shapiro.test(fitX2$residuals)
par(mfrow=c(2,2))	### provide plots in a 2x2 layout
plot(fitX2)
mtext("Diagnostic plots", outer=TRUE, line=-2, font=2, cex=1.2)

#Transform X2 so the assumptions for normality and homogeneity hold
testX2=aov(log(X2)~W,data=datas)
summary(testX2)
bartlett.test(log(X2)~W,data=datas)
shapiro.test(testX2$residuals)

#Anova and tests for assumptions for X3
fitX3=aov(X3~W,data=datas)
summary(fitX3)
bartlett.test(X3~W,data=datas)
shapiro.test(fitX3$residuals)
par(mfrow=c(2,2))	### provide plots in a 2x2 layout
plot(fitX3)
mtext("Diagnostic plots", outer=TRUE, line=-2, font=2, cex=1.2)

#Transform X3 so the assumptions for normality and homogeneity hold
testX3=aov(I(X3^(1/2))~W,data=datas)
summary(testX3)
bartlett.test(I(X3^(1/2))~W,data=datas)
shapiro.test(testX3$residuals)

#########################################################################################################
#B

library(alr3)
attach(highway)
scatterplotMatrix(~Y+X1+X2+X3,data=datas, smooth=FALSE,groups=datas$W, col=palette()[1:3])
title("Scattrplot matrix", outer=TRUE, line=-1, font=2, cex=1.2)
#########################################################################################################
#C (Y on X1)
fitX1=lm(Y~X1,data=datas)
summary(fitX1)
#########################################################################################################
#D (Y on X1+X2+X3+W+W*X1+W*X2+W*X3)

fit=lm(Y~X1+X2+X3+W+W*X1+W*X2+W*X3,data=datas)
summary(fit)
coefficients(fit)
#########################################################################################################
#E
#ANOVA test and normality and homoskedasticity tests
anova(fit)
shapiro.test(fit$residuals)

par(mfrow=c(2,2))	### provide plots in a 2x2 layout
plot(fit)
mtext("Diagnostic plots", outer=TRUE, line=-2, font=2, cex=1.2)
#########################################################################################################
#F Stepwise regression
step(fit,direction = 'both')
#########################################################################################################
#G
datas$Z=cut(datas$X3,breaks=4)

#Table for X3 and W
contW=table(datas$X3,datas$W)
print(contW)

#Table for X3 and Z
contZ=table(datas$X3,datas$Z)
print(contZ)

#Table for Z and W
contWZ=table(datas$W,datas$Z)
print(contWZ)
#########################################################################################################
#H
#Two Way ANOVA
fit=aov(Y~W+Z,data=datas)
summary(fit)

#Test assumptions
shapiro.test(fit$residuals)
par(mfrow=c(2,2))	### provide plots in a 2x2 layout
plot(fitX3)
mtext("Diagnostic plots", outer=TRUE, line=-2, font=2, cex=1.2)