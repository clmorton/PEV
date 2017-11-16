#Load Data
setwd("~/GitHub/PEV")
attach(Integrated_Spreadsheet_contiguous)

#Load Packages; use the function instal.package() if a first instal is needed
library(psych)
library(car)
library(ggplot2)
library(Hmisc)
library(corrplot)

#Histograms of Private EVs per '000 cars across the UK 

hist(PPEV1k, breaks = 50, xlab = "Private Plug-in Electric Vehicles per thousand cars", main = NULL, font.lab = 2, col = "gray80")
hist(PBEV1k, breaks = 50, xlab = "Private Pure Battery Electric Vehicles per thousand cars", main = NULL, font.lab = 2, col = "gray80")
hist(PPHEV1k, breaks = 50, xlab = "Private Plug-in Hybrid Electric Vehicles per thousand cars", main = NULL, font.lab = 2, col = "gray80")

#Descriptive Statistics

describe(Integrated_Spreadsheet)

#Bivariate Analaysis with Socioeconomics 

pairs.panels(Integrated_Spreadsheet_contiguous[c(7,19:30)], method = "spearman", hist.col = "gray80", ellipses = "FALSE", breaks = 25)
cor1 <- rcorr(as.matrix(Integrated_Spreadsheet_contiguous[c(7,19:30)]), type = "spearman")
cor1
cor1$r
cor1$P
corrplot(cor1$r, type="upper", order="original", 
         p.mat = cor1$P, sig.level = 0.01, insig = "blank", tl.srt = 45, tl.col = "black")

#Bivariate Analaysis with Travel 

pairs.panels(Integrated_Spreadsheet_contiguous[c(7,32:42,53,6)], method = "spearman", hist.col = "gray80", ellipses = "FALSE", breaks = 25)
cor2 <- rcorr(as.matrix(Integrated_Spreadsheet_contiguous[c(7,32:42,53,6)]), type = "spearman")
cor2
cor2$r
cor2$P
corrplot(cor2$r, type="upper", order="original", 
         p.mat = cor2$P, sig.level = 0.01, insig = "blank", tl.srt = 45, tl.col = "black")

#Bivariate Analaysis with Household

pairs.panels(Integrated_Spreadsheet_contiguous[c(7,43:52)], method = "spearman", hist.col = "gray80", ellipses = "FALSE", breaks = 25)
cor3 <- rcorr(as.matrix(Integrated_Spreadsheet_contiguous[c(7,43:52)]), type = "spearman")
cor3
cor3$r
cor3$P
corrplot(cor3$r, type="upper", order="original", 
         p.mat = cor3$P, sig.level = 0.01, insig = "blank", tl.srt = 45, tl.col = "black")

# Benchmark OLS log-log Regression Models

mod1 <- lm(log(PPEV1k) ~ log(MedianAge) + log(Level4) + log(SelfEmp) + log(MedianY))
summary(mod1)
par(mfrow=c(2,2))
plot(mod1, main = "Socioeconomic Model")
vif(mod1)
anova(mod1)
AIC(mod1)

mod2 <- lm(log(PPEV1k) ~ log(PopDens) + log(SemiD) + log(MeanRes))
summary(mod2)
par(mfrow=c(2,2))
plot(mod2, main = "Household Model")
vif(mod2)
anova(mod2)
AIC(mod2)

mod3 <- lm(log(PPEV1k) ~ log(OneCar) + log(CarDrive) + log(PHEV1k) + TotCPoint)
summary(mod3)
par(mfrow=c(2,2))
plot(mod3, main = "Transport Model")
vif(mod3)
anova(mod3)
AIC(mod3)

mod4 <- lm(log(PPEV1k) ~ log(MedianAge) + log(Level4) + log(SelfEmp) + log(MedianY) +
        log(PopDens) + log(SemiD) + log(MeanRes) + log(OneCar) + log(CarDrive) + log(PHEV1k) + TotCPoint)
summary(mod4)
par(mfrow=c(2,2))
plot(mod4, main = "Integrated Model")
vif(mod4)
anova(mod4)
AIC(mod4)