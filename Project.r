
setwd("/Users/sreevedatippavajhala/Library/CloudStorage/OneDrive-UniversityoftheFraserValley/Documents/Winter/STAT 315/project")

dataset <- read.csv("TRAIN.csv", header=TRUE)
attach(dataset)
head(dataset)

# logistic regression model

full.model1 <- glm(TenYearCHD ~ age+ education+ as.factor(sex)+as.factor(is_smoking) 
                   +cigsPerDay+as.factor(BPMeds)+as.factor(prevalentStroke)+as.factor(prevalentHyp)
                   +as.factor(diabetes)+totChol+sysBP+diaBP+BMI+heartRate+glucose, family = binomial())
summary(full.model1)

#install.packages("alr4")
library(alr4)
mmps(full.model1)

model1<-  glm(TenYearCHD ~ age+ as.factor(sex)
              +cigsPerDay+as.factor(prevalentStroke)+totChol+sysBP+glucose, family = binomial())
summary(model1)

# Using marginal model plots from model (1) to check “Is model (1) a valid model for the data”? 

mmps(model1)

# VIF
library(car)
vif(model1) 

#Gaussin density curves.
x1 =age
x2 =education
x3=sex
x4=is_smoking
x5=cigsPerDay
x6=BPMeds
x7=prevalentStroke
x8=prevalentHyp
x9=diabetes
x10=totChol
x11=sysBP
x12=diaBP
x13=BMI
x14=heartRate
x15=glucose
y=TenYearCHD

par(mfrow=c(3,3))
plot(density(x1[y == 0 ],bw="SJ",kern="gaussian"),type="l",
     main="Gaussian kernel density estimate",xlab="age")
rug(x1[y==0])
lines(density(x1[y==1],bw="SJ",kern="gaussian"),lty=2)
rug(x1[y==1])
legend("topright",legend=c("No","Yes"),lty=1:2,title="TenYearCHD?")

plot(density(x2[y == 0 ],bw="SJ",kern="gaussian"),type="l",
     main="Gaussian kernel density estimate",xlab="education")
rug(x2[y==0])
lines(density(x2[y==1],bw="SJ",kern="gaussian"),lty=2)
rug(x2[y==1])
legend("topright",legend=c("No","Yes"),lty=1:2,title="TenYearCHD?")

plot(density(x5[y == 0 ],bw="SJ",kern="gaussian"),type="l",
     main="Gaussian kernel density estimate",xlab="cigsPerDay")
rug(x5[y==0])
lines(density(x5[y==1],bw="SJ",kern="gaussian"),lty=2)
rug(x5[y==1])
legend("topright",legend=c("No","Yes"),lty=1:2,title="TenYearCHD?")

plot(density(x10[y == 0 ],bw="SJ",kern="gaussian"),type="l",
     main="Gaussian kernel density estimate",xlab="totChol")
rug(x10[y==0])
lines(density(x10[y==1],bw="SJ",kern="gaussian"),lty=2)
rug(x10[y==1])
legend("topright",legend=c("No","Yes"),lty=1:2,title="TenYearCHD?")

plot(density(x11[y == 0 ],bw="SJ",kern="gaussian"),type="l",
     main="Gaussian kernel density estimate",xlab="sysBP")
rug(x11[y==0])
lines(density(x11[y==1],bw="SJ",kern="gaussian"),lty=2)
rug(x11[y==1])
legend("topright",legend=c("No","Yes"),lty=1:2,title="TenYearCHD?")

plot(density(x12[y == 0 ],bw="SJ",kern="gaussian"),type="l",
     main="Gaussian kernel density estimate",xlab="diaBP")
rug(x12[y==0])
lines(density(x12[y==1],bw="SJ",kern="gaussian"),lty=2)
rug(x12[y==1])
legend("topright",legend=c("No","Yes"),lty=1:2,title="TenYearCHD?")

plot(density(x13[y == 0 ],bw="SJ",kern="gaussian"),type="l",
     main="Gaussian kernel density estimate",xlab="BMI")
rug(x13[y==0])
lines(density(x13[y==1],bw="SJ",kern="gaussian"),lty=2)
rug(x13[y==1])
legend("topright",legend=c("No","Yes"),lty=1:2,title="TenYearCHD?")

plot(density(x14[y == 0 ],bw="SJ",kern="gaussian"),type="l",
     main="Gaussian kernel density estimate",xlab="heartRate")
rug(x14[y==0])
lines(density(x14[y==1],bw="SJ",kern="gaussian"),lty=2)
rug(x14[y==1])
legend("topright",legend=c("No","Yes"),lty=1:2,title="TenYearCHD?")

plot(density(x15[y == 0 ],bw="SJ",kern="gaussian"),type="l",
     main="Gaussian kernel density estimate",xlab="glucose")
rug(x15[y==0])
lines(density(x12[y==1],bw="SJ",kern="gaussian"),lty=2)
rug(x15[y==1])
legend("topright",legend=c("No","Yes"),lty=1:2,title="TenYearCHD?")


# logistic regression model with transformed predictor variables

full.model2 <- glm(TenYearCHD ~ log(age) + education + as.factor(sex) + as.factor(is_smoking) + cigsPerDay+ 
                     as.factor(BPMeds) + as.factor(prevalentStroke) + as.factor(prevalentHyp) + as.factor(diabetes) + 
                     totChol + sysBP + diaBP+log(diaBP) + BMI + heartRate +glucose+log(glucose), family = binomial())
summary(full.model2)


# model2<-glm(TenYearCHD ~ I(age^2) + as.factor(sex)  + cigsPerDay + as.factor(prevalentStroke) +
#               totChol + sysBP + diaBP+log(diaBP) +  glucose, family = binomial())
# summary(model2)

model2<-glm(TenYearCHD ~ log(age) + as.factor(sex)  + cigsPerDay + as.factor(prevalentStroke) +
              totChol + sysBP + diaBP+log(diaBP) +  glucose, family = binomial())
summary(model2)


mmps(model2)

#  Plots of predictor variables ratings with different slopes for each value of y

par(mfrow=c(2,2))

plot(log(age),education, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="log(age)", ylab="education")
abline(lsfit(log(age)[TenYearCHD==0],  education[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(log(age)[TenYearCHD==1], education[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

plot(log(age),cigsPerDay, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="log(age)", ylab="cigsPerDay")
abline(lsfit(log(age)[TenYearCHD==0],  cigsPerDay[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(log(age)[TenYearCHD==1], cigsPerDay[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

plot(log(age),totChol, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="log(age)", ylab="totChol")
abline(lsfit(log(age)[TenYearCHD==0],  totChol[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(log(age)[TenYearCHD==1], totChol[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

plot(log(age),sysBP, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="log(age)", ylab="sysBP")
abline(lsfit(log(age)[TenYearCHD==0],  sysBP[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(log(age)[TenYearCHD==1], sysBP[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

par(mfrow=c(2,2))
plot(log(age),BMI, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="log(age)", ylab="BMI")
abline(lsfit(log(age)[TenYearCHD==0],  BMI[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(log(age)[TenYearCHD==1], BMI[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

plot(log(age),diaBP, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="log(age)", ylab="diaBP")
abline(lsfit(log(age)[TenYearCHD==0],  diaBP[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(log(age)[TenYearCHD==1], diaBP[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")


plot(log(age),heartRate, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="log(age)", ylab="heartRate")
abline(lsfit(log(age)[TenYearCHD==0],  heartRate[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(log(age)[TenYearCHD==1], heartRate[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

plot(log(age),glucose, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="log(age)", ylab="glucose")
abline(lsfit(log(age)[TenYearCHD==0],  glucose[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(log(age)[TenYearCHD==1], glucose[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

par(mfrow=c(2,2))

plot(log(age),BPMeds, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="log(age)", ylab="BPMeds")
abline(lsfit(log(age)[TenYearCHD==0],  BPMeds[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(log(age)[TenYearCHD==1], BPMeds[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

plot(log(age),prevalentStroke, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="log(age)", ylab="prevalentStroke")
abline(lsfit(log(age)[TenYearCHD==0],  prevalentStroke[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(log(age)[TenYearCHD==1], prevalentStroke[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

plot(log(age),prevalentHyp, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="log(age)", ylab="prevalentHyp")
abline(lsfit(log(age)[TenYearCHD==0],  prevalentHyp[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(log(age)[TenYearCHD==1], prevalentHyp[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

plot(log(age),prevalentStroke, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="log(age)", ylab="prevalentStroke")
abline(lsfit(log(age)[TenYearCHD==0],  prevalentStroke[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(log(age)[TenYearCHD==1], prevalentStroke[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

par(mfrow=c(2,2))

plot(log(age),diabetes, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="log(age)", ylab="diabetes")
abline(lsfit(log(age)[TenYearCHD==0],  diabetes[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(age[TenYearCHD==1], diabetes[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

# 
plot(education,cigsPerDay, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="education", ylab="cigsPerDay")
abline(lsfit(education[TenYearCHD==0],  cigsPerDay[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(education[TenYearCHD==1], cigsPerDay[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")


plot(education,totChol, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="education", ylab="totChol")
abline(lsfit(education[TenYearCHD==0],  totChol[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(education[TenYearCHD==1], totChol[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

plot(education,sysBP, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="education", ylab="sysBP")
abline(lsfit(education[TenYearCHD==0],  sysBP[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(education[TenYearCHD==1], sysBP[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

par(mfrow=c(2,2))

plot(education,BMI, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="education", ylab="BMI")
abline(lsfit(education[TenYearCHD==0],  BMI[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(education[TenYearCHD==1], BMI[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

plot(education,diaBP, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="education", ylab="diaBP")
abline(lsfit(education[TenYearCHD==0],  diaBP[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(education[TenYearCHD==1], diaBP[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")


plot(education,heartRate, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="education", ylab="heartRate")
abline(lsfit(education[TenYearCHD==0],  heartRate[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(education[TenYearCHD==1], heartRate[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

plot(education,glucose, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="education", ylab="glucose")
abline(lsfit(education[TenYearCHD==0],  glucose[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(education[TenYearCHD==1], glucose[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

par(mfrow=c(2,2))

plot(education,BPMeds, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="education", ylab="BPMeds")
abline(lsfit(education[TenYearCHD==0],  BPMeds[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(education[TenYearCHD==1], BPMeds[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

plot(education,prevalentStroke, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="education", ylab="prevalentStroke")
abline(lsfit(education[TenYearCHD==0],  prevalentStroke[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(education[TenYearCHD==1], prevalentStroke[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

plot(education,prevalentHyp, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="education", ylab="prevalentHyp")
abline(lsfit(education[TenYearCHD==0],  prevalentHyp[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(education[TenYearCHD==1], prevalentHyp[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

plot(education,prevalentStroke, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="education", ylab="prevalentStroke")
abline(lsfit(education[TenYearCHD==0],  prevalentStroke[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(education[TenYearCHD==1], prevalentStroke[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

par(mfrow=c(2,2))

plot(education,diabetes, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="education", ylab="diabetes")
abline(lsfit(education[TenYearCHD==0],  diabetes[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(education[TenYearCHD==1], diabetes[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

#cigsPerDay

plot(cigsPerDay,BPMeds, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="cigsPerDay", ylab="BPMeds")
abline(lsfit(cigsPerDay[TenYearCHD==0],  BPMeds[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(cigsPerDay[TenYearCHD==1], BPMeds[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

plot(cigsPerDay,prevalentStroke, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="cigsPerDay", ylab="prevalentStroke")
abline(lsfit(cigsPerDay[TenYearCHD==0],  prevalentStroke[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(cigsPerDay[TenYearCHD==1], prevalentStroke[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")


plot(cigsPerDay,prevalentHyp, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="cigsPerDay", ylab="prevalentHyp")
abline(lsfit(cigsPerDay[TenYearCHD==0],  prevalentHyp[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(cigsPerDay[TenYearCHD==1], prevalentHyp[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

par(mfrow=c(2,2))

plot(cigsPerDay,diabetes, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="cigsPerDay", ylab="diabetes")
abline(lsfit(cigsPerDay[TenYearCHD==0],  diabetes[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(cigsPerDay[TenYearCHD==1], diabetes[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

plot(cigsPerDay,totChol, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="cigsPerDay", ylab="totChol")
abline(lsfit(cigsPerDay[TenYearCHD==0],  totChol[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(cigsPerDay[TenYearCHD==1], totChol[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

plot(cigsPerDay,sysBP, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="cigsPerDay", ylab="sysBP")
abline(lsfit(cigsPerDay[TenYearCHD==0],  sysBP[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(cigsPerDay[TenYearCHD==1], sysBP[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

plot(cigsPerDay,BMI, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="cigsPerDay", ylab="BMI")
abline(lsfit(cigsPerDay[TenYearCHD==0],  BMI[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(cigsPerDay[TenYearCHD==1], BMI[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

par(mfrow=c(2,2))

plot(cigsPerDay,diaBP, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="cigsPerDay", ylab="diaBP")
abline(lsfit(cigsPerDay[TenYearCHD==0],  diaBP[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(cigsPerDay[TenYearCHD==1], diaBP[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

plot(cigsPerDay,heartRate, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="cigsPerDay", ylab="heartRate")
abline(lsfit(cigsPerDay[TenYearCHD==0],  heartRate[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(cigsPerDay[TenYearCHD==1], heartRate[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

plot(cigsPerDay,glucose, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="cigsPerDay", ylab="glucose")
abline(lsfit(cigsPerDay[TenYearCHD==0],  glucose[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(cigsPerDay[TenYearCHD==1], glucose[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

#BPMeds

plot(BPMeds,prevalentStroke, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="BPMeds", ylab="prevalentStroke")
abline(lsfit(BPMeds[TenYearCHD==0],  prevalentStroke[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(BPMeds[TenYearCHD==1], prevalentStroke[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

par(mfrow=c(2,2))

plot(BPMeds,prevalentHyp, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="BPMeds", ylab="prevalentHyp")
abline(lsfit(BPMeds[TenYearCHD==0],  prevalentHyp[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(BPMeds[TenYearCHD==1], prevalentHyp[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

plot(BPMeds,diabetes, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="BPMeds", ylab="diabetes")
abline(lsfit(BPMeds[TenYearCHD==0],  diabetes[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(BPMeds[TenYearCHD==1], diabetes[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

plot(BPMeds,totChol, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="BPMeds", ylab="totChol")
abline(lsfit(BPMeds[TenYearCHD==0],  totChol[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(BPMeds[TenYearCHD==1], totChol[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

plot(BPMeds,sysBP, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="BPMeds", ylab="sysBP")
abline(lsfit(BPMeds[TenYearCHD==0],  sysBP[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(BPMeds[TenYearCHD==1], sysBP[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

par(mfrow=c(2,2))

plot(BPMeds,BMI, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="BPMeds", ylab="BMI")
abline(lsfit(BPMeds[TenYearCHD==0],  BMI[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(BPMeds[TenYearCHD==1], BMI[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

plot(BPMeds,diaBP, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="BPMeds", ylab="diaBP")
abline(lsfit(BPMeds[TenYearCHD==0],  diaBP[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(BPMeds[TenYearCHD==1], diaBP[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")


plot(BPMeds,heartRate, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="BPMeds", ylab="heartRate")
abline(lsfit(BPMeds[TenYearCHD==0],  heartRate[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(BPMeds[TenYearCHD==1], heartRate[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

plot(BPMeds,glucose, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="BPMeds", ylab="glucose")
abline(lsfit(BPMeds[TenYearCHD==0],  glucose[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(BPMeds[TenYearCHD==1], glucose[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

par(mfrow=c(2,2))

#prevalentStroke

plot(prevalentStroke,prevalentHyp, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="prevalentStroke", ylab="prevalentHyp")
abline(lsfit(prevalentStroke[TenYearCHD==0],  prevalentHyp[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(prevalentStroke[TenYearCHD==1], prevalentHyp[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

plot(prevalentStroke,diabetes, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="prevalentStroke", ylab="diabetes")
abline(lsfit(prevalentStroke[TenYearCHD==0],  diabetes[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(prevalentStroke[TenYearCHD==1], diabetes[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

plot(prevalentStroke,totChol, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="prevalentStroke", ylab="totChol")
abline(lsfit(prevalentStroke[TenYearCHD==0],  totChol[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(prevalentStroke[TenYearCHD==1], totChol[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

plot(prevalentStroke,sysBP, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="prevalentStroke", ylab="sysBP")
abline(lsfit(prevalentStroke[TenYearCHD==0],  sysBP[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(prevalentStroke[TenYearCHD==1], sysBP[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

par(mfrow=c(2,2))

plot(prevalentStroke,BMI, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="prevalentStroke", ylab="BMI")
abline(lsfit(prevalentStroke[TenYearCHD==0],  BMI[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(prevalentStroke[TenYearCHD==1], BMI[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

plot(prevalentStroke,diaBP, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="prevalentStroke", ylab="diaBP")
abline(lsfit(prevalentStroke[TenYearCHD==0],  diaBP[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(prevalentStroke[TenYearCHD==1], diaBP[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

plot(prevalentStroke,heartRate, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="prevalentStroke", ylab="heartRate")
abline(lsfit(prevalentStroke[TenYearCHD==0],  heartRate[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(prevalentStroke[TenYearCHD==1], heartRate[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

plot(prevalentStroke,glucose, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="prevalentStroke", ylab="glucose")
abline(lsfit(prevalentStroke[TenYearCHD==0],  glucose[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(prevalentStroke[TenYearCHD==1], glucose[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

par(mfrow=c(2,2))
# prevalentHyp

plot(prevalentHyp,diabetes, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="prevalentHyp", ylab="diabetes")
abline(lsfit(prevalentHyp[TenYearCHD==0],  diabetes[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(prevalentHyp[TenYearCHD==1], diabetes[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

plot(prevalentHyp,totChol, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="prevalentHyp", ylab="totChol")
abline(lsfit(prevalentHyp[TenYearCHD==0],  totChol[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(prevalentHyp[TenYearCHD==1], totChol[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

plot(prevalentHyp,sysBP, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="prevalentHyp", ylab="sysBP")
abline(lsfit(prevalentHyp[TenYearCHD==0],  sysBP[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(prevalentHyp[TenYearCHD==1], sysBP[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

plot(prevalentHyp,BMI, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="prevalentHyp", ylab="BMI")
abline(lsfit(prevalentHyp[TenYearCHD==0],  BMI[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(prevalentHyp[TenYearCHD==1], BMI[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

par(mfrow=c(2,2))

plot(prevalentHyp,diaBP, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="prevalentHyp", ylab="diaBP")
abline(lsfit(prevalentHyp[TenYearCHD==0],  diaBP[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(prevalentHyp[TenYearCHD==1], diaBP[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")


plot(prevalentHyp,heartRate, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="prevalentHyp", ylab="heartRate")
abline(lsfit(prevalentHyp[TenYearCHD==0],  heartRate[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(prevalentHyp[TenYearCHD==1], heartRate[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

plot(prevalentHyp,glucose, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="prevalentHyp", ylab="glucose")
abline(lsfit(prevalentHyp[TenYearCHD==0],  glucose[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(prevalentHyp[TenYearCHD==1], glucose[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

# diabetes

plot(diabetes,totChol, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="diabetes", ylab="totChol")
abline(lsfit(diabetes[TenYearCHD==0],  totChol[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(diabetes[TenYearCHD==1], totChol[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

par(mfrow=c(2,2))

plot(diabetes,sysBP, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="diabetes", ylab="sysBP")
abline(lsfit(diabetes[TenYearCHD==0],  sysBP[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(diabetes[TenYearCHD==1], sysBP[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

plot(diabetes,BMI, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="diabetes", ylab="BMI")
abline(lsfit(diabetes[TenYearCHD==0],  BMI[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(diabetes[TenYearCHD==1], BMI[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

plot(diabetes,diaBP, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="diabetes", ylab="diaBP")
abline(lsfit(diabetes[TenYearCHD==0],  diaBP[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(diabetes[TenYearCHD==1], diaBP[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")


plot(diabetes,heartRate, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="diabetes", ylab="heartRate")
abline(lsfit(diabetes[TenYearCHD==0],  heartRate[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(diabetes[TenYearCHD==1], heartRate[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

par(mfrow=c(2,2)) 

plot(diabetes,glucose, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="diabetes", ylab="glucose")
abline(lsfit(diabetes[TenYearCHD==0],  glucose[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(diabetes[TenYearCHD==1], glucose[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

# totChol

plot(totChol,sysBP, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="totChol", ylab="sysBP")
abline(lsfit(totChol[TenYearCHD==0],  sysBP[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(totChol[TenYearCHD==1], sysBP[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

plot(totChol,BMI, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="totChol", ylab="BMI")
abline(lsfit(totChol[TenYearCHD==0],  BMI[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(totChol[TenYearCHD==1], BMI[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

plot(totChol,diaBP, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="totChol", ylab="diaBP")
abline(lsfit(totChol[TenYearCHD==0],  diaBP[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(totChol[TenYearCHD==1], diaBP[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

par(mfrow=c(2,2))

plot(totChol,heartRate, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="totChol", ylab="heartRate")
abline(lsfit(totChol[TenYearCHD==0],  heartRate[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(totChol[TenYearCHD==1], heartRate[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

plot(totChol,glucose, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="totChol", ylab="glucose")
abline(lsfit(totChol[TenYearCHD==0],  glucose[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(totChol[TenYearCHD==1], glucose[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

# sysBP

plot(sysBP,BMI, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="sysBP", ylab="BMI")
abline(lsfit(sysBP[TenYearCHD==0],  BMI[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(sysBP[TenYearCHD==1], BMI[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

plot(sysBP,diaBP, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="sysBP", ylab="diaBP")
abline(lsfit(sysBP[TenYearCHD==0],  diaBP[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(sysBP[TenYearCHD==1], diaBP[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

par(mfrow=c(2,2))

plot(sysBP,heartRate, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="sysBP", ylab="heartRate")
abline(lsfit(sysBP[TenYearCHD==0],  heartRate[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(sysBP[TenYearCHD==1], heartRate[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

plot(sysBP,glucose, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="sysBP", ylab="glucose")
abline(lsfit(sysBP[TenYearCHD==0],  glucose[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(sysBP[TenYearCHD==1], glucose[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

# BMI

plot(BMI,diaBP, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="BMI", ylab="diaBP")
abline(lsfit(BMI[TenYearCHD==0],  diaBP[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(BMI[TenYearCHD==1], diaBP[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")


plot(BMI,heartRate, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="BMI", ylab="heartRate")
abline(lsfit(BMI[TenYearCHD==0],  heartRate[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(BMI[TenYearCHD==1], heartRate[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

par(mfrow=c(2,2))

plot(BMI,glucose, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="BMI", ylab="glucose")
abline(lsfit(BMI[TenYearCHD==0],  glucose[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(BMI[TenYearCHD==1], glucose[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

# diaBP

plot(diaBP,heartRate, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="diaBP", ylab="heartRate")
abline(lsfit(diaBP[TenYearCHD==0],  heartRate[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(diaBP[TenYearCHD==1], heartRate[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

plot(diaBP,glucose, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="diaBP", ylab="glucose")
abline(lsfit(diaBP[TenYearCHD==0],  glucose[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(diaBP[TenYearCHD==1], glucose[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

plot(heartRate,glucose, pch=TenYearCHD+1, col=ifelse(TenYearCHD == 0, "red", "blue"), xlab="heartRate", ylab="glucose")
abline(lsfit(heartRate[TenYearCHD==0],  glucose[TenYearCHD==0]), lty=1, col="red")
abline(lsfit(heartRate[TenYearCHD==1], glucose[TenYearCHD==1]), lty=2, col="blue")
legend("topleft", legend=c("No","Yes"), pch=1:2, col=c("red", "blue"), title="TenYearCHD?")

model3<- glm(TenYearCHD ~ log(age) + as.factor(sex) + cigsPerDay+
               as.factor(BPMeds) + as.factor(prevalentStroke) + as.factor(prevalentHyp) + as.factor(diabetes)
             + totChol + sysBP + diaBP+log(diaBP) + BMI + heartRate + glucose
             +BPMeds:BMI+prevalentHyp:glucose + diabetes:sysBP+ prevalentStroke:heartRate+prevalentHyp:diaBP, family = binomial())
summary(model3)

mmps(model3)

# model comaprision

#AIC

AIC(model1, model2, model3)

#Hosmer-Lemeshow Test

library(ResourceSelection)
hoslem.test(model1$y, fitted(model1), g=10)
hoslem.test(model2$y, fitted(model2), g=10)
hoslem.test(model3$y, fitted(model3), g=10)

# difference in deviance between model

anova(model2, model3, test="Chisq")


b0 <- model3$coeff[1]
b0
b1 <- model3$coeff[2]
b1
b2 <- model3$coeff[3]
b2
b3 <- model3$coeff[4]
b3
b4 <- model3$coeff[5]
b4
b5 <- model3$coeff[6]
b5
b6 <- model3$coeff[7]
b6
b7 <- model3$coeff[8]
b7
b8 <- model3$coeff[9]
b8
b9 <- model3$coeff[10]
b9
b10 <- model3$coeff[11]
b10
b11 <- model3$coeff[12]
b11
b12 <- model3$coeff[13]
b12
b13 <- model3$coeff[14]
b13
b14 <- model3$coeff[15]
b14
b15 <-model3$coeff[16]
b15
b16<-model3$coeff[17]
b16
b17<-model3$coeff[18]
b17
b18<-model3$coeff[19]
b18
b19<-model3$coeff[20]
b19


#install.packages("arm") # load library arm that contains the function se.coef()
library(arm)

se.b0 <- se.coef(model3)[1]
se.b0
se.b1 <- se.coef(model3)[2]
se.b1
se.b2 <- se.coef(model3)[3]
se.b2
se.b3 <- se.coef(model3)[4]
se.b3
se.b4 <- se.coef(model3)[5]
se.b4
se.b5 <- se.coef(model3)[6]
se.b5
se.b6 <- se.coef(model3)[7]
se.b6
se.b7 <- se.coef(model3)[8]
se.b7
se.b8 <- se.coef(model3)[9]
se.b8
se.b9 <- se.coef(model3)[10]
se.b9
se.b10 <- se.coef(model3)[11]
se.b10
se.b11 <- se.coef(model3)[12]
se.b11
se.b12 <- se.coef(model3)[13]
se.b12
se.b13 <- se.coef(model3)[14]
se.b13
se.b14 <- se.coef(model3)[15]
se.b14
se.b15 <- se.coef(model3)[16]
se.b15
se.b16 <- se.coef(model3)[17]
se.b16
se.b17 <- se.coef(model3)[18]
se.b17
se.b18 <- se.coef(model3)[19]
se.b18
se.b19 <- se.coef(model3)[20]
se.b19


# 95% CI for beta
LB.90 <- b0 - qnorm(0.95,0,1)*se.b0
UB.90 <- b0 + qnorm(0.95,0,1)*se.b0
CI.95 <- c(LB.90, UB.90)  
CI.95

## 95% CI for odds ratio
exp(CI.95) 

# 95% CI for beta
LB.90 <- b1 - qnorm(0.95,0,1)*se.b1
UB.90 <- b1 + qnorm(0.95,0,1)*se.b1
age_CI.95 <- c(LB.90, UB.90)  
age_CI.95


## 95% CI for odds ratio
exp(age_CI.95) 

# 95% CI for beta
LB.90 <- b2 - qnorm(0.95,0,1)*se.b2
UB.90 <- b2 + qnorm(0.95,0,1)*se.b2
sex_CI.95 <- c(LB.90, UB.90)  
sex_CI.95


## 95% CI for odds ratio
exp(sex_CI.95) 

# 95% CI for beta
LB.90 <- b3 - qnorm(0.95,0,1)*se.b3
UB.90 <- b3 + qnorm(0.95,0,1)*se.b3
cigs_CI.95 <- c(LB.90, UB.90)  
cigs_CI.95


## 95% CI for odds ratio
exp(cigs_CI.95) 

# 95% CI for beta
LB.90 <- b4 - qnorm(0.95,0,1)*se.b4
UB.90 <- b4 + qnorm(0.95,0,1)*se.b4
BPMed_CI.95 <- c(LB.90, UB.90)  
BPMed_CI.95


## 95% CI for odds ratio
exp(BPMed_CI.95) 

# 95% CI for beta
LB.90 <- b5 - qnorm(0.95,0,1)*se.b5
UB.90 <- b5 + qnorm(0.95,0,1)*se.b5
stroke_CI.95 <- c(LB.90, UB.90)
stroke_CI.95


## 95% CI for odds ratio
exp(stroke_CI.95) 

# 95% CI for beta
LB.90 <- b6 - qnorm(0.95,0,1)*se.b6
UB.90 <- b6 + qnorm(0.95,0,1)*se.b6
Hyp_CI.95 <- c(LB.90, UB.90)  
Hyp_CI.95


## 95% CI for odds ratio
exp(Hyp_CI.95)

# 95% CI for beta
LB.90 <- b7 - qnorm(0.95,0,1)*se.b7
UB.90 <- b7 + qnorm(0.95,0,1)*se.b7
diabetes_CI.95 <- c(LB.90, UB.90)  
diabetes_CI.95

## 95% CI for odds ratio
exp(diabetes_CI.95)


# 95% CI for beta
LB.90 <- b8 - qnorm(0.95,0,1)*se.b8
UB.90 <- b8 + qnorm(0.95,0,1)*se.b8
totChol_CI.95 <- c(LB.90, UB.90)  
totChol_CI.95

## 95% CI for odds ratio
exp(totChol_CI.95)

# 95% CI for beta
LB.90 <- b9 - qnorm(0.95,0,1)*se.b9
UB.90 <- b9 + qnorm(0.95,0,1)*se.b9
sysBP_CI.95 <- c(LB.90, UB.90)  
sysBP_CI.95

## 95% CI for odds ratio
exp(sysBP_CI.95)

# 95% CI for beta
LB.90 <- b10 - qnorm(0.95,0,1)*se.b10
UB.90 <- b10 + qnorm(0.95,0,1)*se.b10
diaBP_CI.95 <- c(LB.90, UB.90)  
diaBP_CI.95

## 95% CI for odds ratio
exp(diaBP_CI.95)

# 95% CI for beta
LB.90 <- b11 - qnorm(0.95,0,1)*se.b11
UB.90 <- b11 + qnorm(0.95,0,1)*se.b11
logdiaBP_CI.95 <- c(LB.90, UB.90)  
logdiaBP_CI.95

## 95% CI for odds ratio
exp(logdiaBP_CI.95)

# 95% CI for beta
LB.90 <- b12 - qnorm(0.95,0,1)*se.b12
UB.90 <- b12 + qnorm(0.95,0,1)*se.b12
BMI_CI.95 <- c(LB.90, UB.90)  
BMI_CI.95

## 95% CI for odds ratio
exp(BMI_CI.95)

# 95% CI for beta
LB.90 <- b13 - qnorm(0.95,0,1)*se.b13
UB.90 <- b13 + qnorm(0.95,0,1)*se.b13
heartRate_CI.95 <- c(LB.90, UB.90)  
heartRate_CI.95

## 95% CI for odds ratio
exp(heartRate_CI.95)

# 95% CI for beta
LB.90 <- b14 - qnorm(0.95,0,1)*se.b14
UB.90 <- b14 + qnorm(0.95,0,1)*se.b14
glucose_CI.95 <- c(LB.90, UB.90)  
glucose_CI.95

## 95% CI for odds ratio
exp(glucose_CI.95)

# 95% CI for beta
LB.90 <- b15- qnorm(0.95,0,1)*se.b15
UB.90 <- b15 + qnorm(0.95,0,1)*se.b15
b15_CI.95 <- c(LB.90, UB.90)  
b15_CI.95

## 95% CI for odds ratio
exp(b15_CI.95)

# 95% CI for beta
LB.90 <- b16- qnorm(0.95,0,1)*se.b16
UB.90 <- b16 + qnorm(0.95,0,1)*se.b16
b16_CI.95 <- c(LB.90, UB.90)  
b16_CI.95

## 95% CI for odds ratio
exp(b16_CI.95)

# 95% CI for beta
LB.90 <- b17- qnorm(0.95,0,1)*se.b17
UB.90 <- b17 + qnorm(0.95,0,1)*se.b17
b17_CI.95 <- c(LB.90, UB.90)  
b17_CI.95

## 95% CI for odds ratio
exp(b17_CI.95)

# 95% CI for beta
LB.90 <- b18- qnorm(0.95,0,1)*se.b18
UB.90 <- b18 + qnorm(0.95,0,1)*se.b18
b18_CI.95 <- c(LB.90, UB.90)  
b18_CI.95

## 95% CI for odds ratio
exp(b18_CI.95)

# 95% CI for beta
LB.90 <- b19- qnorm(0.95,0,1)*se.b19
UB.90 <- b19 + qnorm(0.95,0,1)*se.b19
b19_CI.95 <- c(LB.90, UB.90)  
b19_CI.95

## 95% CI for odds ratio
exp(b19_CI.95)


# odds ratio
exp(b0)
exp(b1)
exp(b2)
exp(b3)
exp(b4)
exp(b5)
exp(b6)
exp(b7)
exp(b8)
exp(b9)
exp(b10)
exp(b11)
exp(b12)
exp(b13)
exp(b14)
exp(b15)
exp(b16)
exp(b17)
exp(b18)
exp(b19)



