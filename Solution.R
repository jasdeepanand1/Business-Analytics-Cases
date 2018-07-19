getwd()
setwd("G:/BA/AnalyticsVidhya/")
loan=read.csv("train_u6lujuX_CVtuZ9i.csv")
loan$LoanAmount[is.na(loan$LoanAmount)]=128
str(loan)
quantile(loan$LoanAmount,seq(0,1,0.02))
loan$LoanAmount=ifelse(loan$LoanAmount>275,275,loan$LoanAmount)
loan$Loan_Amount_Term[is.na(loan$Loan_Amount_Term)]=360   
which(is.na(loan$LoanAmount))
loan$Gender[which(loan$Gender=="")]="Male"
loan$Gender_M=ifelse(loan$Gender=="Male",1,0)
loan$Gender_M
loan=loan[-2]
loan$Married_Y=ifelse(loan$Married=="Yes",1,0)
loan=loan[-2]
loan$Property_Area_R=ifelse(loan$Property_Area=="Rural",1,0)
loan$Property_Area_U=ifelse(loan$Property_Area=="Urban",1,0)
loan=loan[-10]
loan$Education_G=ifelse(loan$Education=="Graduate",1,0)
loan=loan[-3]
loan$Self_Employed[which(loan$Self_Employed=="")]="No"
loan$Self_Employed_Y=ifelse(loan$Self_Employed=="Yes",1,0)
loan1=loan
str(loan1)
loan1=loan1[-3]
loan1$Credit_History[which(is.na(loan1$Credit_History))]=1
loan1$Dependents[which(loan1$Dependents=="")]="0"
loan1$Dependents_1=ifelse(loan1$Dependents=="1",1,0)
loan1$Dependents_2=ifelse(loan1$Dependents=="2",1,0)
loan1$Dependents_0=ifelse(loan1$Dependents=="0",1,0)
loan1=loan1[-2]
loan1$Loan_Status=as.character(loan1$Loan_Status)
loan1$Loan_Status=ifelse(loan1$Loan_Status=="Y",1,0)
loan1=loan1[-1]
mod12=lm(Loan_Status ~ ., data = loan1)
library(car)
p=vif(mod)
sort(p,decreasing = T)
testdata=loan1
t_model=glm(as.factor(Loan_Status)~.,family = "binomial", data = testdata )
summary(t_model)
stpmod12 <- step(t_model, direction = "both")
formula(stpmod12)
summary(stpmod12)
t1_model=glm(as.factor(Loan_Status) ~ Dependents_1+Education_G+Credit_History+Married_Y+Property_Area_R+Property_Area_U,family = "binomial", data = testdata)
summary(t1_model)
t1_model$score<-predict(t1_model,newdata = testdata, type = "response")
head(t1_model$score)
library(lattice)
library(ggplot2)
library(caret)
library(e1071)
prediction2 <- ifelse(t1_model$score >= 0.6,1,0)
caret::confusionMatrix(factor(prediction2), factor(testdata$Loan_Status))
test123=head(testdata)
head(test123)
test123$score<-predict(t1_model, newdata = test123, type = "response")


loan_1=read.csv("test_Y3wMUE5_7gLdaTN.csv")
loan_1$Property_Area_R=ifelse(loan_1$Property_Area=="Rural",1,0)
loan_1$Property_Area_U=ifelse(loan_1$Property_Area=="Urban",1,0)
loan_1=loan_1[-12]
table(loan_1$Credit_History)
summary(loan_1$Credit_History)
loan_1$Credit_History[is.na(loan_1$Credit_History)]=1
summary(loan_1$Loan_Amount_Term)
table(loan_1$Loan_Amount_Term)
loan_1$Loan_Amount_Term[is.na(loan_1$Loan_Amount_Term)]=360
summary(loan_1$LoanAmount)
boxplot(loan_1$LoanAmount)
hist(loan_1$LoanAmount)
loan_1$LoanAmount[is.na(loan_1$LoanAmount)]=125
quantile(loan_1$LoanAmount,seq(0,1,0.02))
loan_1$LoanAmount=ifelse(loan_1$LoanAmount>225,225,loan_1$LoanAmount)
summary(loan_1$ApplicantIncome)
boxplot(loan_1$ApplicantIncome)
table(loan_1$Self_Employed)
loan_1$Self_Employed=as.character(loan_1$Self_Employed)
class(loan_1$Self_Employed)
loan_1$Self_Employed=ifelse(loan_1$Self_Employed=="","No",loan_1$Self_Employed)
loan_1$Self_Employed_Y=ifelse(loan_1$Self_Employed=="Yes",1,0)
str(loan_1)
str(loan1)
loan_1=loan_1[-6]
table(loan_1$Education)
loan_1$Education_G=ifelse(loan_1$Education=="Graduate",1,0)
loan_1=loan_1[-5]
loan_1$Married_Y=ifelse(loan_1$Married=="Yes",1,0)
loan_1=loan_1[-3]
table(loan_1$Gender)
loan_1$Gender=as.character(loan_1$Gender)
loan_1$Gender=ifelse(loan_1$Gender=="","Male",loan_1$Gender)
loan_1$Gender_M=ifelse(loan_1$Gender=="Male",1,0)
loan_1=loan_1[-2]
table(loan_1$Dependents)
loan_1$Dependents=as.character(loan_1$Dependents)
loan_1$Dependents=ifelse(loan_1$Dependents=="",0,loan_1$Dependents)
loan_1$Dependents_0=ifelse(loan_1$Dependents==0,1,0)
loan_1$Dependents_1=ifelse(loan_1$Dependents=="1",1,0)
head(loan_1$Dependents_0)
head(loan_1$Dependents_2)
loan_1$Dependents_2=ifelse(loan_1$Dependents=="2",1,0)
str(loan_1)
loan_1=loan_1[-2]
str(loan_1)
final=predict(t1_model,newdata = loan_1,type = "response")
table(final)
final=ifelse(final>0.6,"Y","N")
final
write.csv(final,file = "new.csv")
dir()
