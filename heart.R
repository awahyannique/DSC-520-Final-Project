heart<-read.csv("heart.csv")

library(knitr)
library(readr)
library(dplyr)
library(ggplot2)
library(mlr)
library(cowplot)
library(tidyverse)
library(corrplot)
library(qgraph)
library(jtools)
library(caret)
library(DataExplorer)
library(funModeling)
library(ggm)

tbl_df(heart)
str(heart)

corr <- cor(heart)
par(oma=c(0,0,0,0))
corrplot(corr,method = "number",type="lower",order = "hclust")


heart$sex <- ifelse(heart$sex==1,"male","female")
heart$cp  <- ifelse(heart$cp==1,"typical angina",ifelse(heart$cp==2, 
                                                        "atypical angina",ifelse(heart==3, "non-anginal pain","asymptomatic")))
heart$restecg <- ifelse(heart$restecg==0, "normal", ifelse(heart$trestbps==1,
                                                           "ST-T abnormality",      "hypertrophy"))

heart$exang <- ifelse(heart$exang==1,"yes","no")
heart$slope <- ifelse(heart$slope==1,"upsloping", ifelse(heart$slope== 2,
                                                         "flat","downsloping"))
heart$thal <- ifelse(heart$thal==3,"normal", ifelse(heart$thal==6,"fixed defect",
                                                    "reversable defect"))
heart$fbs <- ifelse(heart$fbs==1,"true","false") # fasting or not
heart$target <- ifelse(heart$target==0,"typical","worse")


str(heart)

df_status(heart)
introduce(heart)

plot_intro(heart)
plot_correlation(heart[,-14], type = "c")


# Plot bar charts with `price` feature
plot_bar(heart[,-14],binary_as_factor = TRUE, ggtheme = theme_get()   , title="Barplots for each variable")

boxplot(heart)
names(heart)
hist(heart$age, main="Age", col="blue")
hist(heart$sex, main="Sex", col="red")
hist(heart$cp, main="cp", col="orange")
hist(heart$trestbps, main="trestbps", col="blue")
hist(heart$chol, main="chol", col="red")
hist(heart$fbs, main="fbs", col="red")
hist(heart$restecg, main="restecg", col="red")
hist(heart$thalach, main="thalach", col="green")
hist(heart$exang, main="exang", col="green")    
hist(heart$oldpeak, main="oldpeak", col="blue") 
hist(heart$slope, main="slope", col="blue") 
hist(heart$ca, main="ca", col="blue") 
hist(heart$thal, main="thal", col="blue") 
hist(heart$target, main="target", col="chartreuse") 


par(mfrow=c(2,2))
hist(heart$trestbps, col = "skyblue", xlab = "Blood Pressure", main = 
       "Histogram of trestbps: resting blood pressure")
hist(heart$age, col = "lightblue", xlab = "Age", main = 
       "Histogram of age distribution")
hist(heart$chol, col = "lightgreen", xlab = "Cholesterol", main = 
       "Histogram of Cholesterol distribution")
hist(heart$thalach, col = "green", xlab = "Heart rate", main = 
       "maximum heart rate achieved")





