
#HANDLE MISSING VALUES 
#THERE ARE VARIOUS WAY TO  HANDLE MISSING VALUES :
#1.DELETEING
#2.IMPUTATING MISSING VALUES WITH MEAN MODE MEDIAN
#3.KNN IMPUTATION
#4.SIMPLE LINEAR REGRESSION

#1.DELETEING OBSERVATIONS
#if data is very large in size and where all the classess predicted
#are very large enough those dataset with require deletion of missing
#values 

#2.Inputating mean ,median mode
data<-c(21,23,23,43,54,NA,09,78,NA,66)
mean(data)#ouput NA
median(data)#output NA
mode(data)#numeric 

#if there is less variation betwen the value of dataset then u can go for mean ,median and mode imputation
is.na(data)
data[is.na(data)]
#ouput NA,NA

#mean inputation
mean(data[!is.na(data)])
#39.625
data[is.na(data)]<-mean(data[!is.na(data)])
data

#median imputation
data1<-c(23,24,25,26,27,28,NA,12,NA,NA)
data1[is.na(data1)]
data1[is.na(data1)]<-median(data1[!is.na(data1)])
data1

#using regression
x<-c(23,23,23,45,67,21,97,54,35)
y<-c(24,23,53,45,67,21,97,54,NA)
z<-c(23,23,63,45,67,21,NA,54,35)
Dataregression<-data.frame(x,y,z)
Dataregression
Dataregression[is.na(Dataregression)]
which(is.na(Dataregression))
cor(Dataregression)
cor(Dataregression,use="complete.obs")
symnum(cor(Dataregression,use="complete.obs"))      

#For that let us create a function that will make our task easier
#The following function takes a vector as an argument and returns a binary vector of 0 corresponding the missing value in the argument vector and 1 otherwise.
missdummy<-function(t){
  x<-dim(length(t))
  x[which(is.na(t))]=1
  x[which(!is.na(t))]=0
  return(x)
}

#Now we will use this function to create a dummy variable that will indicate missing value using 0, otherwise willtake the value 1.

#Now we will use this function to create a dummy variable that will indicate missing value using 0, otherwise willtake the value 1.
s<-c('s')
Dataregression[,s]<-NA
Dataregression
Dataregression$dummy <- missdummy(Dataregression$y)
Dataregression


#Next, let us fit a linear model with y as dependent variable and x as independent variable.
lm(y~x,Dataregression)
for(i in 1:nrow(Dataregression)){
  if(Dataregression$dummy[i]==1)
{ Dataregression$y[i]=9.3365+0.8762*Dataregression$x[i]}
}

Dataregression


