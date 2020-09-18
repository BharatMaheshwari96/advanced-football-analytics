data<-read.csv('http://users.stat.ufl.edu/~winner/data/nfl2008_fga.csv') 


#modeling HOMEKICK to TOGO YDLINE KICKER


y=data$homekick   

x1=data$togo   

x2=data$ydline 

x3=data$kicker 

df=data.frame( x1,x2,x3,y) 



# data cleaning: removing missing values, removing outliers,... 

DATA <- na.omit(df) # remove missing values 

set.seed(145) 

####split dataset 

n=nrow(DATA) 

indexes = sample(n,n*(80/100)) 

trainset = DATA[indexes,] #<- train set 

testset = DATA[-indexes,] #<- test set 

# Fit the full model 

actual=testset$y 

full.model <- glm(trainset$y ~., data = trainset, family='gaussian') #<- predicting test set using train model

yhat=predict( full.model, testset) 
yhat
###########________RMSE________###########
rmse1=sqrt((sum(yhat -actual)^2)/(nrow(testset))) 
rmse1



phat_i=predict(full.model,testset, type="response") 
phat_i
predictedvalues=rep(0,length(phat_i)) 
predictedvalues
predictedvalues[phat_i>0.5]=1  

actual=testset$y

df=data.frame(actual,predictedvalues) 
df
################Confusion Matrix#######################
confusion_matrix=table( predictedvalues, actualvalues=actual) #confusion matrix 
confusion_matrix
accuracy=mean(predictedvalues == actual) # accuary  
accuracy