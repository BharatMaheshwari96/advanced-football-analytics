library(ggplot2)
library(factoextra)
library(GGally)
library(CCA)
library(MASS)

######################## first part-> using LDA Model ########################

data= read.csv('http://users.stat.ufl.edu/~winner/data/nfl2008_fga.csv') 
#attach(data) 

lda.model = lda (factor(qtr)~ togo+kicker+ydline, data=data)
lda.model


####################### Second Part-> Applying PCA and ploting graph ####################

data= read.csv('http://users.stat.ufl.edu/~winner/data/nfl2008_fga.csv')
data<-na.omit(data)
data.active <- data[(1:1039),(10:12)]
head(data.active)
res.pca <- prcomp(na.omit(data.active, scale = TRUE))
fviz_screeplot(res.pca)

######################  third part-> Canonical Correlation Analysis ######################
library(ggplot2)
library(factoextra)
library(GGally)
library(CCA)
data= read.csv('http://users.stat.ufl.edu/~winner/data/nfl2008_fga.csv') 
data <- na.omit(data)
X<- data[1:1039,10:12]
Y <- data[1:1039,14:15]
cancor(na.omit(X),na.omit(Y) )
correl <- matcor(na.omit(X),na.omit(Y) )
img.matcor(correl, type = 2)
cc1 <- cancor(na.omit(X),na.omit(Y))  ### function from standard R instalation
#cc2 <- cc(na.omit(X),na.omit(Y))
cc1$cor
cc2$cor
cc1$xcoef
cc2$xcoef
ggpairs(data,12)
X<- as.matrix(data$ydline)
Y<- as.matrix(data$distance)
matcor(X,Y)
#cc1 <- cc(X,Y)
cc1$cor

######################### fourth part -> K-means clustering analysis ###################

DATA_1 <- read.csv('http://users.stat.ufl.edu/~winner/data/nfl2008_fga.csv')
DATA_1 <-na.omit(DATA_1)
head(DATA_1)
DATA_1.stand <- scale(DATA_1[,12])
k.means.fit <- kmeans(DATA_1.stand, 3)
attributes(k.means.fit)
k.means.fit$centers
k.means.fit$cluster
k.means.fit$size

wssplot<- function(DATA_1, nc = 15 , seed = 1234){
  wss <- (nrow(DATA_1)-1)*sum(apply(DATA_1,2,var))
  for(i in 2:nc){
    set.seed(seed)
    wss[i] <-sum(kmeans(DATA_1, centers=i)$withinss)
  }
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}

wssplot(DATA_1.stand, nc=6)
