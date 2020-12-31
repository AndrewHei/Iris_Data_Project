library(datasets)
data(iris)
summary(iris)
names(iris)
library(dplyr)
virginica<-filter(iris,Species=='virginica')
head(virginica)
sepalLength6<- filter(iris, Species=='virginica',Sepal.Length>6)
tail(sepalLength6)
selected <- select(iris,Sepal.Length, Sepal.Width, Petal.Length)
selected2 <- select(iris,Sepal.Length:Petal.Length)
head(selected,3)
identical(selected,selected2)
newCol <- mutate(iris,greater.half=Sepal.Width>0.5*Sepal.Length)
tail(newCol)
sum(newCol$greater.half,TRUE)
arr.vig<-newCol %>% filter(Species=='virginica')%>% arrange(Sepal.Width)
head(arr.vig)
arr.vig[30:35,]
summarise(arr.vig,mean.length=mean(Sepal.Length,na.rm=TRUE))
summarise(arr.vig,sd.length=sd(Sepal.Length,na.rm=TRUE))
plot(iris)
plot(iris$Sepal.Width,iris$Sepal.Length)
hist(iris$Sepal.Width)
#Machine Learning Part:
library(caret)
  #create a list of 80% of the rows in the original dataset for training
validation_index<- createDataPartition(iris$Species, p=0.80,list=FALSE)
  #select 20% of the data for validation
validation<-iris[-validation_index,]
  #Use 80% of data to train and test models
iris2<-iris[validation_index,]
  #check dimensions
dim(iris2)
  #check attribute
sapply(iris2,class)
head(iris2)
  #list level for the class
levels(iris2$Species)
  #Summarize the class distribution
percentage<-prop.table(table(iris2$Species))*100
cbind(freq=table(iris2$Species),percentage=percentage)
summary(iris2)
  #split input and output
x<-iris2[,1:4]
y<-iris2[,5]
  #boxplot for each attribute on one image
par(mfrow=c(1,4))
  for(i in 1:4){
    boxplot(x[,i],main=names(iris)[i])}
  #barplot for class breakdown
plot(y)
  #scatterplot matrix
library(ellipse)
featurePlot(x=x,y=y,plot="ellipse")
featurePlot(x=x,y=y,plot='box')
  #density plots for each attribute by class value
scales<-list(x=list(relation="free"),y=list(relation="free"))
featurePlot(x=x,y=y,plot="density",scales=scales)
  #Run algorithms using 10-fold cross validation
control <- trainControl(method="cv",number=10)
metric<-"Accuracy"
#Build models
  #Linear algorithms
set.seed(7)
fit.lda<-train(Species~.,data=iris2,method="lda",metric=metric,trControl=control)
  #Non-linear algorithms
    #CART
set.seed(7)
fit.cart<-train(Species~.,data=iris2,method="rpart",metric=metric,trCOntrol=control)
    #kNN
set.seed(7)
fit.knn<-train(Species~.,data=iris2,method="knn",metric=metric,trControl=control)
  #Advanced algorithms
    #SVM
set.seed(7)
fit.svm<-train(Species~., data=iris2, method="svmRadial",metric=metric, trControl=control)
    #Ramdom forest
set.seed(7)
fit.rf<-train(Species~.,data=iris2,method="rf",metric=metric, trControl=control)
#summarize accuracy
results <- resamples(list(lda=fit.lda, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)
#summarize best model
print(fit.lda)
#estimate skill of LDA on the validation dataset
predictions<-predict(fit.lda,validation)
confusionMatrix(predictions,validation$Species)
