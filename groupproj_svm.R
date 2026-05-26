#MATH 4323 Project
#download data "Breast Cancer Wisconsin Data Set"
install.packages("readr")
library(readr)
data= read_csv("C:/Users/citla/Downloads/data.csv", col_names = TRUE)
View(data)

#only use the mean variables!!!
#data with M or B classifier
new_data = subset(data, select= c(2:12))
View(new_data)
attach(new_data)
library(e1071)
scale=TRUE
#--------------------------------------------------
#SVM
#Split Data 80/20 Validation Set
set.seed(1)
n = nrow(new_data)
train = sample(1:n, 0.8*n)


#Linear Kernel with 80/20 Validation Set
svm.obj = svm(as.factor(diagnosis) ~.,
              data= new_data,
              kernel='linear',
              subset= train)
train.pred=predict(svm.obj, newdata= new_data[train,])
print(mean(train.pred !=new_data$diagnosis[train]))
print(table(pred = train.pred, true= new_data$diagnosis[train]))

#Select optimal cost and gamma values using tune function on train data
#train on 80%
set.seed
tune.out=tune(svm,
              as.factor(diagnosis)~., data=new_data[train,],
              kernel= "radial",
              ranges=list(cost=c(0.1,1,5,10,100),
                          gamma=c(0.001,0.01,0.1,1,2)))
summary(tune.out)
#gamma=0.1 with cost= 1,5,10 and gamma=0.01 with cost= 100 have lowest same error rates= 0.048


#Radial Kernel with 80/20 Validation Set
#Train on 80%
set.seed(1)
for (j in 3:1){
  svm.obj = svm(as.factor(diagnosis) ~., 
                data=new_data, 
                cost=5,
                kernel='radial', 
                gamma=10^{-j},
                subset=train)
  train.pred=predict(svm.obj, newdata= new_data[train,])
  print(mean(train.pred !=new_data$diagnosis[train]))
  print(table(pred = train.pred, true= new_data$diagnosis[train]))
}


#Test on the 20% using cost=5 and gamma= 0.1
set.seed(1)
svm.obj = svm(as.factor(diagnosis) ~., 
              data=new_data, 
              cost=5,
              kernel='radial', 
              gamma=0.1,
              subset=train)
svm.pred= predict(svm.obj, new_data[-train,])
table(pred = svm.pred, true= new_data$diagnosis[-train])
mean(svm.pred != new_data$diagnosis[-train])



#Apply to full dataset 
set.seed(1)
svm.final= svm(as.factor(diagnosis)~.,
               data=new_data,
               kernel="radial",
               cost=5,
               gamma=0.1)
summary(svm.final)
svm.final.pred = predict(svm.final, newdata = new_data)
table(pred = svm.final.pred,true = new_data$diagnosis)
mean(svm.final.pred != new_data$diagnosis)

#Plot fitted boundary with respect to some pair of predictors
#Use radius and texture
set.seed(1)
svm.plot = svm(as.factor(diagnosis) ~ radius_mean + texture_mean,
               data = new_data,
               kernel = "radial",
               cost = 5,
               gamma = 0.1)
summary(svm.plot)
plot(svm.plot,
     new_data[, c("radius_mean", "texture_mean", "diagnosis")])
svm.plot.pred = predict(svm.plot, newdata = new_data)
table(pred = svm.plot.pred,true = new_data$diagnosis)
mean(svm.plot.pred != new_data$diagnosis)
