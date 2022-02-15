#####################################################################
#
# Classification of ECG data using ANN
#
# Version 0.2: shuffled training data, multi-class classification, confusion matrix
#
#####################################################################

library(keras)
#install_keras() #only first time

#data folder (relative to current)
srcdir <- "C:/Users/MEHAK BAL/Downloads/Mehak/R/"

#data files
srcfile1 <- "mitbih_test.csv"
srcfile2 <- "mitbih_train.csv"
#srcfile3 <- "ptbdb_abnormal.csv"
#srcfile4 <- "ptbdb_normal.csv"

#read data
set.seed(2020)
srcfile <- srcfile2
traindata <- read.csv(paste0(srcdir, srcfile),header = F)
traindata <- traindata[sample(nrow(traindata)), ]
srcfile <- srcfile1
testdata <- read.csv(paste0(srcdir, srcfile),header = F)

#some info from training data
unique(traindata[,188])
sum(traindata[,188] == 0)
sum(traindata[,188] == 1)
sum(traindata[,188] == 2)
sum(traindata[,188] == 3)
sum(traindata[,188] == 4)

C0 = which(traindata[,188]==0)
C1 = which(traindata[,188]==1)
C2 = which(traindata[,188]==2)
C3 = which(traindata[,188]==3)
C4 = which(traindata[,188]==4)

#plot data, if needed
#color <- "red"
#plot(x = t,y = amplitude)
len <- ncol(traindata) - 1
t <- seq(from = 0, length.out = len, by = 8/1000)

plot(x = t, y = traindata[C0[1],1:len], col = "red", type = "o", 
     main = "One ECG beat for every Hearbeat class", xlab = "time [ms]", ylab = "amplitude")
lines(x = t, y = traindata[C1[1],1:len], col = "yellow", type = "o", main = srcfile)
lines(x = t, y = traindata[C2[1],1:len], col = "black", type = "o", main = srcfile)
lines(x = t, y = traindata[C3[1],1:len], col = "green", type = "o", main = srcfile)
lines(x = t, y = traindata[C4[1],1:len], col = "blue", type = "o", main = srcfile)
legend("topright", legend = c("Category'0'= N", "Category'1'= S", "Category'2'= V","Category'3'= F","Category'4'= Q"), lty = c(1,1,1,1,1), col = c("red", "yellow", "black","green","blue"))


#plots for means
plot(x = t, y = colMeans(traindata[C0,1:len]), col = "red", type = "o", 
     main = "Mean of Hearbeat classes", xlab = "time [ms]", ylab = "amplitude")
lines(x = t, y = colMeans(traindata[C1,1:len]), col = "yellow", type = "o", main = srcfile)
lines(x = t, y = colMeans(traindata[C2,1:len]), col = "black", type = "o", main = srcfile)
lines(x = t, y = colMeans(traindata[C3,1:len]), col = "green", type = "o", main = srcfile)
lines(x = t, y = colMeans(traindata[C4,1:len]), col = "blue", type = "o", main = srcfile)
legend("topright", legend = c("Category'0'= N", "Category'1'= S", "Category'2'= V","Category'3'= F","Category'4'= Q"), pch = "ooooo", col = c("red", "yellow", "black","green","blue"))

hist(colMeans(traindata[C0,1:len]))
hist(colMeans(traindata[C1,1:len]))
hist(colMeans(traindata[C2,1:len]))
hist(colMeans(traindata[C3,1:len]))
hist(colMeans(traindata[C4,1:len]))

#retrieve inputs/outputs from data
x_train <- as.matrix(traindata[,1:187])
y_train <- to_categorical(traindata[,188], 5)


#Test: retrieve inputs/outputs from data
x_test <- as.matrix(testdata[,1:187])
y_test <- to_categorical(testdata[,188], 5)



#create the model
model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 256, activation = 'relu', input_shape = c(187)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 5, activation = 'softmax')
#model %>% 
#  layer_dense(units = 64, activation = 'relu', input_shape = c(187)) %>% 
#  layer_dropout(rate = 0.4) %>% 
#  layer_dense(units = 32, activation = 'relu') %>%
#  layer_dropout(rate = 0.3) %>%
#  layer_dense(units = 5, activation = 'softmax')



#print some model info (if wanted)
summary(model)


#assemble the model
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)


#train
model.info <- model %>% fit(
  x_train, y_train, 
  epochs = 30, batch_size = 128, 
  validation_split = 0.2
)

#plot the model info
plot(model.info)
model %>% evaluate(x_test, y_test)

#test on test data
test.results <- model %>% predict_classes(x_test)
#create and show the confusion matrix
library('caret')
confusionMatrix(factor(test.results), factor(testdata[,188]))



#some nice plots with t-SNE
library(Rtsne)

per <- .4
N <- nrow(testdata)
tsnedata <- testdata[sample(N, as.integer(per * N)),]

Labels<-tsnedata[,188]
tsnedata[,188]<-as.factor(tsnedata[,188])
## for plotting
colors = rainbow(length(unique(tsnedata[,188])))
names(colors) = unique(tsnedata[,188])

## Executing the algorithm on curated data
tsne <- Rtsne(tsnedata[,-188], dims = 2, perplexity=30, verbose=TRUE, max_iter = 500)
exeTimeTsne<- system.time(Rtsne(tsnedata[,-188], dims = 2, perplexity=30, verbose=TRUE, max_iter = 500))

## Plotting
plot(tsne$Y, t='n', main="Samples from MIT-BIH for ECG beat classification",xlab = "time [ms]", ylab = "amplitude")
text(tsne$Y, labels=tsnedata[,188], col=colors[tsnedata[,188]])

