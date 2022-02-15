#####################################################################
#
# Classification of ECG data using ANN
#
# Version 0.4: shuffled training data, binary classification, confusion matrix
#
#####################################################################

library(keras)
#install_keras() #only first time

#data folder (relative to current)
srcdir <- "C:/Users/MEHAK BAL/Downloads/Mehak/R/"

#data files
#srcfile1 <- "mitbih_test.csv"
#srcfile2 <- "mitbih_train.csv"
srcfile3 <- "ptbdb_abnormal.csv"
srcfile4 <- "ptbdb_normal.csv"

#read data
srcfile <- srcfile3
adata <- read.csv(paste0(srcdir, srcfile),header = F)
srcfile <- srcfile4
ndata <- read.csv(paste0(srcdir, srcfile),header = F)

set.seed(2020)
NDTRAINING <- sample(0.8*nrow(ndata)) #normal data sample for training
ADTRAINING <- sample(0.8*nrow(adata)) #abnormal data sample for training


traindata <- rbind(ndata[NDTRAINING,], adata[ADTRAINING,])
traindata <- traindata[sample(nrow(traindata)), ]

testdata <- rbind(ndata[-NDTRAINING,], adata[-ADTRAINING,])

#some info from training data
unique(traindata[,188])
sum(traindata[,188] == 0)
sum(traindata[,188] == 1)
sum(testdata[,188] == 0)
sum(testdata[,188] == 1)


#retrieve inputs/outputs from data
x_train <- as.matrix(traindata[,1:187])
y_train <- to_categorical(traindata[,188], 2)


#Test: retrieve inputs/outputs from data
x_test <- as.matrix(testdata[,1:187])
y_test <- to_categorical(testdata[,188], 2)



#create the model
model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 256, activation = 'relu', input_shape = c(187)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 2, activation = 'softmax')
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
plot(tsne$Y, t='n', main="tsne")
text(tsne$Y, labels=tsnedata[,188], col=colors[tsnedata[,188]])

