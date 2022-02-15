#####################################################################
#
# Classification of ECG data using ANN
#
# Version 0.1
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
srcfile <- srcfile1
traindata <- read.csv(paste0(srcdir, srcfile),header = F)
srcfile <- srcfile2
testdata <- read.csv(paste0(srcdir, srcfile),header = F)

#some info from training data
unique(traindata[,188])
sum(traindata[,188] == 0)
sum(traindata[,188] == 1)
sum(traindata[,188] == 2)
sum(traindata[,188] == 3)
sum(traindata[,188] == 4)


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




