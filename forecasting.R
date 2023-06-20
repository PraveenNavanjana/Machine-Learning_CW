#import libraries
library(ggpubr)
library(neuralnet)
library(readxl)
library(Metrics)

Data_load <- read_excel("C:/Users/my pc/Desktop/ML cw/uow_consumption.xlsx")
Data_load
summary(Data_load)
boxplot(Data_load[,-1]) #plot before normalizing data
#normalize function 
normalize <- function(x){
  return ((x - min(x)) / (max(x) - min(x)))
}
uow_load_norm <- as.data.frame(lapply(Data_load[-1,-1], normalize))# normalized data without 

View(uow_load_norm)
summary(uow_load_norm)
boxplot(uow_load_norm) #plot after normalization

#split data
traindata <- uow_load_norm[1:400,] #data used for training model 
testdata <- uow_load_norm[400:469,] #data used for testing model

View(traindata)
names(traindata)
testdata

################ Training Model ######################
traindata_input <- matrix( ,nrow = 0, ncol = 4) #matrix to extract input values#
traindata_output <- c() #vector to store output value#
for (i in 1:length(traindata$X0.83333333333333337)) {
  end_value <- i + 3 #find last record number for each input set#
  
  #break loop if output value is out of bounds#
  if (end_value+1 > length(traindata$X0.83333333333333337)) {
    break
  }
  
  #collect new records set to the existing records
  new_input <- traindata$X0.83333333333333337[i:end_value] #store 1st 4 records of iteration as new input set
  new_output <- traindata$X0.83333333333333337[end_value+1] #store 5th record as output value
  
  traindata_input <- rbind(traindata_input, new_input) #add the new input vector to inputs matrix as a row
  traindata_output <- append(traindata_output, new_output) #add new output value to output vector
}

#create a data frame with all inputs and outputs
traindata_input_output_df <- cbind(as.data.frame(traindata_input), traindata_output)

#neural network training model with 2 hidden layers containing 10 neurons and 7 neurons 
order4_nn <- neuralnet(traindata_output ~ V1 + V2 + V3 + V4, 
                       data = traindata_input_output_df, hidden=c(5), linear.output=TRUE)
print(order4_nn)
plot(order4_nn)

################### Testing Model ######################
testdata_input <- matrix( ,nrow = 0, ncol = 4) 
testdata_output <- c()
for (i in 1:length(testdata$X0.83333333333333337)) {
  end_value <- i + 3 
  
  if (end_value+1 > length(testdata$X0.83333333333333337)) {
    break
  }
  
  
  new_input <- testdata$X0.83333333333333337[i:end_value] 
  new_output <- testdata$X0.83333333333333337[end_value+1] 
  
  testdata_input <- rbind(testdata_input, new_input)
  testdata_output <- append(testdata_output, new_output)
}

testdata_input
#make test data a data frame
testdata_df <- as.data.frame(testdata_input)
#test the neural network with test data
order4_nn_results <- compute(order4_nn, testdata_df)
results <- data.frame(actual = testdata_output, prediction = order4_nn_results$net.result) 

results

names(Data_load)

results_min <- min(Data_load$"0.83333333333333337")
results_min
results_max <- max(Data_load$"0.83333333333333337")
#calculating accuracy 
predicted = results$prediction * abs(diff(range(testdata_output))) + min(testdata_output)
actual = results$actual * abs(diff(range(testdata_output))) + min(testdata_output)

#function for un-normalizing data
unnormalize <- function(x, min, max) {
  return( (max - min)*x + min )
}
comparison = data.frame(predicted,actual)
comparison
unnormalizeed_results <- unnormalize(comparison, results_min, results_max)

unnormalizeed_results

deviation = ((unnormalizeed_results$actual-unnormalizeed_results$predicted)/unnormalizeed_results$actual)

deviation

comparison=data.frame(unnormalizeed_results$predicted,unnormalizeed_results$actual,deviation)

comparison

accuracy = 1 - abs(mean(deviation))
accuracy
#get RMSE value
pred_RMSE <- rmse(unnormalizeed_results$actual, unnormalizeed_results$predicted)
pred_RMSE
pred_mae <- mae(unnormalizeed_results$actual, unnormalizeed_results$predicted)
pred_mae
pred_mape <- mape(unnormalizeed_results$actual, unnormalizeed_results$predicted)
pred_mape
pred_smape <- smape(unnormalizeed_results$actual, unnormalizeed_results$predicted)
pred_smape









