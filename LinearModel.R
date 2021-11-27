# clears all objects in "global environment"
#rm(list=ls())
# clears the console area
cat("\014")
# Starts random numbers at the same sequence
set.seed(123)
# prints the current working directory
print(paste("WORKING DIRECTORY: ",getwd()))

myLibraries<-c("scatterplot3d","caret")
library(pacman)
pacman::p_load(char=myLibraries,install=TRUE,character.only=TRUE)



#Change sorce of 'lab2functions.R'
source("C:\\Users\\Devang\\Desktop\\Data Science Sem 1\\Business Analytics\\Lab\\Lab 2\\lab2functions.R")



# Change sorce of CSV file called "Cars.csv"
Cars<-read.csv("C:\\Users\\Devang\\Desktop\\Data Science Sem 1\\Business Analytics\\Lab\\Lab 2\\Cars.csv")
library(caret)

#define one-hot encoding function
dummy <- dummyVars(" ~ .", data=Cars)
#perform one-hot encoding on data frame
Cars <- data.frame(predict(dummy, newdata=Cars))
#view final data frame
print(head(Cars))
print("Dataset is a in frame called Cars")


#Boston<-Boston[order(runif(nrow(Boston))),]
#Or using the sample() function
Cars<-Cars[sample(1:nrow(Cars)),]
print(head(Cars))

# Create a TRAINING dataset using first 70% of the records
# and the remaining 30% is used as TEST
# use ALL fields (columns)
training_records<-round(nrow(Cars)*(70/100))
training_data <- Cars[1:training_records,]
testing_data = Cars[-(1:training_records),]

print(training_records)
print(head(training_data))
print(head(testing_data))
# ************************************************
#Continuing simple linear regression
#Visualise the dataset

# pairs(training_data[,c("year","mileage","tax","mpg","engineSize")])

# ************************************************
# OUTPUT : List - Metrics of $MAE, $RMSE and $R2
# ************************************************
NscatterPlotError<-function(datasetTrain, datasetTest, outputName,predictorName){
  #print("Inside NscatterPlotError")
  formular<-paste(outputName,"~",predictorName)
  
  linearModel<-lm(formula=formular,data=datasetTrain)
  #print(linearModel)
  
  predictorInput <- subset(datasetTest, select=predictorName)
  #print(predictorInput)
  
  y_predicted <- predict(linearModel, predictorInput)
  #print("=======y_predicted======")
  #print(summary(linearModel))
  y_actual <- datasetTest[, outputName]
  #print("======y_actual======")
  #print(y_actual)
  
  RMSE <- round(sqrt(mean(y_actual - y_predicted)^2),digits=2)
  print(RMSE)
  MAE <- round(mean(abs(y_actual - y_predicted)))
  print(MAE)
  r2 <- round(summary(linearModel)$adj.r.squared)
  print(r2)
  
  
  
  
  error <- y_actual-y_predicted
  #print(error)
  
  
  results<-data.frame(predictorInput,y_actual,error)
  #print("============results==========")
  #print(results)
  
  
  # order from lowest to highest the valexpected values
  # for ease of visulisation
  results <- results[order(y_actual), ]
  #print(results)
  
  
  plot(results[,predictorName],
       results$y_actual,
       pch=4,
       ylab=outputName,
       xlab=predictorName,
       main="Linear Regression Errors",
       sub=paste("MAE=",MAE,"RMSE=",RMSE," R2=",r2))
  
  #Plot the linear model as a BLUE straight line
  abline(linearModel,col = "blue", lwd=3)
  
  # highlighting the RED error magnitude lines
  suppressWarnings(arrows(results[,predictorName],
                          results$y_actual,
                          results[,predictorName],
                          results$y_actual-results$error,
                          length=0.05,angle=90,code=3,col="red"))
  
  return(list(MAE=MAE, RMSE= RMSE,r2=r2))
}

results<-NscatterPlotError(datasetTrain=training_data,
                           datasetTest=testing_data,
                           outputName="price",
                           predictorName="tax")




#print(results)




# ========================================================
# ========== Multiple Linear Regression Model ============
# ========================================================

linearModelTransform2Inputs<-lm(price~log(tax)+mpg,data=training_data)

r2<-round(summary(linearModelTransform2Inputs)$adj.r.squared, digits=2)
print(paste("r^2 with log(tax) and mpg added=",r2))



x<-Cars[,"tax"]
y<-Cars[,"price"]
z<-Cars[,"mpg"]
library(scatterplot3d)
scatterplot3d(x,y,z, pch=16,highlight.3d = TRUE)





NscatterPlotMultiLinearRegression(datasetTrain = training_data,
                                  datasetTest = testing_data,
                                  outputName = "price",
                                  predictorName1 = "tax",
                                  predictorName2 = "mileage")

NscatterPlotMultiLinearRegression(datasetTrain = training_data,
                                  datasetTest = testing_data,
                                  outputName = "price",
                                  predictorName1 = "year",
                                  predictorName2 = "mpg")




# =========== All inputs now ! =======

#linearModelTransformAllInputs<-lm(price~.-tax+log(tax),data=training_data)

#r2<-round(summary(linearModelTransformAllInputs)$adj.r.squared, digits=2)
#print(paste("Calculated r^2 with all variables added=",r2))

