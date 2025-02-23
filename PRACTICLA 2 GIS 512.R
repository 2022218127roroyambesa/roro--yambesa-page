# Install packages if not already installed
if (!require(randomForest)) install.packages("randomForest", dependencies = TRUE)
if (!require(caret)) install.packages("caret", dependencies = TRUE)
# Load the libraries
library(randomForest)
library(caret)
getwd()
# Read the CSV file with the correct file path
data <- read.csv("C:/Users/202218127/Documents/Data/Task 1/CTG.csv", header = TRUE) 
str(data)
data$NSP <- as.factor(data$NSP)
table(data$NSP)
set.seed(123)# Set seed for reproducibility (ensures the same random split every time the code runs)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3)) 
train <- data[ind==1,]
test <- data[ind==2,]
set.seed(222)
rf <-randomForest(NSP~., data=train, ntree = 300, mtry = 8,importance = TRUE, proximity = TRUE)
print(rf) # Print the tuning results
attributes(rf)# Display the attributes of the trained Random Forest model
p1 <- predict(rf, train)
confusionMatrix(p1, train$NSP)
p1 <- predict(rf, test)# Make predictions on the test dataset
confusionMatrix(p1, test$NSP)
plot(rf)# Plot the Random Forest model (visualizes error rates as trees increase)
t <- tuneRF(train[,-22], train[,22],  stepFactor = 0.5, plot = TRUE,  ntreeTry = 300,  trace = TRUE,  improve = 0.05)
hist(treesize(rf), main = "No. of Nodes for the Trees",  col = "green")
varImpPlot(rf, sort = T, n.var = 10, main = "Top 10 - Variable Importance")
importance(rf) 
varUsed(rf)
partialPlot(rf,train, MSTV, "2")
getTree(rf, 1, labelVar = TRUE)
MDSplot(rf, train$NSP)
library(randomForest) 
library(caret)
Data <- read.csv("C:/Users/202218127/Documents/Data/Task 2/sample_data.csv")
View(Data)
str(Data)
ind <- sample (2, nrow (Data), replace = TRUE , prob = c (0.7, 0.3))  
training_data = Data[ind==1,
testing_data = Data[ind==2,]                     
rf = randomForest (Salinity~., data=training_data) 
rf = randomForest (Salinity~., data=training_data) 
library(randomForest) 
# Ensure data splitting is done
ind <- sample(2, nrow(Data), replace = TRUE, prob = c(0.7, 0.3))
training_data <- Data[ind == 1, ]
testing_data <- Data[ind == 2, ]
if (!"Salinity" %in% colnames(training_data)) {stop("Error: Column 'Salinity' not found in training_data!")}
rf <- randomForest(Salinity ~ ., data = training_data)
plot(rf)
Salinity_Predict = predict(rf, testing_data) 
testing_data$Salinity_Predict = Salinity_Predict 
View(testing_data)
cfm = table(testing_data$Salinity, testing_data$Salinity_Predict)
cfm
classification_Accuracy = sum(diag(cfm)/sum(cfm)) 
classification_Accuracy
# Install required packages (if not already installed)
install.packages(c("stars", "raster", "ggplot2", "sf", "ExtractTrainData"))
# Load the necessary libraries
library(stars)
library(raster)
library(ggplot2)
library(sf)
library(ExtractTrainData)
image_BBS<-stack ("C:/Users/202218127/Documents/Data/Task 2/RGB.tif")
image_BBS
plotRGB (image_BBS,3,2,1, scale =255, stretch='lin')
i<-0
for (i in seq (1,3,1)){
fname = paste ("layer_",i,".tif",sep="")  
layers <- image_BBS [[i]]
writeRaster( layers, fname,
format ="C:/Users/202218127/Documents/Data/Task 2/RGB.tif" , datatype='FLT4', overwrite =TRUE)
library(raster)
if (!exists("layers")) { layers <- raster("C:/Users/202218127/Documents/Data/Task 2/RGB.tif.ovr")  }
Layer_1<-raster("C:/Users/202218127/Documents/Data/Task 2/layer_1.tif")
Layer_2<-raster("C:/Users/202218127/Documents/Data/Task 2/layer_2.tif") 
Layer_3<-raster("C:/Users/202218127/Documents/Data/Task 2/layer_3.tif")
data1<-stack(Layer_1,Layer_2,Layer_3) 
data1
plotRGB (image_BBS,3,2,1, scale =255, stretch='lin')
# Load necessary library
library(raster)
# Plot the RGB image correctly
plotRGB(image_BBS, r = 3, g = 2, b = 1, scale = 255, stretch = "lin")         
features<-st_read("C:/Users/202218127/Documents/Data/Task 2/Shapefile") 
features
str(features)
Plot(features)
