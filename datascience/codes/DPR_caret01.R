# Data Programming with R
# Run Machine Learning models with caret

Installed <- TRUE  # For checking if package is installed
toInstall <- c('tidyverse','vroom','data.table' ,'caret', 'RANN', 'randomForest', 'gbm', 'xgboost', 'caretEnsemble', 'C50', 'earth', 'tictoc')
if(Installed){install.packages(toInstall, repos = "http://cran.us.r-project.org")}
lapply(toInstall, require, character.only = TRUE) # call into library

# Import dataset: let different packages race
library(tidyverse)
tic()
TEDS2016 <- read_csv("https://raw.githubusercontent.com/datageneration/gentlemachinelearning/master/data/TEDS2016.csv")
toc()
library(vroom)
tic()
TEDS2016 <- vroom("https://raw.githubusercontent.com/datageneration/gentlemachinelearning/master/data/TEDS2016.csv")
toc()
library(data.table)
tic()
TEDS2016 <- fread("https://raw.githubusercontent.com/datageneration/gentlemachinelearning/master/data/TEDS2016.csv")
toc()

# Structure of the dataframe

str(TEDS2016)

# See top 6 rows and 10 columns
head(TEDS2016[, 1:10])

# Quick examine of dv
library(descr)
freq(TEDS2016$votetsai)

# Create the training and test datasets
set.seed(100)

# Step 1: Get row numbers for the training data
train_rno <- createDataPartition(TEDS2016$votetsai, p=0.8, list=FALSE)
class(train_rno)
# Step 2: Create the training dataset
trainTEDS <- TEDS2016[train_rno,]

# Step 3: Create the test dataset
testTEDS <- TEDS2016[-train_rno,]

# Store X and Y for later use.
x = trainTEDS[, 2:19]
y = trainTEDS$votetsai


# Missing values

# Create the knn imputation model on the training data
preProcess_missingdata_model <- preProcess(as.data.frame(trainTEDS), method='knnImpute')
preProcess_missingdata_model


# Use the imputation model to predict the values of missing data points
# install.packages("RANN")
library(RANN)  # required for knnInpute
trainTEDS <- predict(preProcess_missingdata_model,trainTEDS)
anyNA(trainTEDS)


# One-Hot Encoding
# Creating dummy variables is converting a categorical variable to as many binary variables as here are categories.
dummies_model <- dummyVars(votetsai ~ ., data=trainTEDS)

# Create the dummy variables using predict. The Y variable (votetsai) will not be present in trainData_mat.
trainTEDS_mat <- predict(dummies_model, newdata = trainTEDS)

# # Convert to dataframe
trainTEDS <- data.frame(trainTEDS_mat)

# # See the structure of the new dataset
str(trainTEDS)


preProcess_range_model <- preProcess(trainTEDS, method='range')
trainTEDS <- predict(preProcess_range_model, newdata = trainTEDS)

# Append the Y variable
trainTEDS$votetsai <- y

apply(trainTEDS[, 1:19], 2, FUN=function(x){c('min'=min(x), 'max'=max(x))})
trainTEDS$votetsai = as.factor(trainTEDS$votetsai)
# trellis.par.set(theme = col.whitebg(), warn = FALSE)
featurePlot(x = trainTEDS[, 1:18], 
            y = trainTEDS$votetsai, 
            plot = "box",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))

featurePlot(x = trainTEDS[, 1:18], 
            y = trainTEDS$votetsai, 
            plot = "density",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))

set.seed(100)
options(warn=-1)

# Variable selection using Recursive Feature Elimination (rfe)
# subsets1 <- c("dpp","taiwanese","dadmainland","econworse", "pubwelf", "south","green")
subsets = c(1,2,7, 8, 10, 13, 16, 18)

# names(trainTEDS)
# Backward selection/Recursive Feature elimination
ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 2,
                   verbose = FALSE)

# Training the model (could take a while)
# use tictoc package to time the process
library(tictoc)
tic("Training the model")
lmProfile <- rfe(x=trainTEDS[, 1:18], y=trainTEDS$votetsai,
                 sizes = subsets,
                 rfeControl = ctrl)
toc()
lmProfile
# See available algorithms in caret
modelnames <- paste(names(getModelInfo()), collapse=',  ')
modelnames


modelLookup('earth')

# Set the seed for reproducibility
set.seed(100)

# Multivariate Adaptive Regression Splines (MARS)
# Train the model using randomForest and predict on the training data itself.
tic("earth method")
model_earth = train(votetsai ~ ., data=trainTEDS, method='earth')
fitted <- predict(model_earth)
toc()
plot(model_earth, main="Model Accuracies with MARS")

varimp_earth <- varImp(model_earth)

plot(varimp_earth, main="Variable Importance with MARS")

# Step 1: Impute missing values 
testTEDS2 <- predict(preProcess_missingdata_model, testTEDS)  

# Step 2: Create one-hot encodings (dummy variables)
testTEDS3 <- predict(dummies_model, testTEDS2)
freq(testTEDS$votetsai)
# Step 3: Transform the features to range between 0 and 1
testTEDS4 <- predict(preProcess_range_model, testTEDS3)

# View
head(testTEDS4[, 1:10])

# Predict on testData
predicted <- predict(model_earth, testTEDS4)
head(predicted)


# Compute the confusion matrix
confusionMatrix(reference = as.factor(testTEDS$votetsai), data = predicted, mode='everything', positive='Yes')

