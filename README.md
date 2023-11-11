# online_shoppers_intention
Q:1

# Load the dataset
data <- read.csv ("Desktop/R-final/online_shoppers_inten=on.csv")
View(data)

# Create separate data frames for ExitRates and PageValues
exitRates <- data$ExitRates
pageValues <- data$PageValues

# Plot histograms side by side

hist_exitRates <- ggplot(data, aes(x = ExitRates)) +
 geom_histogram(binwidth = 0.05, color = "black", fill = "lightblue") +
 labs(x = "ExitRates", y = "Frequency") +
 gg=tle("Histogram of ExitRates")
hist_pageValues <- ggplot(data, aes(x = PageValues)) +
 geom_histogram(binwidth = 0.05, color = "black", fill = "lightblue") +
 labs(x = "PageValues", y = "Frequency") +
 gg=tle("Histogram of PageValues")
 
# Arrange the histograms side by side
grid.arrange(hist_exitRates, hist_pageValues, ncol = 2)

Q:2

# Set seed for reproducibility
set.seed(123)

# Generate random indices for training set
trainIndex <- sample(1:nrow(data), size =floor(0.7 * nrow(data)))

# Create training and tes=ng sets
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Logis=c Regression
logit <- glm(Revenue ~ .,
 data = trainData, family="binomial")
summary(logit)
my_predic=on <- predict(logit, testData, type="response")

# building a decision tree
library(rpart)
library(rpart.plot)
library(caret)
library(caTools)
library(pROC)
my_tree <- rpart(Revenue ~ .,
 data = trainData, method="class", cp=0.02)
rpart.plot(my_tree, type=1, extra=1)
predic=ons <- predict(my_tree, testData, type="prob")

Q:3 

cm_log <- confusionMatrix(data= as.factor(as.numeric(my_predic=on>0.5)),
 reference=as.factor(as.numeric(testData$Revenue)))
 
cm_tree <- confusionMatrix(data= as.factor(as.numeric(predic=ons[,2]>0.5)),
 reference=as.factor(as.numeric(testData$Revenue)))

 Q:4 

 # Extract numerical variables
numerical_vars <- data[, sapply(data, is.numeric)]
df <- as.data.frame(numerical_vars)

# Create an empty vector to store the results
results <- vector("list", ncol(df))

# Loop through each numerical variable
for (i in 1:ncol(df)) {
 my_min <- try(min(df[, i], na.rm = TRUE))
 my_max <- try(max(df[, i], na.rm = TRUE))
 my_mean <- try(mean(df[, i], na.rm = TRUE))
 my_std <- try(sd(df[, i], na.rm = TRUE))
 my_median <- median(df[, i], na.rm = TRUE)
 results[[i]] <- c(my_min, my_mean, my_std, my_max, my_median)
}

# Print the results
for (i in 1:ncol(df)) {
 var <- colnames(df)[i]
 cat("Variable:", var, "\n")
 cat("Median:", results[[i]][5], "\n")
 cat("Standard Devia=on:", results[[i]][3], "\n\n")
}
