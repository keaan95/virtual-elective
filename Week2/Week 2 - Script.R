df <- read.csv("https://raw.githubusercontent.com/keaan95/virtual-elective/master/Week1/data.csv")

## Remove NAs
df <- df[ , colSums(is.na(df)) == 0]

## Dataframe of just values
dfint <- df[, !(colnames(df) %in% c("diagnosis"))]

##Create a dataframe of correlations
cormat <- round(cor(dfint),2)

## Correlations into two columns for plotting
# install.packages("reshape2")
library("reshape2")
melted_cormat <- melt(cormat)
head(melted_cormat)

# install.packages("ggplot2")
library(ggplot2)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile(color = "white")

# Making beautiful graphs - lots of R tutorials on the internet
# Heatmaps - colour schemes to visualise and stretch differences in data
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

##Feature Selection
df_filt <- df[, colnames(df) %in%
                c("radius_mean","texture_mean","perimeter_mean",
                  "area_mean","diagnosis")]

## Instead of plotting everything using ggplot2 - we can use the plotting functions in caret
# install.packages("caret")
library("caret")
plot.new()
featurePlot(x = df_filt[, 2:5], 
            y = as.factor(df_filt$diagnosis), 
            plot = "box",
            strip=strip.custom(par.strip.text=list(cex=.9)),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))

featurePlot(x = df_filt[, 2:5], 
            y = as.factor(df_filt$diagnosis), 
            plot = "density",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))


# Create the training and test datasets

# Seeding ensures replicability!
set.seed(100)


# install.packages("nnet")
library(nnet)

# install.packages("e1071")
library("e1071")

# install.packages("caretEnsemble") - ensembl adding more than 1 algorithm to our model or test multiple algorithms at once
library(caretEnsemble)

# Step 1: Get row numbers for the training data
trainRowNumbers <- createDataPartition(df_filt$diagnosis, p=0.8, list=FALSE)

# Step 2: Create the training  dataset
trainData <- df_filt[trainRowNumbers,]

# Step 3: Create the test dataset
testData <- df_filt[-trainRowNumbers,]


##Lets make Cross-validation 10x as our control
control <- trainControl(method="cv", number=10)

##Assess performance by ROC
metric <- "ROC"

results <- train(diagnosis~., data=trainData, method="rf", metric="ROC",
                 trControl = trainControl(method = "cv", number = 10,classProbs=TRUE,summaryFunction = twoClassSummary, savePredictions = T))

alg_list <- c("rf", "glm", "gbm", "lda", "rpart", "nnet","svmRadial","knn")
set.seed(65)
?caretList
resultsList <- caretList(diagnosis~ ., data=trainData,methodList = alg_list,metric="ROC",
                         trControl = trainControl(method = "cv", number = 10,classProbs=TRUE,summaryFunction = twoClassSummary,
                                                  savePredictions = T))

##Which algorithm is performing the best?
resListdata <- resamples(resultsList)
?resamples
resListdata$values
dotplot(resListdata)

## How does our best performing model do on our test data?

predictions <- predict(resultsList$knn,testData)
results<- confusionMatrix(predictions,as.factor(testData$diagnosis), positive="M")

## Save for next time
saveRDS(resultsList$knn, "breast_cancer_model.RDS")