# Before kicking off remember to set your directory e.g. as below:
# getwd()
# setwd("C:/Users/keaan/Documents")

# install.packages("randomForest")
## install.packages("caretEnsemble")
### install.packages("nnet")

# If using last week's script, remember to adapt the URL to below #
df <- read.csv("https://raw.githubusercontent.com/keaan95/virtual-elective/master/Week1/indian_liver_patient.csv")

###Eliminate columns where we have missing data.
df <- df[ , colSums(is.na(df)) == 0]

# Take a look at the data
head(df)

# For our ML algorithm, we only supply it numbers and not names / characters so we'll extract these columns
# Create a Pdata [phenotypic data - data that helps better describe the data]

###Pdata Create
pdata <- df[,colnames(df) %in% c("Age","Gender","Dataset")]
pdata$diagnosis <- as.character(pdata$Dataset)
pdata$diagnosis <- ifelse(pdata$diagnosis=="1","Disease",pdata$diagnosis)
pdata$diagnosis <- ifelse(pdata$diagnosis=="2","Normal",pdata$diagnosis)
head(pdata)

###dataset of just integers and without pdata
dfint <- df[,!colnames(df) %in% c("Age","Gender","Dataset")]

###Figure out how correlated our variables are
cormat <- round(cor(dfint),2)

library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)

library(ggplot2)
# For an advanced GGPLOT2 tutorial - http://r-statistics.co/Complete-Ggplot2-Tutorial-Part1-With-R-Code.html
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()


melt(dfint,drop=FALSE)


# Feature Selection Methods - In other words, what variables shall we use for our model

df_filt <- df[, colnames(df) %in% c("Direct_Bilirubin","Total_Bilirubin","Alkaline_Phosphotase","Alamine_Aminotransferase","Aspartate_Aminotransferase","Total_Protiens",
                                    "Albumin")]
# Importantly we need to realign our pdata diagnosis with our new dataframe
df_filt$diagnosis <- pdata$diagnosis

##Automatic Boxplots and Histograms
plot.new()
#install.packages("caret")

library(caret)
featurePlot(x = df_filt[, 1:7], 
            y = as.factor(pdata$diagnosis), 
            plot = "box",
            strip=strip.custom(par.strip.text=list(cex=.9)),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))

# Create the training and test datasets
set.seed(100)
library(nnet)
library(caretEnsemble)

# Step 1: Get row numbers for the training data
trainRowNumbers <- createDataPartition(pdata$Dataset, p=0.8, list=FALSE)

# Step 2: Create the training  dataset
trainData <- df_filt[trainRowNumbers,]

# Step 3: Create the test dataset
testData <- df_filt[-trainRowNumbers,]


##Lets make Cross-validation 10x as our control
control <- trainControl(method="cv", number=10,classProbs=TRUE,summaryFunction = twoClassSummary, savePredictions = T)

##Assess performance by ROC
metric <- "ROC"


?train
## Seeding ensures reproducibility
set.seed(65)
# Do one run with randomforest for example
results <- train(diagnosis~., data=trainData, method="rf", metric="ROC",
                 trControl = control)
# View our results
plot(results)
results$results

### Grid Searching using one parameter - adjusting mtry over all 7 variables
set.seed(65)
tunegrid <- expand.grid(.mtry=c(1:7))
rf_gridsearch <- train(diagnosis~., data=trainData, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)

# Grid searching using two parameters - saving the outputs
## Adjusting ntree
?randomForest.default
## We can read that ntree of 500 is default so we'll go above and below

control <- trainControl(method="cv", number=10, search="grid",
                        classProbs=TRUE,summaryFunction = twoClassSummary, savePredictions = T)
tunegrid <- expand.grid(.mtry=c(1:7))
modellist <- list()
for (ntree in c(100, 200, 500, 1000)) {
  set.seed(65)
  models <- train(diagnosis~., data=trainData, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control, ntree=ntree)
  id <- toString(ntree)
  modellist[[id]] <- models
}

# Comparing results across multiple tree sizes
results_gridsearch <- resamples(modellist)
summary(results_gridsearch)
dotplot(results_gridsearch)

predictions <- predict(modellist$`500`,testData)
resultsBESTmodel<- confusionMatrix(predictions,as.factor(testData$diagnosis), positive="Disease")
saveRDS(resultsBESTmodel, "liver_disease_tuned_model.RDS")

## Comparing a default RF against other algorithms
alg_list <- c("rf", "glm", "nnet")
set.seed(65)
?caretList
resultsList <- caretList(diagnosis~ ., data=trainData,methodList = alg_list,metric="ROC",
          trControl = trainControl(method = "cv", number = 10,classProbs=TRUE,summaryFunction = twoClassSummary,
                                   savePredictions = T))

resListdata <- resamples(resultsList)
?resamples
resListdata$values

dotplot(resListdata)

## Lets see how our best performing model does on validation.
predictions <- predict(resultsList$rf,testData)
resultsDEFmodel<- confusionMatrix(predictions,as.factor(testData$diagnosis), positive="Disease")

## Lets save our best performing model
saveRDS(resultsList$rf, "liver_disease_default_model.RDS")

## Alternatively we can save our R environment [to save computational time]
save.image(file = "Week3env.RData")

# we can load this back into R by
getwd()
list.files()

bc_model <- readRDS("liver_disease_model.RDS")



