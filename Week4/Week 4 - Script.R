# Before kicking off remember to set your directory e.g. as below:
# getwd()
# Remove the hashtag to run the line of script
# setwd("C:/Users/keaan/Documents")



# If using last week's script, remember to adapt the URL to below #
df <- read.csv("https://raw.githubusercontent.com/keaan95/virtual-elective/master/Week4/heart_failure_clinical_records_dataset.csv")

###Eliminate columns where we have missing data.
df <- df[ , colSums(is.na(df)) == 0]

# Take a look at the data
head(df)

# More details on the data
# https://www.kaggle.com/andrewmvd/heart-failure-clinical-data

# For our ML algorithm, our classification is DEATH_EVENT so we can extract this and give it a more apt name

###Pdata Create
pdata <- df[drop=FALSE,,colnames(df) %in% c("DEATH_EVENT")]
pdata$endpoint <- as.character(pdata$DEATH_EVENT)
pdata$endpoint <- ifelse(pdata$endpoint=="1","Death",pdata$endpoint)
pdata$endpoint <- ifelse(pdata$endpoint=="0","Living",pdata$endpoint)
head(pdata)

# TASK 1
# SELECT VARIABLES FOR YOUR ALGORITHM
# CLINICAL INSIGHT
# FILTER by the distribution of the data e.g. correlations, histograms, boxplots, variances in the data
# WRAPPER by using the algorithm to reiteratively determine the optimal number of features
# EMBEDDED methods are computationally complex but apply penalty weightings against complexity and some have proven higher levels of performance on many types of dataset e.g. Lasso

# Boruta, Recursive Feature Elimination



###Correlation dataset without DEATH_EVENT, age, sex, time
dfint <- df[,!colnames(df) %in% c("DEATH_EVENT","age","sex","time")]

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


# Finally specify the variables, you want for your model - 'feature selection'

df_filt <- df[, colnames(df) %in% c("anaemia","creatinine_phosphokinase","diabetes","ejection_fraction","high_blood_pressure","platelets"
                                    ,"serum_creatinine","serum_sodium","sex","smoking")]
# Importantly we need to realign our classification endpoint with our new features for our caret package
df_filt$endpoint <- pdata$endpoint

##Automatic Boxplots and Histograms
# Documentation is here - https://machinelearningmastery.com/data-visualization-with-the-caret-r-package/
plot.new()
#install.packages("caret")

library(caret)
featurePlot(x = df_filt[, 1:10], 
            y = as.factor(df_filt$endpoint), 
            plot = "density",
            strip=strip.custom(par.strip.text=list(cex=.9)),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))


### TASK 2
# Select your Algorithm or algorithms
# alg_list variables can be found at https://topepo.github.io/caret/available-models.html
alg_list <- c("rf", "nnet","glm")
alg_one <- "rf"

# Remember to set seed each time
set.seed(100)

library(nnet)


# Create the training and test datasets
# Step 1: Get row numbers for the training data
trainRowNumbers <- createDataPartition(pdata$DEATH_EVENT, p=0.8, list=FALSE)
# Step 2: Create the training  dataset
trainData <- df_filt[trainRowNumbers,]
# Step 3: Create the test dataset
testData <- df_filt[-trainRowNumbers,]
##Cross-validation 10x as our control
control <- trainControl(method="cv", number=10,classProbs=TRUE,summaryFunction = twoClassSummary, savePredictions = T)
##Assess performance by ROC
metric <- "ROC"


?train
## Seeding ensures reproducibility
set.seed(65)
results <- train(endpoint~., data=trainData, method=alg_one, metric="ROC",
                 trControl = control)
# View our results variable
plot(results)
results$results

# If you'd like to run more than 1 algorithm
# CaretEnsemble lets us run multiple algorithms
library(caretEnsemble)
set.seed(65)
?caretList
resultsList <- caretList(endpoint~ ., data=trainData,methodList = alg_list,metric="ROC",
          trControl = trainControl(method = "cv", number = 10,classProbs=TRUE,summaryFunction = twoClassSummary,
                                   savePredictions = T))

resListdata <- resamples(resultsList)
?resamples
resListdata$values
dotplot(resListdata)


###TASK 3 - Evaluate the performance on UNSEEN DATA
predictions <- predict(resultsList$rf,testData)
resultsDEFmodel<- confusionMatrix(predictions,as.factor(testData$endpoint), positive="Death")
resultsDEFmodel



### TASK 4
# Overall, how will this impact how you manage patients initially and on follow-up?


## Save our best performing model
saveRDS(resultsList$rf, "heart_failure_model.RDS")

## Alternatively we can save our R environment
save.image(file = "Week4env.RData")

# we can load this back into R by
getwd()
list.files()

hf_model <- readRDS("heart_failure_model.RDS")

### BONUS Feature Selection Methods

#WRAPPER - Variable Importance - how much a given model uses that variable to make predictions
varimp <- varImp(resultsList$rf,scale=FALSE)
plot(varimp)


#RECURSIVE FEATURE ELIMINATIOn
rfecontrol <- rfeControl(functions=rfFuncs, method="cv", number=10)
rferesults <- rfe(trainData[,1:ncol(trainData)-1],
                                        as.factor(trainData[,ncol(trainData)]),
                                        sizes=c(1:ncol(trainData)-1),
                                        rfeControl=rfecontrol,metric =  "ROC",
                                        summaryFunction = twoClassSummary)

print(rferesults)
predictors(rferesults)

plot(rferesults, type=c("g", "o"))

##Internally algorithms such as RandomForests do strong feature selection so RFE is a useful feature selector for algorithms without this
# In other words, your choice of feature selection is more important when you have chosen an algorithm to build your model