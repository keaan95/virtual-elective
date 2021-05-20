
setwd("C:/Users/keaan/Documents")
df <- read.csv("indian_liver_patient.csv", sep=",",header=T)

###Lets eliminate columns where we have missing data.
df <- df[ , colSums(is.na(df)) == 0]



###eliminate columns that we do not need
# df <- df[,!colnames(df) %in% c("Dataset")]


###pdata create

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
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

# If one or more variables are more correlated, this suggests one is depenent on the other.
## An algorithm will assume and estimate relationships based on independent variables.
# Based on the algorithm we selection, we may miss out on dependent details 
# We are therefore at a risk of overfitting and to avoid this we can remove correlated variables. This may further repercussions

### 


### Dimensionality Reduction

# Transform a large set of variables into a smaller set, whilst preserving variability.
# In effect avoid losing variation in the data

# Instead of looking at variables individually, convert these into a function that maximises this variance using uncorrelated variables.
# Does this by applying eigenvectors [a function that explains the variation in data]

# Particularly useful in biological based data when the variance of a variable is not proportional to the mean and picture of separability is more difficult to tease out.

pcas <- prcomp(dfint,scale=TRUE,center=TRUE)
str(pcas)
pcasdf <-data.frame(pcas$x)
head(pcasdf)

require(ggplot2)
ggplot(pcasdf,aes(x=PC1,y=PC2,col=pdata$diagnosis))+
  labs(color = "Subtypes") +
  geom_point(size=3)





melt(dfint,drop=FALSE)


# Feature Selection Methods
# In other words what varaibles shall we use for our model
# Our expertise / experience as clinicians to pick out what is relevant.

# Can feature select by mathematical methods [Week 2]

# For this first task - lets just take some variables
head(df_filt)

df_filt <- df[, colnames(df) %in% c("Direct_Bilirubin","Total_Bilirubin","Alkaline_Phosphotase","Alamine_Aminotransferase","Aspartate_Aminotransferase","Total_Protiens",
                                    "Albumin")]
df_filt$diagnosis <- pdata$diagnosis

##Automatic Boxplots and Histograms
plot.new()

require(caret)
featurePlot(x = df_filt[, 1:7], 
            y = as.factor(pdata$diagnosis), 
            plot = "box",
            strip=strip.custom(par.strip.text=list(cex=.9)),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))

# Create the training and test datasets
set.seed(100)
library(nnet)
require("e1071")
## install.packages("caretEnsemble")
require(caretEnsemble)

# Step 1: Get row numbers for the training data
trainRowNumbers <- createDataPartition(pdata$Dataset, p=0.8, list=FALSE)

# Step 2: Create the training  dataset
trainData <- df_filt[trainRowNumbers,]

# Step 3: Create the test dataset
testData <- df_filt[-trainRowNumbers,]


##Lets make Cross-validation 10x as our control
control <- trainControl(method="cv", number=10)

##Assess performance by ROC
metric <- "ROC"


?train
## Lets
set.seed(65)
results <- train(diagnosis~., data=trainData, method="rf", metric="ROC",
                 trControl = trainControl(method = "cv", number = 10,classProbs=TRUE,summaryFunction = twoClassSummary, savePredictions = T))

## Lets evaluate more than one model
alg_list <- c("rf", "glm", "gbm", "lda", "rpart", "nnet","svmRadial","knn")
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
predictions <- predict(resultsList$glm,testData)
results<- confusionMatrix(predictions,as.factor(testData$diagnosis), positive="Disease")

## Lets save our best performing model
saveRDS(resultsList$glm, "liver_disease_model.RDS")

# we can load this back into R by
getwd()
list.files()

bc_model <- readRDS("breast_cancer_model.RDS")


# boruta, lasso and RFE


require(glmnet)
  x <- as.matrix(df_filt[,1:ncol(df_filt)-1]) # all X vars
  y <- as.double(as.matrix(ifelse(df_filt[,ncol(df_filt)]=='Normal', 0, 1))) # Only Class
  # Fit the LASSO model (Lasso: Alpha = 1)
  set.seed(100)
  fit.lasso.df_filt <- glmnet(x, y, family = "binomial")
  plot.lasso.df_filt <- cv.glmnet(x, y, family='binomial', alpha=1, parallel=TRUE, standardize=TRUE, type.measure='deviance')
  plot(plot.lasso.df_filt)
  # Results
  cat('Min Lambda: ', plot.lasso.df_filt$lambda.min, '\n 1Sd Lambda: ', plot.lasso.df_filt$lambda.1se)
  out.lasso.df_filt <- round(as.matrix(coef(plot.lasso.df_filt, s=plot.lasso.df_filt$lambda.min)), 2)
  out.lasso.df_filt[out.lasso.df_filt[, 1] != 0, ]
  
  
names(out.lasso.df_filt[out.lasso.df_filt[, 1] != 0, ])[-1]

feature_selector <- list(names(out.lasso.df_filt[out.lasso.df_filt[, 1] != 0, ])[-1])
names(feature_selector) <- c("Lasso")
