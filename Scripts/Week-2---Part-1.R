
setwd("C:/Users/keaan/Documents")
df <- read.csv("data.csv", sep=",",header=T,row.names = 1)

###Lets eliminate columns where we have missing data.
df <- df[ , colSums(is.na(df)) == 0]

###Lets adjust our input of our dataframe into integers - ready for ML
# So we have factors i.e. a grouped categorical variable that needs to be converted to characters [short strings of text] -> numeric
# df$diagnosis <- as.character(df$diagnosis)

pdata <- df[drop=FALSE,, grep("diagnosis", names(df))]
head(pdata)
pdata$diagnosis <- as.character(pdata$diagnosis)
pdata$diagnosis2 <- pdata$diagnosis
pdata$diagnosis <- ifelse(pdata$diagnosis=="M","Malignant",pdata$diagnosis)
pdata$diagnosis <- ifelse(pdata$diagnosis=="B","Benign",pdata$diagnosis)
head(pdata)


# df$diagnosis <- ifelse(df$diagnosis=="M",1,df$diagnosis)
# df$diagnosis <- ifelse(df$diagnosis=="B",0,df$diagnosis)
# df$diagnosis <- as.numeric(df$diagnosis)


hist_b <- df$perimeter_mean[df$diagnosis=="B"]
hist_m <- df$perimeter_mean[df$diagnosis=="M"]

# First distribution
plot.new()
hist(hist_b, breaks=10, xlim=c(0,250), col=rgb(0,0,1,0.5), xlab="Perimeter Mean", 
     ylab="Density", main="Perimeter Mean across Breast Tissue Types" )

# Second with add=T to plot on top
hist(hist_m, breaks=10, xlim=c(0,250), col=rgb(1,0,0,0.5), add=T)

# Add legend
legend("topright", legend=c("Benign","Malignant"), col=c(rgb(0,0,1,0.5), 
                                                         rgb(1,0,0,0.5)), pt.cex=2, pch=15 )


###Figure out how correlated our variables are

dfint <- df[, !(colnames(df) %in% c("diagnosis"))]

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




### Other dimensionality reduction plots
require(umap)
umaps <- umap(dfint)


umaps$layout <- as.data.frame(umaps$layout)
umaps$layout$V3 <- rownames(umaps$layout)
umaps$layout$V4 <- pdata$diagnosis[match(umaps$layout$V3,rownames(pdata))]

ggplot(umaps$layout,aes(x=V1,y=V2, color=V4)) +
  geom_point(size=3) +
  xlab("UMAP 1") +
  ylab("UMAP 2") +
  guides(fill=guide_legend(title="Tumour Type"))

require(Rtsne)

downtsnedata <- Rtsne(df,perplexity = 15)
downtsnedf <- as.data.frame(downtsnedata$Y)
rownames(downtsnedf) <- rownames(pdata)
downtsnedf$V3 <- as.matrix(rownames(downtsnedf))
downtsnedf$V4 <- pdata$diagnosis[match(downtsnedf$V3,rownames(pdata))]


ggplot(downtsnedf,aes(x=V1,y=V2, color=V4)) +
  geom_point(size=4) +
  guides(fill=guide_legend(title="LRCG Subtypes")) +
  xlab("TSNE 1") +
  ylab("TSNE 2")

require(reshape2)

melt(df,drop=FALSE)


# Feature Selection Methods
# In other words what varaibles shall we use for our model
# Our expertise / experience as clinicians to pick out what is relevant.

# Can feature select by mathematical methods [Week 2]

# For this first task - lets just take some variables
head(df)

df_filt <- df[, colnames(df) %in% c("radius_mean","texture_mean","perimeter_mean","area_mean","diagnosis")]

##Automatic Boxplots and Histograms
plot.new()

require(caret)
featurePlot(x=iris[,1:4], y=iris[,5], plot="box", scales=list(x=list(relation="free"), y=list(relation="free")), auto.key=list(columns=3))

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
set.seed(100)
library(nnet)
require("e1071")
## install.packages("caretEnsemble")
require(caretEnsemble)

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
predictions <- predict(resultsList$knn,testData)
results<- confusionMatrix(predictions,testData$diagnosis, positive="M")

## Lets save our best performing model
saveRDS(resultsList$knn, "breast_cancer_model.RDS")

# we can load this back into R by
getwd()
list.files()

bc_model <- readRDS("breast_cancer_model.RDS")

##Practically, we would usually start with a simple model - one that can be explainable or by previous knowledge would with with the data we're working with
##We rarely would through a library at it and simply select because we tune and improve our models 

###

## How can we validate our results from a few models to see how we would have done?

## Loops to do some code on every value in a vector

numberlist <-1:10

for(i in numberlist){
  print(i)
}

## Loop on names in a list

names(resultsList)  

for(alg in names(resultsList)){
  print(alg)
}

##Loop on names in a list and put it into a new list

algList <- 

for(alg in names(resultsList)){
  print(alg)
}

predictionsList <- list()
confusionMatrixList <- list()

for (alg in names(resultsList)) {
  predictionsList[[alg]] <- predict(resultsList[[alg]],testData)
  confusionMatrixList[[alg]] <- confusionMatrix(predictionsList[[alg]],testData$diagnosis, positive="M")
  print(alg)
}

##Lets see our outputs
predictionsList$rf
confusionMatrixList$svmRadial
confusionMatrixList$svmRadial$byClass

##Bonus Homework - Lets compare + plot the performance on validation across all our models
as.data.frame(confusionMatrixList$svmRadial$byClass)

###Further Learning
## Ensembl models using the package we have - https://cran.r-project.org/web/packages/caretEnsemble/vignettes/caretEnsemble-intro.html
