receptors <- c("ER","PR","HR")


receptors[1]

receptors=="ER"

getwd()

df <- read.csv("data.csv",sep=",")
head(df)

is.na(df)


df <- df[,colSums(is.na(df)) == 0]

is.na(df)

with(df,table(diagnosis))

str(df)

pdata <- df[drop=FALSE,,grep(pattern = "diagnosis",names(df))]

pdata

names(df)

hist(df$perimeter_mean)
?hist

hist(df$perimeter_mean,col="purple",xlab="Perimeter Mean",main="Distribution of Perimeter Means across Breast Cancer Patients")

benign <- df$perimeter_mean[df$diagnosis=="B"]
malignant <- df$perimeter_mean[df$diagnosis=="M"]

plot.new()
hist(benign,xlim=c(0,250),breaks=10,col = "purple",
     xlab="Perimeter Mean",
     ylab="Density",
     main="Perimeter mean across breast Cancer Patients")
hist(malignant,xlim=c(0,250),breaks=10, col = "red",add=T)

###

install.packages("ggplot2")
library("ggplot2")


head(df)

df[,!colnames(df) %in% c("diagnosis")]

dfint <- df[,!colnames(df) %in% c("diagnosis")]

dfcor <- round(cor(dfint),2)

install.packages("reshape2")
library(reshape2)

dfcor

melt_dfcor <- melt(dfcor)

ggplot(data=melt_dfcor,aes(x=Var1,y=Var2,fill=value)) +
  geom_tile(color="white")

ggplot(data = melt_dfcor, aes(x=Var1, y=Var2, fill=value))+
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

?ggplot
