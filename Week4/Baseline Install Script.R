# Baseline install scripts

# Caret is the ML package
# GGplot2 is a graph production package
# CaretEnsemble allows us to parse multiple algorithms at once
# RandomForest and nnet are two different classification algorithms that we will be trialling.

install.packages(c("caret","ggplot2","caretEnsemble","randomForest","nnet"))

# After installing these packages - restart R.
# Next load packages below

library("caret")
library("ggplot2")
library("caretEnsemble")
library("randomForest")
library("nnet")

# Please do say if you have any errors loadi