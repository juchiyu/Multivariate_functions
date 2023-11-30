### 10-fold cross validation for PLSC ###
### Author: Ju-Chi Yu
### Date: Nov. 30th, 2023
##---------------------------------------

## Reading packages ----
library(TExPosition) # to run PLSC
library(purrr) # used in the kfold function

## Read needed functions ----
source("ProjectSupplementaryData4PLS.R")
source("PLS.kFoldCV.R")

## Read example data ----
data(beer.tasting.notes)
data1<-beer.tasting.notes$data[,1:8]
data2<-beer.tasting.notes$data[,9:16]

## Run PLSC ----
pls.res <- tepPLS(data1,data2, graphs = FALSE)

## Perform 10-fold validation
pls.cv <- PLS.kFoldCV(data1, data2, pls.res, k = 10)
