### Split Half cross validation for PLSC ###
### Author: Ju-Chi Yu
### Date: Oct. 31st, 2025
##---------------------------------------

## Reading packages ----
library(TExPosition) # to run PLSC
library(purrr) # used in the kfold function

## Read needed functions ----
source("ProjectSupplementaryData4PLS.R")
source("PLS.SplitHalfCV.R")

## Read example data ----
data(beer.tasting.notes)
data1<-beer.tasting.notes$data[,1:8]
data2<-beer.tasting.notes$data[,9:16]

## Run PLSC ----
pls.res <- tepPLS(data1,data2, graphs = FALSE)

## Perform 10-fold validation
pls.split <- PLS.SplitHalfCV(data1, data2, pls.res)

## Results - results from the two halves ----
### a list of data1 loadings (variable x components) from each iteration
pls.split$cross.validation.res$p.test # Half 1
pls.split$cross.validation.res$p.train # Half 2

### a list of data2 loadings (variable x components) from each iteration
pls.split$cross.validation.res$q.test # Half 1
pls.split$cross.validation.res$q.train # Half 2

### a list of eigenvalues (a vector with length equals the number of components) from each iteration
pls.split$cross.validation.res$eig.test # Half 1
pls.split$cross.validation.res$eig.train # Half 1

### Correlation between the two halves
pls.split$cross.validation.res$cor.res$p # iteration x components
pls.split$cross.validation.res$cor.res$q # iteration x components

### Checking the correlation between the two halves of all PLS dimensions
colMeans(pls.split$cross.validation.res$cor.res$p) # a vector with length equals # of components
colMeans(pls.split$cross.validation.res$cor.res$q) # a vector with length equals # of components

## An example to plot the results
library(ggplot2)
library(tidyr)
### setting column names
colnames(pls.split$cross.validation.res$cor.res$p) <- paste0("Dim", 1:ncol(pls.split$cross.validation.res$cor.res$p))
### making it a data frame, so that we can plot it later
df <- as.data.frame(pls.split$cross.validation.res$cor.res$p)
### Reformat the data frame for plotting
df_long <- pivot_longer(df, cols = everything(),
                        names_to = "Dim", values_to = "Value")
### Plot the density function of each column (i.e., component)
ggplot(df_long, aes(x = Value, color = Dim)) +
  geom_density(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Density of each column", x = "Value", y = "Density")
