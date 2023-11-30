### An example of the plotting function
### Created by Ju-Chi Yu; 2023 11/16
# Package to install
devtools::install_github("HerveAbdi/PTCA4CATA") # If you have never installed it
library(PTCA4CATA)
library(ggplot2)
library(tidyverse)
#---------------------------------
# get example data
data("iris")
#---------------------------------
# create color vectors based on group information
## 1. create a vector of colors you want with names of each group ----
## tips: make sure the names match the group information you have
grp.col <- c("setosa" = "darkorchid4",
             "versicolor" = "seagreen",
             "virginica" = "coral")
## Notes: if you have the coloring information, you can also use available ones as well.
##        But, make sure that you have names matching what you have in the vector with
##        group information.

## 2. create a color vector of observations by recoding the groups with their color ----
ind.col <- recode(iris$Species, # the vector with group information
                  !!!grp.col)

## Optional: I like to put them in a list with two matrices: oc (observation colors) and gc (group colors)
col.list <- list(oc = as.matrix(ind.col),
                 gc = as.matrix(grp.col))
## tips: I also like to assign rownames to oc with IDs, which can be helpful later
rownames(col.list$oc) <- rownames(iris)
#---------------------------------
# Now we start plotting
## 1. individual points ----
f.graph <- createFactorMap(iris,
                           axis1 = 1, axis2 = 2, # These specify the columns of data you're using to plot your two axes
                           col.points = col.list$oc[rownames(iris),], # coloring the points: 
                                                                      # I like to use rownames(iris) to make sure that 
                                                                      # the colors are ordered according to the same 
                                                                      # order as the data
                           col.labels = col.list$oc[rownames(iris),], # coloring labels for points; usually the same as col.points
                           title = "Title goes here",
                           pch = 19, # shapes of points
                           cex = 2, # size of points
                           alpha.points = 0.3, # transparency of points
                           text.cex = 2, # size of labels
                           col.background = NULL, # white background
                           col.axes = "gray60", # color for axes
                           alpha.axes = 0.5, # transparency of axes
                           )

### Notes: there are several things in f.graph list which can be added to create different combination of the figure
###       1. zeMap: this is the combination of everything
###       2. zeMap_background: just the background (this should always be used to start with if not plotting zeMap)
###       3. zeMap_dots: just the points
###       4. zeMap_text: just the labels
###       5. constraints: the constraints (min max of both axes) of the plot which can be reuse later

## 2. group points ----
### If you haven't computed the group means, you can do it with the following code
grp.mean <- Boot4Mean(iris[,-5], # data (I'm just removing the last column as it's the group info)
                      iris$Species, # the group vector
                      niter = 1000 # times of resampling for bootstrap of the group means
                      )
grp.mean
### Notes: the result of this function is consist of 3 things which you can check by printing it
### tips: the columns names will not be kept, but they should be the same for the plotting function to work
###       So, let's add them back
grp.mean <- lapply(grp.mean, 
                   function(x){
                     colnames(x) <- colnames(iris[,-5])
                     x
                   })

#------------now plot-----------
## plotting group means
f.mean <- createFactorMap(grp.mean$GroupMeans,
                           axis1 = 1, axis2 = 2, # These specify the columns of data you're using to plot your two axes
                           col.points = col.list$gc[rownames(grp.mean$GroupMeans),], # coloring the points: 
                           # I like to use rownames of the data to make sure that 
                           # the colors are ordered according to the same 
                           # order as the groups in the data
                           col.labels = col.list$gc[rownames(grp.mean$GroupMeans),], # coloring labels for points; usually the same as col.points
                           title = "Title goes here",
                           pch = 17, # shapes of points
                           cex = 3, # size of points
                           alpha.points = 0.7, # transparency of points
                           text.cex = 4, # size of labels
                           col.background = NULL, # white background
                           col.axes = "gray60", # color for axes
                           alpha.axes = 0.5, # transparency of axes
)

## plotting bootstrapped confidence intervals for group means
f.CI <- MakeCIEllipses(grp.mean$BootCube,
                       axis1 = 1, axis2 = 2,
                       col = col.list$gc[rownames(grp.mean$BootCube),], # use the rownames to make sure you have the colors in the correct order
                       names.of.factors = colnames(grp.mean$BootCube)[c(1,2)], # This is a weird argument where you need to specify the column names of the data you're plotting
                       line.size = 0.8, # size of the line of the ellpises
                       alpha.ellipse = 0.01, # transparency of the areas of ellipises
                       alpha.line = 0.8 # transparency of the line of the ellipses
                       )

## Now combine them different elements (they are layered up according to the order you add them)
f.final <- f.graph$zeMap_background + # the background from the observation plot
  f.graph$zeMap_dots + # the points of individuals
  f.CI + # the confidence intervals: I like to put them before the group means so that the group means can be seen clearer
  f.mean$zeMap_dots + # the points of groups
  f.mean$zeMap_text + # the labels of groups
  xlab("Sepal length") + # title of x axis
  ylab("Sepal width") + # title of y axis
  theme() # you can customize your plot more with `theme()` from ggplot

f.final
