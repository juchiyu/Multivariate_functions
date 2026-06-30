library(TExPosition)
# devtools::install_github("HerveAbdi/PTCA4CATA")
library(PTCA4CATA)
# devtools::install_github("HerveAbdi/data4PCCAR")
library(data4PCCAR)
library(dplyr)
library(ggplot2)
## example data from ExPosition
data(beer.tasting.notes)
data1<-beer.tasting.notes$data[,1:8]
data2<-beer.tasting.notes$data[,9:16]

## fake design on rows
row.dx <- c(rep("A", 10), rep("B", 10), rep("C", 10), rep("D", 8))
col4dx <- c("A" = "#F28E2B",
            "B" = "#59A14F",
            "C" = "#D37295",
            "D" = "#B07AA1") # I like color scheme from here: https://jrnold.github.io/ggthemes/reference/tableau_color_pal.html
row.col <- list()
row.col$gc <- col4dx # group colors
row.col$oc <- recode(row.dx, !!!col4dx) # replace row.dx according to col4dx

## visualize your data
corrplot::corrplot(cor(data1, data2), method = "shade")

## This is the line that runs PLSC
pls.res <- tepPLS(data1, data2, 
                  center1 =  TRUE, scale1 = TRUE, # if you center and/or scale the variables of "data1"
                  center2 = TRUE, scale2 = TRUE, # if you center and/or scale the variables of "data2"
                  graphs = FALSE)

pls.boot <- Boot4PLSC(data1, data2, 
                      center1 =  TRUE, scale1 = TRUE, # if you center and/or scale the variables of "data1"
                      center2 = TRUE, scale2 = TRUE, # if you center and/or scale the variables of "data1" 
                      critical.value = 2,
                      nIter = 1000)

pls.perm <- perm4PLSC(data1, data2, 
                  center1 =  TRUE, scale1 = TRUE, # if you center and/or scale the variables of "data1"
                  center2 = TRUE, scale2 = TRUE, # if you center and/or scale the variables of "data2"
                  nIter = 1000, permType = 'byColumns')
#------------------------------------------
## Lx vs. Ly
# The plotting function I generates figures useing the columns of a matrix, and 
# here I'm plotting the first latent variables (that describe the rows, i.e., participants) from both table
# so I combine them into a two-column matrix
lxly <- cbind(pls.res$TExPosition.Data$lx[,1], pls.res$TExPosition.Data$ly[,1]) 
colnames(lxly) <- c(paste0("Dim", 1, c(".X", ".Y")))

## group means --  if you have groups on the rows
# this function runs bootstrap (and compute group means) according to the design specified by a vector (i.e., row.dx)
lxly.boot <- Boot4Mean(lxly, row.dx, niter = 1000)
colnames(lxly.boot$GroupMeans) <- colnames(lxly.boot$BootCube) <- c(paste0("Dim", 1, c(".X", ".Y")))

## plot latent variables
lxly.all <- createFactorMap(lxly,
                            title = paste0("Latent variables"),
                            col.background = NULL,
                            col.axes = "orchid4",
                            alpha.axes = 0.5,
                            col.points = row.col$oc, # You can specify colors for dots here
                            alpha.points = 0.2)

lxly.avg <- createFactorMap(lxly.boot$GroupMeans,
                            col.points = row.col$gc[rownames(lxly.boot$GroupMeans)],
                            col.labels =  row.col$gc[rownames(lxly.boot$GroupMeans)], 
                            pch = 17, alpha.points = 1, text.cex = 5)

lxly.CI <- MakeCIEllipses(lxly.boot$BootCube,
                          col =  row.col$gc[rownames(lxly.boot$BootCube)],
                          names.of.factors = c(paste0("Dim", 1, c(".X", ".Y"))), alpha.ellipse = 0.1, line.size = 0.5)

lxly.all$zeMap_background + lxly.all$zeMap_dots + lxly.CI + lxly.avg$zeMap_dots + lxly.avg$zeMap_text

## barplots for loadings
PrettyBarPlot2(pls.res$TExPosition.Data$fi[,1],
               threshold = 0, 
               color4bar = rep("#4E79A7", nrow(pls.res$TExPosition.Data$fi)),
               font.size = 3, main = "Scores - Data X")

PrettyBarPlot2(pls.res$TExPosition.Data$fj[,1],
               threshold = 0, 
               color4bar = rep("#E15759", nrow(pls.res$TExPosition.Data$fj)),
               font.size = 3, main = "Scores - Data Y")
