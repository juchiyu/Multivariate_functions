#' Title split-half cross validation for PLSC
#'
#' @param data1 Data matrix 1 that entered tepPLS
#' @param data2 Data matrix 2 that entered tepPLS
#' @param pls.res results from tepPLS
#' @param center1 (Default = TRUE) center or not for Data1 in tepPLS
#' @param center2 (Default = TRUE) center or not for Data2 in tepPLS
#' @param scale1 (Default = "SS1") how to scale Data1 in tepPLS
#' @param scale2 (Default = "SS1") how to scale Data2 in tepPLS
#' @param DESIGN (Default = NULL) the vector that describes the design 
#' @param iter Number of iterations
#' that should be kept when creating the folds
#'
#' @return
#' @export
#'
#' @examples
PLS.SplitHalfCV <- function(data1, data2, pls.res, 
                            center1 = TRUE, center2 = TRUE, 
                            scale1 = "SS1", scale2 = "SS1", 
                            DESIGN = NULL, iter = 1000){
  
  ## Check data dimensions
  if (nrow(data1) != nrow(data2)) {
    stop("The number of rows of your two data tables do not match!")
  }
  ## Check pls.res
  if (inherits(pls.res, "tepPLS")) 
    stop("This function only takes pls.res from tepPLS output of the `TExPosition` package!")
  
  ## get index for participants
  ID <- seq(nrow(data1))
  n.ID <- length(ID)
  
  split.design <- list()
  
  ## Create halves
  if (is.null(DESIGN)){ ## if resample with no structure
    for (i in 1:iter){
      ID.order <- sample(ID, size = length(ID), replace = FALSE)
      split.idx <- setNames(
        cut(seq(n.ID), unique(quantile(seq(n.ID), probs = seq(0, 1, length = 3))), 
            include.lowest = TRUE, 
            labels = paste0("Half", seq(2))), 
        ID.order)
      split.design[[i]] <- split.idx[order(ID.order)]
    }
  }else{ ## resample with design
    for (i in 1:iter){
      grp.ID <- split(ID, DESIGN)
      grp.n.ID <- lapply(grp.ID, length)
      grp.ID.order <- lapply(grp.ID, function(x) sample(x, size = length(x), replace = FALSE))
      grp.split.idx <- lapply(grp.n.ID, function(x){
        cut(seq(x), unique(quantile(seq(x), probs = seq(0, 1, length = 3))), 
            include.lowest = TRUE, 
            labels = paste0("Fold", seq(2)))
      })
      grp.split.design <- purrr::map2(grp.split.idx, grp.ID.order, setNames)
      split.idx <- setNames(unlist(grp.split.design, use.names = FALSE), unlist(grp.ID.order, use.names = FALSE))
      split.design[[i]] <- split.idx[order(unlist(grp.ID.order, use.names = FALSE))]
    }
  }
  
  
  ## create empty matrices for 10 folds
  pls.shcv <- list(p.test = list(),
                   p.train = list(),
                   q.test = list(),
                   q.train = list(),
                   ci.test = list(),
                   ci.train = list(),
                   cj.test = list(),
                   cj.train = list(),
                   Dv.test = list(),
                   Dv.train = list(),
                   eig.test = list(),
                   eig.train = list(),
                   cor.res = list())
  
  pls.shcv$cor.res <- list(p = matrix(NA, 
                                      nrow = iter,
                                      ncol = ncol(pls.res$TExPosition.Data$lx)),
                           q = matrix(NA, 
                                      nrow = iter,
                                      ncol = ncol(pls.res$TExPosition.Data$lx)))
  
  ## run PLS
  for (k in 1:iter){
    ## get train and test sets
    train.data1 <- data1[split.design[[k]] != "Half1", ]
    test.data1 <- data1[split.design[[k]] == "Half1", ]
    train.data2 <- data2[split.design[[k]] != "Half1", ]
    test.data2 <- data2[split.design[[k]] == "Half1", ]
    ## run PLS with train set
    train.pls <- tepPLS(train.data1, train.data2,
                        center1 = center1, center2 = center2,
                        scale1 = scale1, scale2 = scale2, graphs = FALSE)
    
    ## Flip the sign if the correlation to the original loadings are negative
    flip <- diag(cor(pls.res$TExPosition.Data$pdq$q, train.pls$TExPosition.Data$pdq$q)) < 0
    train.pls$TExPosition.Data$pdq$p[,flip] <- train.pls$TExPosition.Data$pdq$p[,flip]*-1
    train.pls$TExPosition.Data$pdq$q[,flip] <- train.pls$TExPosition.Data$pdq$q[,flip]*-1
    
    ## run PLS with test set
    test.pls <- tepPLS(test.data1, test.data2,
                       center1 = center1, center2 = center2,
                       scale1 = scale1, scale2 = scale2, graphs = FALSE)
    
    ## Flip the sign if the correlation to the original loadings are negative
    flip <- diag(cor(pls.res$TExPosition.Data$pdq$q, test.pls$TExPosition.Data$pdq$q)) < 0
    test.pls$TExPosition.Data$pdq$p[,flip] <- test.pls$TExPosition.Data$pdq$p[,flip]*-1
    test.pls$TExPosition.Data$pdq$q[,flip] <- test.pls$TExPosition.Data$pdq$q[,flip]*-1
    
    ## run test-train correlation
    ndim <- ncol(test.pls$TExPosition.Data$pdq$p)
    pls.shcv$cor.res$p[k,] <- sapply(1:ndim, function(i) cor(train.pls$TExPosition.Data$pdq$p[, i], test.pls$TExPosition.Data$pdq$p[, i]))
    pls.shcv$cor.res$q[k,] <- sapply(1:ndim, function(i) cor(train.pls$TExPosition.Data$pdq$q[, i], test.pls$TExPosition.Data$pdq$q[, i]))
    
    ## save results from the train set
    pls.shcv$p.train[[k]] <- train.pls$TExPosition.Data$pdq$p
    pls.shcv$q.train[[k]] <- train.pls$TExPosition.Data$pdq$q
    pls.shcv$ci.train[[k]] <- train.pls$TExPosition.Data$pdq$p^2
    pls.shcv$cj.train[[k]] <- train.pls$TExPosition.Data$pdq$q^2
    pls.shcv$Dv.train[[k]] <- train.pls$TExPosition.Data$pdq$Dv
    pls.shcv$eig.train[[k]] <- train.pls$TExPosition.Data$pdq$eigs
    
    pls.shcv$p.test[[k]] <- test.pls$TExPosition.Data$pdq$p
    pls.shcv$q.test[[k]] <- test.pls$TExPosition.Data$pdq$q
    pls.shcv$ci.test[[k]] <- test.pls$TExPosition.Data$pdq$p^2
    pls.shcv$cj.test[[k]] <- test.pls$TExPosition.Data$pdq$q^2
    pls.shcv$Dv.test[[k]] <- test.pls$TExPosition.Data$pdq$Dv
    pls.shcv$eig.test[[k]] <- test.pls$TExPosition.Data$pdq$eigs
    
  }

  return(list(resample.grpvec = split.design,
              cross.validation.res = pls.shcv))
  
}
