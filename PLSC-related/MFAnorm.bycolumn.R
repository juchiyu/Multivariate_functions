MFAnorm.bycolumn <- function (data, column.design, table.preprocess = "MFA_Normalization") 
{
  table.processed <- matrix(0, dim(data)[1], dim(data)[2])
  matrixTotal <- sum(data * data)
  to_total = 0
  from_total = 1
  groupMatrix <- t(makeNominalData(as.matrix(column.design)))
  for (i in 1:dim(groupMatrix)[1]) {
    from = sum(groupMatrix[i - 1, ]) + from_total
    to = sum(groupMatrix[i, ]) + to_total
    to_total = to
    from_total = from
    numColumns <- dim(data[, from:to])[2]
    if (table.preprocess != "None" && table.preprocess != 
        "Num_Columns" && table.preprocess != "Tucker" && 
        table.preprocess != "Sum_PCA" && table.preprocess != 
        "RV_Normalization" && table.preprocess != "MFA_Normalization") {
      print(paste("WARNING: Table preprocessing option not recognized. Sum PCA was set as default"))
      table.preprocess.error = table.preprocess
      table.processed[, from:to] <- data[, from:to]/(sqrt(sum(data[, 
                                                                   from:to] * data[, from:to])))
    }
    if (table.preprocess == "None") {
      table.processed = data
    }
    if (table.preprocess == "Num_Columns") {
      table.processed[, from:to] <- data[, from:to]/(numColumns)
    }
    if (table.preprocess == "Tucker") {
      table.processed[, from:to] <- data[, from:to]/sqrt(numColumns)
    }
    if (table.preprocess == "Sum_PCA") {
      table.processed[, from:to] <- data[, from:to]/(sqrt(sum(data[, 
                                                                   from:to] * data[, from:to])))
    }
    if (table.preprocess == "RV_Normalization") {
      table.processed[, from:to] <- data[, from:to]/sqrt(sum(data[, 
                                                                  from:to] %*% t(data[, from:to])))
    }
    if (table.preprocess == "MFA_Normalization") {
      table.processed[, from:to] = data[, from:to]/(svd(data[, from:to])$d[1])
    }
  }
  if (table.preprocess == "None" || table.preprocess == 
      "Num_Columns" || table.preprocess == "Tucker" || 
      table.preprocess == "Sum_PCA" || table.preprocess == 
      "RV_Normalization" || table.preprocess == "MFA_Normalization") {
    print(paste("Preprocessed the Tables of the data matrix using: ", 
                table.preprocess))
  }
  res.table <- list(table.processed = table.processed)
  return(res.table)
}