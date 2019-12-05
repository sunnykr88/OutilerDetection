
#'Outlier Analysis
#'
#'Takes dataframe and shows the outliers and their log transformation, above and below the benchmarks.
#'@param data = dataframe
#'@author Vikas Yadav and Sunny Kumar
#'@export


outlier<- function(data)
{
  UpperCount <- c()
  LowerCount <- c()
  Log_UpperCount <- c()
  Log_LowerCount <- c()
  names <- c()

  for(i in 1:ncol(data))
  {
    if(is.numeric((data[,i])))
    {
      x <- data[,i][!is.na(data[,i])]
      l <- log(data[,i][!is.na(data[,i])])

      q25 = quantile(x)[2]
      q75 = quantile(x)[4]
      l25 = quantile(l)[2]
      l75 = quantile(l)[4]

      lower <- q25-1.5*(q75-q25)
      upper <- q75+1.5*(q75-q25)
      l_lower <- l25-1.5*(l75-l25)
      l_upper <- l75+1.5*(l75-l25)

      names[i] = names(data[i])
      UpperCount[i] = sum(x>upper)
      LowerCount[i] = sum(x<lower)
      Log_UpperCount[i] = sum(l>l_upper)
      Log_LowerCount[i] = sum(l<l_lower)

    }
    else
    {
      next
    }
  }

  outlierTable = cbind(UpperCount,LowerCount,Log_UpperCount,Log_LowerCount)
  rownames(outlierTable) = names

  print("* Only showing for numerical variables")
  return(outlierTable[complete.cases(outlierTable), ])

}
