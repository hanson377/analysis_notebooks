
calcInference <- function(var1,var2){

  ## summary calculations for variable 1
  n1 <- nrow(var1)
  converted1 <- n1*mean(var1$value)
  unconverted1 <- n1-round(converted1,digits=0)

  ## summary calculations for variable 2
  n2 <- nrow(var2)
  converted2 <- n2*mean(var2$value)
  unconverted2 <- n1-round(converted2,digits=0)

  ## model with beta
  model1 <- data.frame(value=rbeta(1000000,converted1+(1/2),unconverted1+(1/2)))
  model2 <- data.frame(value=rbeta(1000000,converted2+(1/2),unconverted2+(1/2)))

  ## calculate probability of model 2 >= model 1 and return

  prob <- sum(model2$value >= model1$value)/nrow(model2)

  ## now, run a simple ttest on the same test and extract the p-value
  pvalue <- t.test(var2,var1,alternative = "greater")$p.value

  return(list(prob,pvalue))

}
