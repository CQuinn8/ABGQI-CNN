# functions used throughout manusctipt code
# convert rate to logit
toLogit<-function(x){
  lgx<-log(x)-log(1-x)
  return(lgx)
}


# x is the value to logit-transform from
fromLogit<-function(x){
  bt<-exp(x)/(1+exp(x))
  return(bt)
}


# function to pull desired stats : will create new col with original Column_name + "mean" or "var"
mean_var <- list(
  mean = ~mean(.x, na.rm = TRUE),
  var = ~var(.x, na.rm = TRUE)
)