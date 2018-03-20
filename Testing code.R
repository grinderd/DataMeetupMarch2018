library(ggplot2)

1+1 

df <- data.frame(x  = rnorm(1000))



ggplot(df,aes(x))+geom_density()
