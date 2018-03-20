library(ggplot2)

1+1 

df <- data.frame(x  = rnorm(1000))



ggplot(df,aes(x))+geom_density()


mean(df$x)

df$y = rnorm(1000)*1.5 +5


summary(lm(y~x, data =df))

print(I did fork it right, after all.)