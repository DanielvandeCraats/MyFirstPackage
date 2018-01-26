length <- runif(15,1,50)
width <- runif(15,1,50)
my.df <- data.frame('length'=length,'width'=width)

devtools::use_data(my.df,overwrite = T)
