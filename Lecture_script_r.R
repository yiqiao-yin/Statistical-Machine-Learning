################# BEGIN OF SCRIPT ##########################

# Copyright @ Yiqiao Yin

# This is lecture notes in r script for statistical 
# machine learning by Liu at Columbia University. 

################# BEGIN OF SCRIPT ##########################

rm(list=ls()) # Clear all variables

setwd("F:/course/CU Stats/STATS W4241(S) - Statistical Machine Learning/7. Data")
zip <- read.table("zip.train", header = FALSE, sep='')

x_train <- as.matrix(zip[,-1])

output.image <- function(vector) {
  digit <- matrix(vector, nrow=16, ncol=16)
  index = seq(from=16,to=1,by=-1)
  sym_digit = digit[,index]
  image(digit, col=gray((8:0)/8), axes=FALSE)
}

output.image(x_train[1,])

par(mfrow=c(3,3), mai=c(0,1,0,1,0,1,0,1))


################# END OF SCRIPT ##########################
