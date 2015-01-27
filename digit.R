setwd("~/Developer/work/Kaggle/digit-recognizer")

library(rpart)

load.data <- function() {
  train <- read.csv("data/train.csv", header=TRUE)
  blind <- read.csv("data/test.csv", header=TRUE)
  
  list(train, blind)
}

process.data <- function(data) {
  
}

train.model <- function(train) {
  fit <- rpart(label ~ ., method="class", data=train)  
  plotcp(fit) # visualize cross-validation results 
  fit
}

write.model <- function(fit, test){
  p <- predict(fit, newdata=test, type="class")
  out <- data.frame(cbind(seq(p), levels(p)[p]))
  names(out) <- c("ImageId","Label")
  write.csv(out, 'prediction.csv', quote=F, row.names=F)
}

print.digits <- function(train) {
  mtrain <- as.matrix(train)
  ##Color ramp def.
  colors<-c('white','black')
  cus_col<-colorRampPalette(colors=colors)
  
  pdf('train_letters.pdf')
  par(mfrow=c(4,4),pty='s',mar=c(3,3,3,3),xaxt='n',yaxt='n')
  for(i in 1:nrow(mtrain))
  {
    z<-array(mtrain[i,-1],dim=c(28,28))
    z<-z[,28:1] ##right side up
    image(1:28,1:28,z,main=mtrain[i,1],col=cus_col(256))
  }
  dev.off()
}
