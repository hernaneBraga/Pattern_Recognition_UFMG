#Função de Pooling Máximo

maxPool <- function(X,sizeofpool){
  n <- dim(X)[1]
  m <- dim(X)[2]
  Xout <- NULL
  for (i in 1:(n-1)){
    for (j in 1:(m-1)){
      newm <- matrix(X[i:(i+1),j:(j+1)],nrow=sizeofpool,ncol=sizeofpool)
      Xout <- c(Xout,max(max(newm)))
    }
  }
  Xout <- matrix(data=Xout,nrow=sqrt(length(Xout)),ncol=sqrt(length(Xout)),byrow=TRUE)
  return(Xout)
}