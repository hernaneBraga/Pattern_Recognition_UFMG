#Função retorna uma lista com as 10 fotos pedidas para treinamento. 
#Para acessá-las, coloca dados_treinamento <-CriaBase()
#Foto 1 está em dados_treinamento[[1]], foto 2 está em dados_treinamento[[2]].....
#As 5 pimeiras fotos são do X e as 5 últimas do C

CriaBase <- function(){
  x1 <- matrix(data=-1,nrow=9,ncol=9)
  i1 <- c(2,3,4,5,6,7,8,2,3,4,5,6,7,8)
  j1 <- c(2,3,4,5,6,7,8,8,7,6,5,4,3,2)
  for (n in 1:14){
    x1[i1[n],j1[n]]<-1
  }
  x2 <- matrix(data=-1,nrow=9,ncol=9)
  i2 <- i1+1
  j2 <- j1
  for (n in 1:14){
    x2[i2[n],j2[n]]<-1
  }
  x3 <- matrix(data=-1,nrow=9,ncol=9)
  i3 <- i1
  j3 <- j1+1
  for (n in 1:14){
    x3[i3[n],j3[n]]<-1
  }
  x4 <- matrix(data=-1,nrow=9,ncol=9)
  i4 <- i1+1
  j4 <- j1+1
  for (n in 1:14){
    x4[i4[n],j4[n]]<-1
  }
  x5 <- matrix(data=-1,nrow=9,ncol=9)
  i5 <- i1-1
  j5 <- j1
  for (n in 1:14){
    x5[i5[n],j5[n]]<-1
  }
  c1 <- matrix(data=-1,nrow=9,ncol=9)
  i1 <- c(3,3,3,3,3,4,5,6,7,7,7,7,7)
  j1 <- c(3,4,5,6,7,3,3,3,3,4,5,6,7)
  for (n in 1:13){
    c1[i1[n],j1[n]]<-1
  }
  c2 <- matrix(data=-1,nrow=9,ncol=9)
  i2 <- i1+1
  j2 <- j1
  for (n in 1:13){
    c2[i2[n],j2[n]]<-1
  }
  c3 <- matrix(data=-1,nrow=9,ncol=9)
  i3 <- i1-1
  j3 <- j1
  for (n in 1:13){
    c3[i3[n],j3[n]]<-1
  }
  c4 <- matrix(data=-1,nrow=9,ncol=9)
  i4 <- i1-1
  j4 <- j1+1
  for (n in 1:13){
    c4[i4[n],j4[n]]<-1
  }
  c5 <- matrix(data=-1,nrow=9,ncol=9)
  i5 <- i1-1
  j5 <- j1+2
  for (n in 1:13){
    c5[i5[n],j5[n]]<-1
  }
  
  #Cria Imagem de Teste X
  imgteste <- matrix(-1,ncol=9,nrow=9)
  i <- c(3,3,3,4,4,4,5,5,6,6,6,7,7,7)
  j <- c(2,3,7,3,4,6,4,5,3,5,6,2,6,7)
  for (n in 1:length(i)){
    imgteste[i[n],j[n]]<-1
  }
  
  return(list(x1,x2,x3,x4,x5,c1,c2,c3,c4,c5, imgteste))
}