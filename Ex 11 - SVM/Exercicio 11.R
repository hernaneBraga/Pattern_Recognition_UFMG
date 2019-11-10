# Bibliotecas usadas
{
rm(list=ls())
library('mlbench')
library('plot3D')
library('roccv')
library('kernlab')
}


# Criação dos dados
{
spiral<-mlbench.spirals(1000,cycles=1, sd=0.05)
dados <- spiral$x

classe1 = 1
classe2 = 2

n <- 2 

dados = cbind(dados, spiral$classes)
idx_classe <- n+1
}


# Definição dos f-folds de treinamento e teste
{

f = 10  #numero de folds

ic1 <- which(dados[,idx_classe] == classe1) # Classe 1
ic2 <- which(dados[,idx_classe] == classe2) # Classe 2

xc1 <- dados[ic1, (1:idx_classe)]
xc2 <- dados[ic2, (1:idx_classe)]

xseqc1 <- randomly_assign(dim(xc1)[1],f)
xseqc2 <- randomly_assign(dim(xc2)[1],f)

#Lista que armazena acuracia de cada fold
acuracia_fold <- matrix(nrow=f,ncol=3)
sd_fold <- matrix(nrow=f,ncol=3)

}

f_i <- 1

# Treino e teste para cada fold f
#for( f_i in 1:1){

  # Separação de treino e teste
  {
  xc1train <- xc1[xseqc1!=f_i,]
  xc2train <- xc2[xseqc2!=f_i,]
  
  # Pontos usados no treinamento da svm
  xin <- rbind(xc1train[ ,1:n], xc2train[ ,1:n])
  yin <- c(xc1train[ ,n+1], xc2train[ ,n+1])
  
  xc1tst <- xc1[xseqc1==f_i,]
  xc2tst <- xc2[xseqc2==f_i,]
  xtest <- rbind(xc1tst, xc2tst)
  }

  # Treino do modelo
  svmtrein <- ksvm(xin,yin,type='C-bsvc',kernel='rbfdot',kpar=list(sigma=0.5),C=10)
  
  
  # Teste
  yhat <- predict(svmtrein, xtest[ , -(n+1)], type="response")
  table(yhat,xtest[ , n+1])

  
  #Plot dos vetores de suporte
  a <- alpha(svmtrein)
  ai <- SVindex(svmtrein)
  nsvec = nSV(svmtrein)
  plot(spiral)
  par(new=T)
  points(xin[ai,1], xin[ai,2], col = "purple") # vetores de suporte
  title("Vetores de suporte")



  # Plot da superfície de contorno e superfício de superação
  xmin <- -1.5
  xmax <- 1.5
  ymin <- -1.5
  ymax <- 1.5
  
  seqi<-seq(-1.5,1.5,0.05)
  seqj<-seq(-1.5,1.5,0.05)
  M1 <- matrix(0,nrow=length(seqi),ncol=length(seqj)) 
  
  ci<-0
  for (i in seqi){
    ci<-ci+1
    cj<-0
    for(j in seqj)
    {
      cj<-cj+1
      M1[ci,cj]<-predict(svmtrein,as.matrix(cbind(i,j)),type="response")
      
    }
  }
  
  

  #plotando a separação
  plot(spiral, xlim=c(xmin, xmax), ylim=c(ymin, ymax))
  par(new=T)
  contour2D(M1,seqi,seqj,colkey = FALSE, add=TRUE, xlab = "X", ylab = "Y", xlim=c(xmin, xmax), ylim=c(ymin, ymax))
  title("Superficie de contorno")
  
  
  persp3D(seqi,seqj,M1,counter=T,theta = 55, phi = 30, r = 40, d = 0.1, expand = 0.5,
          ltheta = 90, lphi = 180, shade = 0.4, ticktype = "detailed", nticks=5)
  title("Superficie de separação")
  


