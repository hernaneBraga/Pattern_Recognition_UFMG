# Bibliotecas usadas
{
rm(list=ls())
library('mlbench')
library('plot3D')
library('roccv')
library('corpcor')
}

# Funções usadas
kde <- function(x, xi, h, n){
    produto <- 1
    for (i in 1:n){
      produto <- produto * (1/h) * (1/sqrt(2*pi)) * exp(-0.5*((x[i] - xi[i])/h)^2)
    }
    return(produto)
}


# Criação dos dados
{
spiral<-mlbench.spirals(1000,cycles=1, sd=0.05)
dados <- spiral$x

classe1 = 1
classe2 = 2

n <- 2 

idx_classe <- n+1


dados = cbind(dados, spiral$classes)

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

# Treino e teste para cada fold f
for( f_i in 1:10){

  # Separação de treino e teste
  {
  xc1train <- xc1[xseqc1!=f_i,]
  xc2train <- xc2[xseqc2!=f_i,]
  xc1tst <- xc1[xseqc1==f_i,]
  xc2tst <- xc2[xseqc2==f_i,]
  }

  # Definição do modelo
  {
    
    h <- 0.25
    Ntreino1 <- length(xc1train[,1])
    Ntreino2 <- length(xc2train[,1])
    
  }
  

  # Teste - Classe 1
{
  Ntstc1 <- length(xc1tst[,1])
  yhatc1<-matrix(nrow=Ntstc1,ncol=1)
  vero_c1<-matrix(nrow=Ntstc1,ncol=2)
  
  kde_c1_tst1 <- matrix(nrow=Ntstc1,ncol=1)
  kde_c2_tst1 <- matrix(nrow=Ntstc1,ncol=1)
  
  
  for (j in 1:Ntstc1) {
    kde_c1_tst1[j,] <- 0
    kde_c2_tst1[j,] <- 0
    for(z in 1:Ntreino1){
      kde_c1_tst1[j,]  <- kde_c1_tst1[j,] + kde(xc1train[z, 1:n], xc1tst[j, 1:n], h, n)
      kde_c2_tst1[j,]  <- kde_c2_tst1[j,] + kde(xc2train[z, 1:n], xc1tst[j, 1:n], h, n)
    }
    
    prob_c1 <- (1/Ntreino1)*kde_c1_tst1[j,]
    prob_c2 <- (1/Ntreino2)*kde_c2_tst1[j,]
    
    vero_c1[j,] <- c(prob_c1,prob_c2)
    aux = prob_c1/prob_c2
    
    if(aux >= 1){  yhatc1[j] <- classe1   }
    else{ yhatc1[j] <- classe2  }
    
  }
}
  
  # Teste - Classe 2
{
  Ntstc2 <- length(xc2tst[,1])
  yhatc2<-matrix(nrow=Ntstc2,ncol=1)
  vero_c2<-matrix(nrow=Ntstc2,ncol=2)
  
  kde_c1_tst2 <- matrix(nrow=Ntstc1,ncol=1)
  kde_c2_tst2 <- matrix(nrow=Ntstc1,ncol=1)
  
  for (j in 1:Ntstc2) {
    kde_c1_tst2[j,] <- 0
    kde_c2_tst2[j,] <- 0
    for(z in 1:Ntreino2){
      kde_c1_tst2[j,] <- kde_c1_tst2[j,] + kde(xc1train[z, 1:n], xc2tst[j, 1:n], h, n)
      kde_c2_tst2[j,] <- kde_c2_tst2[j,] + kde(xc2train[z, 1:n], xc2tst[j, 1:n], h, n)
    }
    
    prob_c1 <- (1/Ntreino1)*kde_c1_tst2[j,]
    prob_c2 <- (1/Ntreino2)*kde_c2_tst2[j,]
    
    vero_c2[j,] <- c(prob_c1,prob_c2)
    
    aux = prob_c2/prob_c1
    
    if(aux >= 1){  yhatc2[j] <- classe2   }
    else{ yhatc2[j] <- classe1  }
    
  }
}

  # Cálculo de acurárica e desvio padrão
  {
    acerto_c1 <- length(which(yhatc1 == classe1)) #Qtde de classificacoes corretas para c1
    acerto_c2 <- length(which(yhatc2 == classe2)) #Qtde de classificacoes corretas para c2
    
    acuracia_fold[f_i,1] <- acerto_c1/Ntstc1    # Taxa de acerto c1
    acuracia_fold[f_i,2]  <- acerto_c2/Ntstc2   # Taxa de acerto c2
    acuracia_fold[f_i,3]<- (acerto_c1+acerto_c2)/(Ntstc1+Ntstc2)
    
    
    sd_fold[f_i,1] <- sd(yhatc1)
    sd_fold[f_i,2] <- sd(yhatc2)
    sd_fold[f_i,3] <- sd(rbind(yhatc1,yhatc2))
    
  }

}

# Acurácia média e desvio padrão
{
  
acuracia_total <- c(mean(acuracia_fold[,1]), mean(acuracia_fold[,2]), 
                    mean(acuracia_fold[,3]) )

  
}
# 
# # Plot dos dados de teste no espaço das verossimelhanças:
{
  xmin <- 0.08
  ymin <- 0.05
  xmax <- 0.2
  ymax <- 0.2

  plot(vero_c1, col="black",xlab = "P(x|C_1)", ylab = "P(x|C_2)", xlim=c(xmin, xmax), ylim=c(ymin, ymax))
  par(new=T)
  plot(vero_c2, col="red",xlab = "P(x|C_1)", ylab = "P(x|C_2)", xlim=c(xmin, xmax), ylim=c(ymin, ymax))
  title(paste("Espaço das Verossimilhanças h =", h))
  }

# # Plot da superfície de contorno
{
  seqi<-seq(-1.5, 1.5, 0.01)
  seqj<-seq(-1, 1, 0.01)
  M1 <- matrix(0,nrow=length(seqi),ncol=length(seqj))

  kde_c1 <- matrix(nrow=length(seqi),ncol=1)
  kde_c2 <- matrix(nrow=length(seqi),ncol=1)

  Gaussiana_plot <- matrix(0,nrow=length(seqi), ncol=length(seqj))
  Gaussiana_plot_2 <- matrix(0,nrow=length(seqi), ncol=length(seqj))
  
  # Cálculo de probabilidade pro grid
  ci<-0
  for (i in seqi){
    ci<-ci+1
    cj<-0
    for(j in seqj)
    {
      cj<-cj+1

      # Cálculo se o ponto tem probabilidade de estar na classe 1 e na classe 2

      # Compara qual classe é provável do ponto pertencer


      kde_c1[cj,] <- 0
      kde_c2[cj,] <- 0
      for(z in 1:Ntreino1){
        kde_c1[cj,] <- kde_c1[cj,] + kde(xc1train[z, 1:n], c(i, j), h, n)
        kde_c2[cj,] <- kde_c2[cj,] + kde(xc2train[z, 1:n], c(i, j), h, n)
      }


      prob_c1 <- (1/Ntreino1)*kde_c1[cj,]
      prob_c2 <- (1/Ntreino2)*kde_c2[cj,]

      Gaussiana_plot[ci,cj]<- prob_c1
      Gaussiana_plot_2[ci,cj]<- prob_c2
      
      aux = prob_c1/prob_c2


      if(aux>=1){  M1[ci,cj] <- classe1   }
      else{ M1[ci,cj] <- classe2  }

    }
  }

  plot(spiral)
  contour2D(M1,seqi,seqj,colkey = FALSE, add=TRUE, xlab = "X", ylab = "Y", xlim=c(xmin, xmax), ylim=c(ymin, ymax))
  title("Superficie de contorno")
  
  persp3D(seqi,seqj,M1,counter=T,theta = 20, phi = 60, r = 40, d = 0.1, expand = 0.5,
          ltheta = 90, lphi = 180, shade = 0.4, ticktype = "detailed", nticks=5)
  title("Superficie de separação")
  
  persp3D(seqi,seqj,Gaussiana_plot,counter=T,theta = 20, phi = 60, r = 40, d = 0.1, expand = 0.5,
          ltheta = 90, lphi = 180, shade = 0.4, ticktype = "detailed", nticks=5)
  title("Superficie de densidade - Classe 1")
  persp3D(seqi,seqj,Gaussiana_plot_2,counter=T,theta = 20, phi = 60, r = 40, d = 0.1, expand = 0.5,
          ltheta = 90, lphi = 180, shade = 0.4, ticktype = "detailed", nticks=5)
  title("Superficie de densidade - Classe 2")
  
}



