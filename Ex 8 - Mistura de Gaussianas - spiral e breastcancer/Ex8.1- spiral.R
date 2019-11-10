# Bibliotecas usadas
{
rm(list=ls())
library('mlbench')
library('plot3D')
library('roccv')
library('corpcor')
}

# Funções usadas
pdfnvar <- function(x, m, K, n){ 
  ((1/(sqrt(((2*pi)^n)*(det(K))))) * exp(-0.5*(t(x - m)%*%(pseudoinverse(K))%*%(x - m))))

  } # Função para estimativa da densidade de classe
pdf_cluster <- function(x, n, lst_media, lst_cov, v_amostras, n_amostras_total){
  # x - ponto a ser testado
  # n - número de variáveis de entrada
  # lst_media - lista com os vetores de media de cada cluster
  # lst_cov - lista com as mastrizes de covariância de cada cluster
  # v_amostras - vetor com o número de amostras de cada cluster
  # n_amostras_total - numero de amostras usadas no treino
  
  result <- 0
  
  for( i in 1:length(v_amostras)){

    result <- result + (v_amostras[i]/n_amostras_total)*(pdfnvar(x, lst_media[[i]], lst_cov[[i]], n))
    
    }
  
  return(result)
  
}


# Criação dos dados
{
spiral<-mlbench.spirals(1000,cycles=1, sd=0.05)
dados <- spiral$x

classe1 = 1
classe2 = 2

n <- 2 
idx_cluster <- n+1
idx_classe <- n+2

}

# Clustering e plot
{
k_clusters <- 16
kmeans_result <- kmeans(dados, k_clusters, iter.max = 10, nstart = 1,
                        algorithm = c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen"), trace=FALSE)

dados = cbind(dados, kmeans_result$cluster, spiral$classes)


# Plot de clusters
xmin <- -1.1
xmax <- 1.1
ymin <- -1.1
ymax <- 1.1

plot(dados, col = kmeans_result$cluster, xlab = "X", ylab = "Y", xlim=c(xmin, xmax), ylim=c(ymin, ymax))
par(new=T)
plot( kmeans_result$centers, col="red", pch = 15, lwd =3,  xlab = "X", ylab = "Y", xlim=c(xmin, xmax), ylim=c(ymin, ymax))

#xlab="X-axis label", ylab="y-axix label",
#xlim=c(xmin, xmax), ylim=c(ymin, ymax))

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
for( f_i in 1:f){
  
  # Separação de treino e teste
  {
  xc1train <- xc1[xseqc1!=f_i,]
  xc2train <- xc2[xseqc2!=f_i,]
  xc1tst <- xc1[xseqc1==f_i,]
  xc2tst <- xc2[xseqc2==f_i,]
  }

  # Definição do modelo
  {
    # Vetor com os números de clusters de c1 e c2
    clusters_c1 <- unique( dados[which (dados[, idx_classe] == classe1), idx_cluster])
    clusters_c2 <- unique( dados[which (dados[, idx_classe] == classe2), idx_cluster])
    
    # Vetor com os números de amostras dos clusters c1 e c2
    n_amostas_clusters_c1 <- matrix(0, nrow = length(clusters_c1), ncol = 1)
    n_amostas_clusters_c2 <- matrix(0, nrow = length(clusters_c2), ncol = 1)
    
    media_clusters_c1 <- list(length(clusters_c1))  #lista com os vetores de média de cada cluster
    media_clusters_c2 <- list(length(clusters_c2))  #lista com os vetores de média de cada cluster
    
    cov_clusters_c1 <- list(length(clusters_c1))  #lista com os vetores de média de cada cluster
    cov_clusters_c2 <- list(length(clusters_c2))  #lista com os vetores de média de cada cluster
 
    treino_total <- rbind(xc1train, xc2train) # 
    
    # Calculo para c1
    for(i in 1:length(clusters_c1)){
      #print(i)
      sub_matriz <- subset.matrix(treino_total, treino_total[,idx_cluster] == clusters_c1[i])
      media_clusters_c1[[i]] <- colMeans(sub_matriz[,1:n])
      cov_clusters_c1[[i]] <- cov(sub_matriz[,1:n])
      
      n_amostas_clusters_c1[i] <- length(treino_total[which (treino_total[, idx_cluster] == clusters_c1[i]), idx_cluster])
    }
    
    # Calculo para c2
    for(j in 1:length(clusters_c2) ){
      
      sub_matrizc2 <- subset.matrix(treino_total, treino_total[,idx_cluster] == clusters_c2[j])
      media_clusters_c2[[j]] <- colMeans(sub_matrizc2[,1:n])
      cov_clusters_c2[[j]] <- cov(sub_matrizc2[,1:n])
      
      n_amostas_clusters_c2[j] <- length(treino_total[which (treino_total[, idx_cluster] == clusters_c2[j]), idx_cluster])
    }   
    
  }
  

  # Teste - Classe 1
  Ntstc1 <- length(xc1tst[,1])
  yhatc1<-matrix(nrow=Ntstc1,ncol=1)
  for (j in 1:Ntstc1) {
  
    prob_c1 <- pdf_cluster(xc1tst[j, 1:n], n, media_clusters_c1, cov_clusters_c1, n_amostas_clusters_c1, length(treino_total[,1]))
    prob_c2 <- pdf_cluster(xc1tst[j, 1:n], n, media_clusters_c2, cov_clusters_c2, n_amostas_clusters_c2, length(treino_total[,1]))
  
    aux = prob_c1/prob_c2
    
    if(aux >= 1){  yhatc1[j] <- classe1   }
    else{ yhatc1[j] <- classe2  }
    
  }

  
  # Teste - Classe 2
  Ntstc2 <- length(xc2tst[,1])
  yhatc2<-matrix(nrow=Ntstc2,ncol=1)

  for (j in 1:Ntstc2) {

    prob_c1 <- pdf_cluster(xc2tst[j, 1:n], n, media_clusters_c1, cov_clusters_c1, n_amostas_clusters_c1, length(treino_total[,1]))
    prob_c2 <- pdf_cluster(xc2tst[j, 1:n], n, media_clusters_c2, cov_clusters_c2, n_amostas_clusters_c2, length(treino_total[,1]))
    
    aux = prob_c2/prob_c1
    
    if(aux >= 1){  yhatc2[j] <- classe2   }
    else{ yhatc2[j] <- classe1  }
    
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


# Plot da superfície de contorno
{
seqi<-seq(-1.5, 1.5, 0.01)
seqj<-seq(-1, 1, 0.01)
M1 <- matrix(0,nrow=length(seqi),ncol=length(seqj))

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


    prob_c1 <- pdf_cluster(c(i,j), n, media_clusters_c1, cov_clusters_c1, n_amostas_clusters_c1, length(dados[,1]))
    prob_c2 <- pdf_cluster(c(i,j), n, media_clusters_c2, cov_clusters_c2, n_amostas_clusters_c2, length(dados[,1]))

    aux = prob_c1/prob_c2


    if(aux>=1){  M1[ci,cj] <- classe1   }
    else{ M1[ci,cj] <- classe2  }

  }
}

  contour2D(M1,seqi,seqj,colkey = FALSE, add=TRUE, xlab = "X", ylab = "Y", xlim=c(xmin, xmax), ylim=c(ymin, ymax))
}