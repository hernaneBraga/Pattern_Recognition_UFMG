# Código inpirado em
# https://www.rdocumentation.org/packages/stats/versions/3.6.1/topics/kmeans

# Bibliotecas usadas ----
rm(list=ls())
library('mlbench')
library('plot3D')

# Função para estimativa da densidade de classe
pdfnvar <- function(x, m, K, n){ ((1/(sqrt(((2*pi)^n)*(det(K))))) * exp(-0.5*(t(x - m)%*%(solve(K))%*%(x - m)))) }


# Criação dos dados ----
spiral<-mlbench.spirals(1000,cycles=1, sd=0.05)
plot(spiral)

dados <- spiral$x
colnames(dados) <- c("x", "y")

# Clustering ----
k_clusters <- 15
kmeans_result <- kmeans(dados, k_clusters, iter.max = 10, nstart = 1, 
                algorithm = c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen"), trace=FALSE)

dados = cbind(dados, kmeans_result$cluster)


# Plot de clusters ----
#Fonte: http://www.sthda.com/english/wiki/r-plot-pch-symbols-the-different-point-shapes-available-in-r
plot(dados, col = kmeans_result$cluster)
par(new=T)
plot( kmeans_result$centers, col="red", pch = 15, lwd =3 )


# Plot da Gaussiana de cada cluster ----
for(k in seq(1, k_clusters, 1) )
{
  sub_matriz <- subset.matrix(dados, dados[,3] == k)
  
  media <- colMeans(sub_matriz[,1:2])
  K_cov <- cov(sub_matriz[,1:2])
  n_variaveis <- 2
  
  seqi<-seq( -1, 1, 0.02)
  seqj<-seq( -1, 1, 0.02)
  
  Gaussiana_plot <- matrix(0,nrow=length(seqi), ncol=length(seqj))
  
  ci<-0
  for (i in seqi){
    ci<-ci+1
    cj<-0
    for(j in seqj)
    {
      cj<-cj+1
      Gaussiana_plot[ci,cj]<- pdfnvar(c(i,j),media, K_cov, n_variaveis)
    }
  }
  
  flag <- FALSE
  if ( k != 1){ flag <- TRUE }
  
  persp3D(seqi,seqj,Gaussiana_plot,counter=T,theta = 20, phi = 60, r = 40, d = 0.1, expand = 0.5,
          ltheta = 90, lphi = 180, shade = 0.4, ticktype = "detailed", nticks=5, add = flag)

}


