# Código inpirado em: 
# https://www.rdocumentation.org/packages/stats/versions/3.6.1/topics/kmeans

# Bibliotecas usadas ----
rm(list=ls())

# Criação dos dados:
s1<-0.3
s2<-0.3
s3<-0.3
s4<-0.3
nc<-100

xc1<-matrix(rnorm( nc*2 ) , ncol=2)*s1 + t (matrix( c ( 2 , 2 ) ,nrow=2,ncol=nc ) )
xc2<-matrix(rnorm( nc*2 ) , ncol=2)*s2 + t (matrix( c ( 4 , 4 ) ,nrow=2,ncol=nc ) )
xc3<-matrix(rnorm( nc*2 ) , ncol=2)*s3 + t (matrix( c ( 2 , 4 ) ,nrow=2,ncol=nc ) )
xc4<-matrix(rnorm( nc*2 ) , ncol=2)*s4 + t (matrix( c ( 4 , 2 ) ,nrow=2,ncol=nc ) )

dados <- rbind(xc1, xc2, xc3,xc4)
colnames(dados) <- c("x", "y")

k <- 2
kmeans_result <- kmeans(dados, k, iter.max = 10, nstart = 1, 
                algorithm = c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen"), trace=FALSE)

plot(dados, col = kmeans_result$cluster)
