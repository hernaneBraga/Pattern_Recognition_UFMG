
# Bibliotecas usadas
rm(list = ls())
source('imagens.R')
source('maxPool.R')
library('kernlab')

require(RnavGraphImageData)

MostraImagem <- function( x, titulo ) {
  rotate <- function(x) t( apply(x, 2, rev) )
  img <- matrix( x, nrow=sqrt(length(x)) )
  cor <- rev( gray(0:50/100) )
  image( rotate( img ), col=cor, xaxt='n',yaxt='n', main = titulo )
  #image( rotate( img ), col=cor, xaxt='n',yaxt='n',ann=FALSE, main = titulo )
  return(img)
}


MostraVetor <- function( x, titulo ) {
  rotate <- function(x) t( apply(x, 2, rev) )
  img <- matrix( x )
  cor <- rev( gray(0:50/100) )
  image( rotate( img ), col=cor, xaxt='n',yaxt='n',main = titulo)
  #image( rotate( img ), col=cor, xaxt='n',yaxt='n',ann=FALSE, main = titulo )
  return(img)
}



# Carregando dados
imagens <- CriaBase()


tf <- 3
#filtros a sere usados:
f <- matrix(c(1,-1,-1,-1,1,-1,-1,-1,1),nrow=tf, ncol = tf)
f2 <- matrix(c(1,-1,1,-1,1,-1,1,-1,1),nrow=tf, ncol = tf)
f3 <- matrix(c(1,1,1,1,-1,-1,1,-1,-1),nrow=tf, ncol = tf)
f4 <- matrix(c(1,1,1,-1,-1,1,-1,-1,1),nrow=tf, ncol = tf)



#1. Convolução: Passar a imagem pelos 4 filtros
#2. Relu: Passar em uma função que transforma em 0 ou 1
#3. Usar max pooling



#######################################
#         Convolução                  #
#######################################

xin <- NULL
vet_img <- NULL

for(i in 1:length(imagens)){
#-----  FIltro 1 -----

img <- imagens[[i]]
#img <- img+f

dimx <- dim(img)[1]
dimy <- dim(img)[2]

MostraImagem(img, "Imagem original")


M1 <- matrix(0, nrow=(dimx - 2), ncol=(dimy -2)) 
M2 <- matrix(0, nrow=(dimx - 2), ncol=(dimy -2)) 
M3 <- matrix(0, nrow=(dimx - 2), ncol=(dimy -2)) 
M4 <- matrix(0, nrow=(dimx - 2), ncol=(dimy -2))

for(l in 1:(dimx - tf)){
  for(c in 1:(dimy -tf)){
    M1[l, c] <- sum(img[l:(l+2), c:(c+2)] *f)
  }
}

MostraImagem(M1, "Imagem após filtro 1")

#-----  FIltro 2 -----



for(l in 1:(dimx - tf)){
  for(c in 1:(dimy -tf)){
    M2[l, c] <- sum(img[l:(l+2), c:(c+2)] *f2)
  }
}

MostraImagem(M2, "Imagem após filtro 2")

#-----  FIltro 3 -----

for(l in 1:(dimx - tf)){
  for(c in 1:(dimy -tf)){
    M3[l, c] <- sum(img[l:(l+2), c:(c+2)] *f3)
  }
}

MostraImagem(M3, "Imagem após filtro 3")


#-----  FIltro 4 -----

for(l in 1:(dimx - tf)){
  for(c in 1:(dimy -tf)){
    M4[l, c] <- sum(img[l:(l+2), c:(c+2)] *f4)
  }
}

MostraImagem(M4, "Imagem após filtro 4")

#######################################
#            Relu                     #
#######################################

M1[M1 < 0] <- 0
M2[M2 < 0] <- 0
M3[M3 < 0] <- 0
M4[M4 < 0] <- 0

MostraImagem(M1, "ReLU - M1")
MostraImagem(M2, "ReLU - M2")
MostraImagem(M3, "ReLU - M3")
MostraImagem(M4, "ReLU - M4")


#######################################
#        Pooling                     #
#######################################
M1 <- maxPool(M1,2)
M2 <- maxPool(M2,2)
M3 <- maxPool(M3,2)
M4 <- maxPool(M4,2)


MostraImagem(M1, "Max Pooling - M1")
MostraImagem(M2, "Max Pooling - M2")
MostraImagem(M3, "Max Pooling - M3")
MostraImagem(M4, "Max Pooling - M4")



#######################################
#        Extração de features         #
#######################################

M <- rbind(M1, M2, M3, M4)

x <- as.vector(t(M))

xin <- rbind(xin, t(x))

vet_img  <- cbind(vet_img, x)


}

#MostraImagem(vet_img[,1:10],"Vetor de características - Treinamento")
#MostraImagem(xin[1:10,],"Vetor de características - 2")

# Treino do modelo
yin <- c("x","x","x","x","x","c","c","c","c","c")

svmtrein <- ksvm(xin[1:10,], yin, type='C-bsvc', kernel='rbfdot', kpar=list(sigma=0.5), C=5)

# Teste
yhat <- predict(svmtrein, t(xin[11,]), type="response")



