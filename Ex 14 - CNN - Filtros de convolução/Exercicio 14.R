#######################################
# Metodo PCA                          #
#######################################

# Bibliotecas usadas
rm(list = ls())
require(RnavGraphImageData)

MostraImagem <- function( x, titulo ) {
  rotate <- function(x) t( apply(x, 2, rev) )
  img <- matrix( x, nrow=sqrt(length(x)) )
  cor <- rev( gray(50:1/50) )
  image( rotate( img ), col=cor, main = titulo )
  return(img)
}
# MostraImagem( faces[1,] )

# Carregando a Base de dados
data( faces )
faces <- t( faces )


#----- Imagem 1 - FIltro 1 -----
img <- MostraImagem(faces[31,], "Imagem original")

dimx <- dim(img)[1]
dimy <- dim(img)[2]

tf <- 3 #filtro 3x3

#Filtro de bordas
f <- matrix(c(-1,-1,-1,-1,8,-1,-1,-1,-1),nrow=3, ncol = 3)
M <- matrix(0, nrow=(dimx - 2), ncol=(dimy -2)) 


for(l in 1:(dimx - tf)){
  for(c in 1:(dimy -tf)){
    M[l, c] <- sum(img[l:(l+2), c:(c+2)] *f)
  }
}

MostraImagem(M, "Filtro de bordas")


#Filtro de linhas verticais
f <- matrix(c(1,2,1,0,0,0,-1,-2,-1),nrow=3, ncol = 3)
M <- matrix(0, nrow=(dimx - 2), ncol=(dimy -2)) 


for(l in 1:(dimx - tf)){
  for(c in 1:(dimy -tf)){
    M[l, c] <- sum(img[l:(l+2), c:(c+2)] *f)
  }
}

MostraImagem(M, "Filtro de linhas verticais")

#Filtro de linhas horizontais
f <- matrix(c(1,0,-1,2,0,-2,1,0,-1),nrow=3, ncol = 3)
M <- matrix(0, nrow=(dimx - 2), ncol=(dimy -2)) 


for(l in 1:(dimx - tf)){
  for(c in 1:(dimy -tf)){
    M[l, c] <- sum(img[l:(l+2), c:(c+2)] *f)
  }
}

MostraImagem(M, "Filtro de linhas horizontais")

#Filtro Sharpen
f <- matrix(c(0,-1,0,-1,5,-1,0,-1,0),nrow=3, ncol = 3)
M <- matrix(0, nrow=(dimx - 2), ncol=(dimy -2)) 


for(l in 1:(dimx - tf)){
  for(c in 1:(dimy -tf)){
    M[l, c] <- sum(img[l:(l+2), c:(c+2)] *f)
  }
}

MostraImagem(M, "Filtro de sharpen")


