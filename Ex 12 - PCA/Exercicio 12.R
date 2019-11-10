#######################################
# Metodo PCA                          #
#######################################

# Bibliotecas usadas
rm(list = ls())
require(RnavGraphImageData)

MostraImagem <- function( x ) {
  rotate <- function(x) t( apply(x, 2, rev) )
  img <- matrix( x, nrow=64 )
  cor <- rev( gray(50:1/50) )
  image( rotate( img ), col=cor )
  
}
# MostraImagem( faces[1,] )

# Carregando a Base de dados
data( faces )
faces <- t( faces )


#Gerando os rotulos
y <- NULL
for(i in 1:nrow(faces) ) {
 y <- c( y, ((i-1) %/% 10) + 1 )

}


# Nomeando os atributos
nomeColunas <- NULL
for(i in 1:ncol(faces)) {
nomeColunas <- c(nomeColunas, paste("a", as.character(i), sep=".") )

}

colnames(faces) <- nomeColunas  # cada coluna é um atributo
rownames(faces) <- NULL         # cada linha é uma amostra





# normalizando os dados (tirando a média das entradas)
faces_norm <- faces
for( i in 1:length(faces[1,])){
 
  media_coluna <- mean(faces[,i])
  faces_norm[,i] <- faces[,i] - media_coluna
}
  

mtx_cov <- cov(faces_norm)  # Calculando matriz de covariancia
aut <- eigen(mtx_cov)       # Calculando autovalores e autovetores

# Analisando autovalores para escolher nmro de caracteristicas
{
  #plot(aut$values, xlab = 'Índice', ylab = 'Valor') #todos os autovalores
#  title("Autovalores")
  
  #plot(aut$values[10:200], xlab = 'Índice', ylab = 'Valor') # 30 primeiros
  #title("Principais autovalores")
  
}


nmro_caracteristicas <- 5
saida_pca <- faces_norm%*%aut$vectors[,1:nmro_caracteristicas]

# Limpando ambiente para proxima etapa
#rm(faces, faces_norm, aut, mtx_cov)


####################################################
# Classificacao usando saida PCA e metodo KDE      #
####################################################

# Bibliotecas usadas
{
 # library('mlbench')
  #library('roccv')
  #library('corpcor')
  library('kernlab')
  library('caret')
  library('rfUtilities')
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

dados <- saida_pca    # Usando saida do PCA como dados de entrada

n <- nmro_caracteristicas # numero de variaveis de entrada

dados = cbind(dados, y)
  
}



# Definição dos f-folds de treinamento e teste
{
  
  f = 10  #numero de folds

  
  #Lista que armazena acuracia de cada fold
  acuracia <- list()
  desvio_padrao <- list()
  media <- list()
  mtx_confusao <- list()
  
}


# Treino e teste para cada fold f
for( f_i in 1:f){
  
  # Separação de treino e teste
  { 
    
    
  #Dividindo os dados
  fl <- createFolds(dados[,1], k = 2, list = TRUE, returnTrain = FALSE)
  

  datatrain <- dados[-fl[[1]],]
  datatrainclass <- y[-fl[[1]]]
  
  datatest <- dados[fl[[1]],]
  datatestclass <- y[fl[[1]]]
    
  

  }
  
  # Treino do modelo
  svmtrein <- ksvm(datatrain, datatrainclass, type='C-bsvc', kernel='rbfdot', kpar=list(sigma=0.5), C=5)
  
  
  # Teste
  yhat <- predict(svmtrein, datatest, type="response")
  mtx_confusao[[f_i]] <- accuracy(yhat, datatestclass)
  
  
  
}



write.csv(mtx_confusao[[2]]$confusion, "confusao_5")

for( i in 1:10){
  print(paste(i," - ", mtx_confusao[[i]]$PCC))
  
}
mtx_confusao[[6]]$users.accuracy


