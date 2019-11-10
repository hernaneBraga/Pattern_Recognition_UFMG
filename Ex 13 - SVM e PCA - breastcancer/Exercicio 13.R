
####################################################
# Classificacao usando saida PCA e metodo KDE      #
####################################################

# Bibliotecas usadas
rm(list = ls())
library('kernlab')
library('caret')
library('rfUtilities')
library(mlbench)
  

data("BreastCancer")
dados <- na.omit(BreastCancer) # elimina dados faltantes
x <- data.matrix(dados[,2:10]) #dados de entrada
y <- data.matrix(dados[,11]) #dados de saida

y[y == "benign"] <- 1
y[y == "malignant"] <- -1
y <- as.numeric(y)


# usando PCA para diminuir as variaveis de entrada
pca <- prcomp(x)
pca$center <- pca$center[order(-pca$center)]


plot(pca$center, xlab = "Variável", ylab = "Autovalor", type="b", main="Auto valores das características")


plot(pca$center, xlab = "Variável", ylab = "Autovalor", type="c", xlim=c(0, 10), ylim=c(1.5, 4.8), main="Auto valores das características")
text(seq(1, length(pca$center), 1), pca$center, names(pca$center))




plot(pca$sdev, xlab = "Variável", ylab = "Variância", type="b", main="Variância das Características")



#Plot de cada característica encontrada por classe
#plot(pca$x[,1], y, col=c("blue", "red")) #plotar aqui


n <- 3 # Numero de caracteristicas escolhida
x <- cbind(pca$x[,1:n]) #Escolha de quais alto vetores usar
dados <- cbind(x,y)



f = 10  #numero de folds

  
#Lista que armazena acuracia de cada fold
csv <- list()
resultados <- list()
  


# Treino e teste para cada fold f
for( f_i in 1:f){
  

  #Separando os folds e Dividindo os dados
  folds <- createFolds(dados[,1], k = 10, list = TRUE, returnTrain = FALSE)
  datatrain <- dados[-folds[[1]],]
  datatest <- dados[folds[[1]],]

  
  # Treino do modelo
  svmtrein <- ksvm(datatrain[,1:n], datatrain[,n+1], type='C-bsvc', kernel='rbfdot', kpar=list(sigma=0.5), C=5)
  
  
  # Teste
  yhat <- predict(svmtrein, datatest[,1:n], type="response")
  resultados[[f_i]] <- accuracy(yhat, datatest[,n+1])
  
}


print("Teste    Maligno[-1](%)    Benigno[1](%)  Acuracia Total(%)")
for( i in 1:10){
  print(paste(i,".", resultados[[i]]$producers.accuracy[[1]], resultados[[i]]$producers.accuracy[[2]], resultados[[i]]$PCC))
  csv[[i]] <- paste(i,",", resultados[[i]]$producers.accuracy[[1]],",", resultados[[i]]$producers.accuracy[[2]], ",", resultados[[i]]$PCC)

}
write.csv(csv, "a")

