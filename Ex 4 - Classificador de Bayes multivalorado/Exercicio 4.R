# Bibliotecas usadas ----
rm(list=ls())
library('Rcpp')
library('RSNNS')


# Função para estimativa da densidade de classe com n variáveis ----
pdfnvar <- function(x, m, K, n){ ((1/(sqrt(((2*pi)^n)*(det(K))))) * exp(-0.5*(t(x - m)%*%(solve(K))%*%(x - m)))) }


# Função para retornar a maior probabilidade entre as quatro classes ----
retorna_maior<-function(p1,p2,p3,p4) {
  maior <- max(p1,p2,p3,p4)
  if (maior == p1) {return (1)}
  else if(maior == p2) {return (2)}
  else if(maior == p3) {return (3)}
  else  {return (4)}
}


# Carregando a base de dados heart ----
heart <- read.csv( "heart.dat", sep='' ,header = FALSE )

nc <- length(heart[,14])
perc_treino <- 0.9

xy <- as.matrix(heart)
ic1 <- which(xy[,14] == 1)
ic2 <- which(xy[,14] == 2)

xc1 <- xy[ic1, (1:13)]
xc2 <- xy[ic2, (1:13)]


# Dividir os dados em 90/10, onde 10% são para testes e aleatórios ----
n_xc1 <- length(xc1[,13])
n_xc2 <- length(xc2[,13])



# Classe 1
Ntrain_c1 <- round(n_xc1 * perc_treino)
Nteste_c1 <- n_xc1 - Ntrain_c1

seqc1 <- sample(length(xc1[,1]))
treinamento_c1_x <-xc1[seqc1[(1:Ntrain_c1)],]
teste_c1_x <-xc1[seqc1[(Ntrain_c1+1):length(xc1[,1])],]


# Classe 2
Ntrain_c2 <- round(n_xc2 * 0.9)
Nteste_c2 <- n_xc2 - Ntrain_c2

seqc2 <- sample(length(xc2[,1]))
treinamento_c2_x <-xc2[seqc2[(1:Ntrain_c2)],]
teste_c2_x <-xc2[seqc2[(Ntrain_c2+1):length(xc2[,1])],]


# Cálculo da média e desvio padrão de cada classe ----
# pdfnvar <- function(x, m, K, n)
# x é a linha inteira
# m é vetor de médias
# K é cov()
# n é o nº de caracteristicas = 14

# Cálculo do vetor de médias das classes c1 e c2
n <- 13


m1 <- colMeans(treinamento_c1_x)
m2 <- colMeans(treinamento_c2_x)

K1 <- cov(treinamento_c1_x)
K2 <- cov(treinamento_c2_x)


# Aplicando a PDF para cada ponto dos dados de teste das classes 1, 2, 3, 4 ----
tam_teste_1 <- length(teste_c1_x[,1])
tam_teste_2 <- length(teste_c2_x[,1])
vetor_resultado_c1 <- matrix(0, nrow = length(tam_teste_1), ncol = 1)
vetor_resultado_c2 <- matrix(0, nrow = length(tam_teste_2), ncol = 1)

# Classe 1
for (i in seq(1, tam_teste_1, 1) ){
  
    # Calcula a probabilidade do ponto de teste da classe 1 pertencer a C1 ou C2 ----
    p_classe_1 <- pdfnvar(teste_c1_x[i,], m1, K1, n)
    p_classe_2 <- pdfnvar(teste_c1_x[i,], m2, K2, n)
   
    # Compara se o ponto pertence as classe 1, 2, 3 ou 4
    vetor_resultado_c1[i]<-retorna_maior(p_classe_1, p_classe_2, -1, -1)
}

# Classe 2
for (i in seq(1, tam_teste_2, 1) ){
  
  # Calcula a probabilidade do ponto de teste da classe 1 pertencer a C1 ou C2 ----
  p_classe_1 <- pdfnvar(teste_c2_x[i,], m1, K1, n)
  p_classe_2 <- pdfnvar(teste_c2_x[i,], m2, K2, n)
  
  # Compara se o ponto pertence as classe 1, 2, 3 ou 4
  vetor_resultado_c2[i]<-retorna_maior(p_classe_1, p_classe_2, -1, -1)
}

# Cálculo de acerto de teste ----
acerto_c1 <- length(which(vetor_resultado_c1 == 1))
acerto_c2 <- length(which(vetor_resultado_c2 == 2))


taxa_acerto_c1 <- acerto_c1/tam_teste_1
taxa_acerto_c2 <- acerto_c2/tam_teste_2
taxa_acerto_total <- (acerto_c1+acerto_c2)/(tam_teste_1+tam_teste_2)
  
print(taxa_acerto_c1)
print(taxa_acerto_c2)
print(taxa_acerto_total)