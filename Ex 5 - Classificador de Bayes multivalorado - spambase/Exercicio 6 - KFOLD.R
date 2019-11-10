# Bibliotecas usadas ----
rm(list=ls())
library('Rcpp')
library('RSNNS')
library('corpcor')
library('roccv')

# Função para estimativa da densidade de classe com n variáveis ----
pdfnvar <- function(x, m, K, n){ ((1/(sqrt(((2*pi)^n)*(det(K))))) * exp(-0.5*(t(x - m)%*%(pseudoinverse(K))%*%(x - m)))) }


# Função para retornar a maior probabilidade entre as quatro classes ----
retorna_maior<-function(p1,p2,p3,p4) {
  maior <- max(p1,p2,p3,p4)
  if (maior == p1) {return (1)}
  else if(maior == p2) {return (2)}
  else if(maior == p3) {return (3)}
  else  {return (4)}
}

# Carregando a base de dados spambase ----
spambase <- read.csv( "spambase.data", sep=',' ,header = FALSE )

xy <- as.matrix(spambase)
ic1 <- which(xy[,58] == 1) # É spam
ic2 <- which(xy[,58] == 0) # Não é spam

xc1 <- xy[ic1, (1:57)]
xc2 <- xy[ic2, (1:57)]



k = 10
xseqc1 <- randomly_assign(dim(xc1)[1],k)
xseqc2 <- randomly_assign(dim(xc2)[1],k)

taxa_acerto_c1 <- matrix(0, nrow = k, ncol = 1)
taxa_acerto_c2 <- matrix(0, nrow = k, ncol = 1)
taxa_acerto_total <- matrix(0, nrow = k, ncol = 1)

for( i in 1:k){
  
  xc1train <- xc1[xseqc1!=i,]
  xc2train <- xc2[xseqc2!=i,]
  xc1tst <- xc1[xseqc1==i,]
  xc2tst <- xc1[xseqc1==i,]
  
  treinamento_c1_x <- xc1train
  treinamento_c2_x <- xc2train
  
  teste_c1_x <- xc1tst
  teste_c2_x <- xc2tst
  
  n <- 57
  
  
  m1 <- colMeans(treinamento_c1_x)
  m2 <- colMeans(treinamento_c2_x)
  
  K1 <- cov(treinamento_c1_x)
  K2 <- cov(treinamento_c2_x)
  
  tam_teste_1 <- length(teste_c1_x[,1])
  tam_teste_2 <- length(teste_c2_x[,1])
  vetor_resultado_c1 <- matrix(0, nrow = length(tam_teste_1), ncol = 1)
  vetor_resultado_c2 <- matrix(0, nrow = length(tam_teste_2), ncol = 1)
  
  # Aplicando a PDF para cada ponto dos dados de teste das classes 1 e 2----
  # Classe 1
  for (j in seq(1, tam_teste_1, 1) ){
    
    # Calcula a probabilidade do ponto de teste da classe 1 pertencer a C1 ou C2 ----
    p_classe_1 <- pdfnvar(teste_c1_x[j,], m1, K1, n)
    p_classe_2 <- pdfnvar(teste_c1_x[j,], m2, K2, n)
    
    # Trata possiveis erros no cálculo da PDF
    if(p_classe_2 == 0  || is.nan(p_classe_2)){  p_classe_2 = -1  }
    else { if(is.infinite(p_classe_2)) { p_classe_2 = 100  }   }
    
    if(p_classe_1 == 0  || is.nan(p_classe_1)){ p_classe_1 = -1  }
    else {if(is.infinite(p_classe_1)) { p_classe_1 = 100  }   }
    
    # Compara se o ponto pertence as classe 1 ou 2
    vetor_resultado_c1[j]<-retorna_maior(p_classe_1, p_classe_2, -1, -1)
  }
  
  # Classe 2
  for (j in seq(1, tam_teste_2, 1) ){
    
    # Calcula a probabilidade do ponto de teste da classe 1 pertencer a C1 ou C2 ----
    p_classe_1 <- pdfnvar(teste_c2_x[j,], m1, K1, n)
    p_classe_2 <- pdfnvar(teste_c2_x[j,], m2, K2, n)
  
    # Trata possiveis erros no cálculo da PDF
    if(p_classe_2 == 0  || is.nan(p_classe_2)){  p_classe_2 = -1  }
    else { if(is.infinite(p_classe_2)) { p_classe_2 = 100  }   }
    
    if(p_classe_1 == 0  || is.nan(p_classe_1)){ p_classe_1 = -1  }
    else {if(is.infinite(p_classe_1)) { p_classe_1 = 100  }   }
    
    # Compara se o ponto pertence as classe 1, 2, 3 ou 4
    vetor_resultado_c2[j]<-retorna_maior(p_classe_1, p_classe_2, -1, -1)
  }
  
  # Cálculo de acerto de teste da iteração  ----
  acerto_c1 <- length(which(vetor_resultado_c1 == 1))
  acerto_c2 <- length(which(vetor_resultado_c2 == 2))
  
  taxa_acerto_c1[i] <- acerto_c1/tam_teste_1
  taxa_acerto_c2[i] <- acerto_c2/tam_teste_2
  taxa_acerto_total[i] <- (acerto_c1+acerto_c2)/(tam_teste_1+tam_teste_2)
  
}

taxa_acerto_final_c1 <- colMeans(taxa_acerto_c1)
taxa_acerto_final_c2 <- colMeans(taxa_acerto_c2)
taxa_acerto_final_total <- colMeans(taxa_acerto_total)


print('taxa_acerto_c1')
print(taxa_acerto_final_c1)

print('taxa_acerto_c2')
print(taxa_acerto_final_c2)
print('taxa_acerto_total')
print(taxa_acerto_final_total) 










