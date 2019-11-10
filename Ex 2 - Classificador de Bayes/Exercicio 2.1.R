rm(list=ls())
library('plot3D')
library('RSNNS')

#--------------------------------

# Tem-se como objetivo:
# 1. Criar o conjunto de dados conforme descrito no exercicio;
# 2. Separar os dados em um conjunto de treinamento com 90% dos dados e um conjunto de testes com 10% dos dados de forma aleatória;
# 3. Fazer o treinamento do classificador utilizando os dados de treinamento;
# 4. Aplique o classificador ao conjunto de treinamento;

# definição dos dados duas dimensoes
s1<-0.8
s2<-0.4
nc<-200
p_user<-0
xc1<-matrix(rnorm(nc*2),ncol=2)*s1 + t(matrix((c(2,2)),ncol=nc,nrow=2))
xc2<-matrix(rnorm(nc*2),ncol=2)*s2 + t(matrix((c(4,4)),ncol=nc,nrow=2))
plot(xc1[,1],xc1[,2],col = ' red ' , xlim = c(0,6),ylim = c(0,6),xlab = ' xs ' ,ylab= ' y ' )
par(new=T)
plot(xc2[,1],xc2[,2],col = ' blue ' , xlim = c(0,6),ylim = c(0,6),xlab = '' ,ylab= '' )

#Dividir os dados em 90/10, onde 10% são para testes e aleatórios
#Classe c1
dados_c1 <- splitForTrainingAndTest(xc1[,1] , xc1[,2], ratio = 0.1)
treinamento_c1_x <- cbind(dados_c1$inputsTrain)
treinamento_c1_y <- cbind(dados_c1$targetsTrain)
teste_c1_x <- cbind(dados_c1$inputsTest)
teste_c1_y <- cbind(dados_c1$targetsTest)

#Classe c2
dados_c2 <- splitForTrainingAndTest(xc2[,1] , xc2[,2], ratio = 0.1)
treinamento_c2_x <- cbind(dados_c2$inputsTrain)
treinamento_c2_y <- cbind(dados_c2$targetsTrain)
teste_c2_x <- cbind(dados_c2$inputsTest)
teste_c2_y <- cbind(dados_c2$targetsTest)


#inputsTrain = valor de x de treinamento
#targetsTrain = valor de y de treinamento

#inputsTest = valor de x de treinamento
#targetsTest = valor de y de treinamento

# Criar uma matriz de 'grid' para plotar o resultado. No nosso caso, uma matriz de 6x6 com distância de 0,01 (?)
# Para cada ponto da matriz, calcular se o ponto pertence a c1 ou c2 usando a função pdf2var
# Plotar a superfície de contorno para o grid

# ------------------------------------------
# PLOT DOS DADOS DE TREINAMENTO E DE TESTE
# TREINAMENTO
plot(treinamento_c1_x[,1],treinamento_c1_y[,1],col = ' red ' , xlim = c(0,6),ylim = c(0,6),xlab = ' xs ' ,ylab= ' y ' )
par(new=T)
plot(treinamento_c2_x[,1],treinamento_c2_y[,1],col = ' blue ' , xlim = c(0,6),ylim = c(0,6),xlab = '' ,ylab= '' )


# TESTE
plot(teste_c1_x[,1],teste_c1_y[,1],col = ' red ' , xlim = c(0,6),ylim = c(0,6),xlab = ' xs ' ,ylab= ' y ' )
par(new=T)
plot(teste_c2_x[,1],teste_c2_y[,1],col = ' blue ' , xlim = c(0,6),ylim = c(0,6),xlab = '' ,ylab= '' )

# ------------------------------------------

#funcao para estimativa da densidade de 2 variáveis
pdf2var<-function(x,y,u1,u2,s1,s2,p) {(1/(2*pi*s1*s2*sqrt(1-(p^2))))*
    exp((-(1)/(2*(1-(p^2))))*((((x-u1)^2)/((s1)^2))+(((y-u2)^2)/((s2)^2))-
                                ((2*p*(x-u1)*(y-u2))/(s1*s2))))
}

#cáculo da média e desvio padrão da classe 1
c1_ux<-mean(treinamento_c1_x)
c1_uy<-mean(treinamento_c1_y)
c1_sx<-sd(treinamento_c1_x)
c1_sy<-sd(treinamento_c1_y)

#cáculo da média e desvio padrão da classe 2
c2_ux<-mean(treinamento_c2_x)
c2_uy<-mean(treinamento_c2_y)
c2_sx<-sd(treinamento_c2_x)
c2_sy<-sd(treinamento_c2_y)


#Aplicando a PDF para cada ponto dos dados de teste da classe 1
vetor_resultado_c1 <- matrix(0, nrow = length(teste_c1_x), ncol = 1)
for (i in seq(1,length(teste_c1_x),1)){
  
    # Parâmentros da função: pdf2var<-function(x,y,u1,u2,s1,s2,p) 
    
    # Cálculo se o ponto tem probabilidade de estar na classe 1 e na classe 2
    p_classe_1<- pdf2var(teste_c1_x[i,1],teste_c1_y[i,1],c1_ux,c1_uy,c1_sx,c1_sy,p_user)
    p_classe_2<- pdf2var(teste_c1_x[i,1],teste_c1_y[i,1],c2_ux,c2_uy,c2_sx,c2_sy,p_user)
    
    # Compara se o ponto pertence a classe 1 ou 2
    if(p_classe_1 > p_classe_2){
        vetor_resultado_c1[i,1]<-1
    }
    else{
        vetor_resultado_c1[i,1]<-2
    }
}

#Aplicando a PDF para cada ponto dos dados de teste da classe 2
vetor_resultado_c2 <- matrix(0, nrow = length(teste_c2_x), ncol = 1)
for (i in seq(1,length(teste_c2_x),1)){
  
  # Parâmentros da função: pdf2var<-function(x,y,u1,u2,s1,s2,p) 
  
  # Cálculo se o ponto tem probabilidade de estar na classe 1 e na classe 2
  p_classe_1<- pdf2var(teste_c2_x[i,1],teste_c2_y[i,1],c1_ux,c1_uy,c1_sx,c1_sy,p_user)
  p_classe_2<- pdf2var(teste_c2_x[i,1],teste_c2_y[i,1],c2_ux,c2_uy,c2_sx,c2_sy,p_user)
  
  # Compara se o ponto pertence a classe 1 ou 2
  if(p_classe_1 > p_classe_2){
    vetor_resultado_c2[i,1]<-1
  }
  else{
    vetor_resultado_c2[i,1]<-2
  }
}

#---------------------

#estimando as densidades em cada ponto de um grid
seqi<-seq(0.06,6,0.06)
seqj<-seq(0.06,6,0.06)
M1 <- matrix(0,nrow=length(seqi),ncol=length(seqj)) 

ci<-0
for (i in seqi){
  ci<-ci+1
  cj<-0
  for(j in seqj)
  {
    cj<-cj+1
    
    # Cálculo se o ponto tem probabilidade de estar na classe 1 e na classe 2
    p_classe_1<- pdf2var(i,j,c1_ux,c1_uy,c1_sx,c1_sy,p_user)
    p_classe_2<- pdf2var(i,j,c2_ux,c2_uy,c2_sx,c2_sy,p_user)
    
    # Compara se o ponto pertence a classe 1 ou 2
    if(p_classe_1>p_classe_2){
      M1[ci,cj]<- 1
    }
    else{
      M1[ci,cj]<- 2
    }
    
  }
}


#plotando as densidades

persp3D(seqi,seqj,M1,counter=T,theta = 55, phi = 30, r = 40, d = 0.1, expand = 0.5,
        ltheta = 90, lphi = 180, shade = 0.4, ticktype = "detailed", nticks=5)



#Plotando os pontos novamente para gráfico final
plot(xc1[,1],xc1[,2],col = ' red ' , xlim = c(0,6),ylim = c(0,6),xlab = ' xs ' ,ylab= ' y ' )
par(new=T)
plot(xc2[,1],xc2[,2],col = ' blue ' , xlim = c(0,6),ylim = c(0,6),xlab = '' ,ylab= '' )
par(new=T)
#plotando as superfícies de contorno
contour2D(M1,seqi,seqj,colkey = FALSE, add=TRUE)


