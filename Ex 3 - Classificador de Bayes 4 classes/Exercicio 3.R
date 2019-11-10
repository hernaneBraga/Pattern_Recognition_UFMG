# Bibliotecas usadas ----
rm(list=ls())
library('plot3D')
library('RSNNS')


# Função para estimativa da densidade de classe ----
pdf2var<-function(x,y,u1,u2,s1,s2,p) {(1/(2*pi*s1*s2*sqrt(1-(p^2))))*
    exp((-(1)/(2*(1-(p^2))))*((((x-u1)^2)/((s1)^2))+(((y-u2)^2)/((s2)^2))-
                                ((2*p*(x-u1)*(y-u2))/(s1*s2))))
}

# Função para retornar a maior probabilidade entre as quatro classes ----
retorna_maior<-function(p1,p2,p3,p4) {
  maior <- max(p1,p2,p3,p4)
  if (maior == p1) {return (1)}
  else if(maior == p2) {return (2)}
  else if(maior == p3) {return (3)}
  else  {return (4)}
}


# Definição do nº de amostras, dados de variância e correlação das classes ----
s1<-0.6
s2<-0.8
s3<-0.2
s4<-1
nc<-200
p_user<-0 #Correlação nula


# Plot inicial dos dados das 4 classes ----
vermelho<-matrix(rnorm(nc*2),ncol=2)*s1 + t(matrix((c(2,2)),ncol=nc,nrow=2))
plot(vermelho[,1] ,vermelho[,2],col = ' red ' , xlim = c(0,6),ylim = c(0,6),xlab = ' xs ' ,ylab= ' y ' )

par(new=T)
azul<-matrix(rnorm(nc*2),ncol=2)*s2 + t(matrix((c(4,4)),ncol=nc,nrow=2))
plot(azul[,1] ,azul[,2],col = ' blue ' , xlim = c(0,6),ylim = c(0,6),xlab = ' xs ' ,ylab= ' y ' )

par(new=T)
verde<-matrix(rnorm(nc*2),ncol=2)*s3 + t(matrix((c(2,4)),ncol=nc,nrow=2))
plot(verde[,1] ,verde[,2],col = ' green ' , xlim = c(0,6),ylim = c(0,6),xlab = ' xs ' ,ylab= ' y ' )

par(new=T)
preto<-matrix(rnorm(nc*2),ncol=2)*s4 + t(matrix((c(4,2)),ncol=nc,nrow=2))
plot(preto[,1] ,preto[,2],col = ' black ' , xlim = c(0,6),ylim = c(0,6),xlab = ' xs ' ,ylab= ' y ' )


# Atribuindo os dados para cada classe ----
xc1 <- vermelho
xc2 <- azul
xc3 <- verde
xc4 <- preto


# Dividir os dados em 90/10, onde 10% são para testes e aleatórios ----
Ntrain <- round(nc * 0.9)
Nteste <- nc - Ntrain

# Classe 1
seqc1 <- sample(length(xc1[,1]))
treinamento_c1_x <-xc1[seqc1[(1:Ntrain)],1]
treinamento_c1_y <-xc1[seqc1[(1:Ntrain)],2]
teste_c1_x <-xc1[seqc1[(Ntrain+1):length(xc1[,1])],1]
teste_c1_y <-xc1[seqc1[(Ntrain+1):length(xc1[,1])],2]

# Classe 2
seqc2 <- sample(length(xc2[,1]))
treinamento_c2_x <-xc2[seqc2[(1:Ntrain)],1]
treinamento_c2_y <-xc2[seqc2[(1:Ntrain)],2]
teste_c2_x <-xc2[seqc2[(Ntrain+1):length(xc2[,1])],1]
teste_c2_y <-xc2[seqc2[(Ntrain+1):length(xc2[,1])],2]

# Classe 3
seqc3 <- sample(length(xc3[,1]))
treinamento_c3_x <-xc3[seqc3[(1:Ntrain)],1]
treinamento_c3_y <-xc3[seqc3[(1:Ntrain)],2]
teste_c3_x <-xc3[seqc3[(Ntrain+1):length(xc3[,1])],1]
teste_c3_y <-xc3[seqc3[(Ntrain+1):length(xc3[,1])],2]

# Classe 4
seqc4 <- sample(length(xc4[,1]))
treinamento_c4_x <-xc4[seqc4[(1:Ntrain)],1]
treinamento_c4_y <-xc4[seqc4[(1:Ntrain)],2]
teste_c4_x <-xc4[seqc4[(Ntrain+1):length(xc4[,1])],1]
teste_c4_y <-xc4[seqc4[(Ntrain+1):length(xc4[,1])],2]



# Plot dos dados de treinamento e de teste
#   Plot dos dados de TREINAMENTO ----
plot(treinamento_c1_x, treinamento_c1_y, col = ' red ' , xlim = c(0,6),ylim = c(0,6),xlab = ' x ' ,ylab= ' y ' )
par(new=T)
plot(treinamento_c2_x, treinamento_c2_y, col = ' blue ' , xlim = c(0,6),ylim = c(0,6),xlab = ' x ' ,ylab= ' y ' )
par(new=T)
plot(treinamento_c3_x, treinamento_c3_y, col = ' green ' , xlim = c(0,6),ylim = c(0,6),xlab = ' x ' ,ylab= ' y ' )
par(new=T)
plot(treinamento_c4_x, treinamento_c4_y, col = ' black ' , xlim = c(0,6),ylim = c(0,6),xlab = ' x ' ,ylab= ' y ' )


#   Plot dos dados de TESTE ----
plot(teste_c1_x, teste_c1_y, col = ' red ' , xlim = c(0,6),ylim = c(0,6),xlab = ' x ' ,ylab= ' y ' )
par(new=T)
plot(teste_c2_x, teste_c2_y, col = ' blue ' , xlim = c(0,6),ylim = c(0,6),xlab = ' x ' ,ylab= ' y ' )
par(new=T)
plot(teste_c3_x, teste_c3_y, col = ' green ' , xlim = c(0,6),ylim = c(0,6),xlab = ' x ' ,ylab= ' y ' )
par(new=T)
plot(teste_c4_x, teste_c4_y, col = ' black ' , xlim = c(0,6),ylim = c(0,6),xlab = ' x ' ,ylab= ' y ' )




# Cálculo da média e desvio padrão de cada classe ----
# Classe 1
c1_ux<-mean(treinamento_c1_x)
c1_uy<-mean(treinamento_c1_y)
c1_sx<-sd(treinamento_c1_x)
c1_sy<-sd(treinamento_c1_y)

# Classe 2
c2_ux<-mean(treinamento_c2_x)
c2_uy<-mean(treinamento_c2_y)
c2_sx<-sd(treinamento_c2_x)
c2_sy<-sd(treinamento_c2_y)

# Classe 3
c3_ux<-mean(treinamento_c3_x)
c3_uy<-mean(treinamento_c3_y)
c3_sx<-sd(treinamento_c3_x)
c3_sy<-sd(treinamento_c3_y)

# Classe 4
c4_ux<-mean(treinamento_c4_x)
c4_uy<-mean(treinamento_c4_y)
c4_sx<-sd(treinamento_c4_x)
c4_sy<-sd(treinamento_c4_y)


# Aplicando a PDF para cada ponto dos dados de teste das classes 1, 2, 3, 4 ----
vetor_resultado_c1 <- matrix(0, nrow = length(teste_c1_x), ncol = 1)
vetor_resultado_c2 <- matrix(0, nrow = length(teste_c2_x), ncol = 1)
vetor_resultado_c3 <- matrix(0, nrow = length(teste_c3_x), ncol = 1)
vetor_resultado_c4 <- matrix(0, nrow = length(teste_c4_x), ncol = 1)

tam_teste <- length(teste_c1_x)
for (i in seq(1, tam_teste, 1) ){
  
    # Calcula a probabilidade do ponto de teste da classe 1 pertencer a C1, C2, C3 ou C4 ----
    p_classe_1<- pdf2var(teste_c1_x[i],teste_c1_y[i],c1_ux,c1_uy,c1_sx,c1_sy,p_user)
    p_classe_2<- pdf2var(teste_c1_x[i],teste_c1_y[i],c2_ux,c2_uy,c2_sx,c2_sy,p_user)
    p_classe_3<- pdf2var(teste_c1_x[i],teste_c1_y[i],c3_ux,c3_uy,c3_sx,c3_sy,p_user)
    p_classe_4<- pdf2var(teste_c1_x[i],teste_c1_y[i],c4_ux,c4_uy,c4_sx,c4_sy,p_user)
    
    # Compara se o ponto pertence as classe 1, 2, 3 ou 4
    vetor_resultado_c1[i]<-retorna_maior(p_classe_1, p_classe_2, p_classe_3, p_classe_4)

    # Calcula a probabilidade do ponto de teste da classe 2 pertencer a C1, C2, C3 ou C4 ----
    p_classe_1<- pdf2var(teste_c2_x[i],teste_c2_y[i],c1_ux,c1_uy,c1_sx,c1_sy,p_user)
    p_classe_2<- pdf2var(teste_c2_x[i],teste_c2_y[i],c2_ux,c2_uy,c2_sx,c2_sy,p_user)
    p_classe_3<- pdf2var(teste_c2_x[i],teste_c2_y[i],c3_ux,c3_uy,c3_sx,c3_sy,p_user)
    p_classe_4<- pdf2var(teste_c2_x[i],teste_c2_y[i],c4_ux,c4_uy,c4_sx,c4_sy,p_user)
    
    # Compara se o ponto pertence as classe 1, 2, 3 ou 4
    vetor_resultado_c2[i]<-retorna_maior(p_classe_1, p_classe_2, p_classe_3, p_classe_4)
    
    # Calcula a probabilidade do ponto de teste da classe 3 pertencer a C1, C2, C3 ou C4 ----
    p_classe_1<- pdf2var(teste_c3_x[i],teste_c3_y[i],c1_ux,c1_uy,c1_sx,c1_sy,p_user)
    p_classe_2<- pdf2var(teste_c3_x[i],teste_c3_y[i],c2_ux,c2_uy,c2_sx,c2_sy,p_user)
    p_classe_3<- pdf2var(teste_c3_x[i],teste_c3_y[i],c3_ux,c3_uy,c3_sx,c3_sy,p_user)
    p_classe_4<- pdf2var(teste_c3_x[i],teste_c3_y[i],c4_ux,c4_uy,c4_sx,c4_sy,p_user)
    
    # Compara se o ponto pertence as classe 1, 2, 3 ou 4
    vetor_resultado_c3[i]<-retorna_maior(p_classe_1, p_classe_2, p_classe_3, p_classe_4)
    
    
    
    # Calcula a probabilidade do ponto de teste da classe 4 pertencer a C1, C2, C3 ou C4 ----
    p_classe_1<- pdf2var(teste_c4_x[i],teste_c4_y[i],c1_ux,c1_uy,c1_sx,c1_sy,p_user)
    p_classe_2<- pdf2var(teste_c4_x[i],teste_c4_y[i],c2_ux,c2_uy,c2_sx,c2_sy,p_user)
    p_classe_3<- pdf2var(teste_c4_x[i],teste_c4_y[i],c3_ux,c3_uy,c3_sx,c3_sy,p_user)
    p_classe_4<- pdf2var(teste_c4_x[i],teste_c4_y[i],c4_ux,c4_uy,c4_sx,c4_sy,p_user)
    
    # Compara se o ponto pertence as classe 1, 2, 3 ou 4
    vetor_resultado_c4[i]<-retorna_maior(p_classe_1, p_classe_2, p_classe_3, p_classe_4)
    
}


# Cálculo de acerto de teste ----
acerto_c1 <- length(which(vetor_resultado_c1[,1] == 1))/length(vetor_resultado_c1)
acerto_c2 <- length(which(vetor_resultado_c2[,1] == 2))/length(vetor_resultado_c2)
acerto_c3 <- length(which(vetor_resultado_c3[,1] == 3))/length(vetor_resultado_c1)
acerto_c4 <- length(which(vetor_resultado_c4[,1] == 4))/length(vetor_resultado_c2)


# Estimando as densidades em cada ponto de um grid ----
seqi<-seq(0, 6, 0.025)
seqj<-seq(0, 6, 0.025)
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
    p_classe_3<- pdf2var(i,j,c3_ux,c3_uy,c3_sx,c3_sy,p_user)
    p_classe_4<- pdf2var(i,j,c4_ux,c4_uy,c4_sx,c4_sy,p_user)
    
    # Compara qual classe é provável do ponto pertencer
    M1[ci,cj]<-retorna_maior(p_classe_1, p_classe_2, p_classe_3, p_classe_4)
    
  }
}


#Plotando as densidades ----
persp3D(seqi,seqj,M1,counter=T,theta = 55, phi = 30, r = 40, d = 0.1, expand = 0.5,
        ltheta = 90, lphi = 180, shade = 0.4, ticktype = "detailed", nticks=5)



#Plotando os pontos novamente para gráfico final e superfície de contorno ----
plot(vermelho[,1] ,vermelho[,2],col = ' red ' , xlim = c(0,6),ylim = c(0,6),xlab = ' xs ' ,ylab= ' y ' )
par(new=T)

plot(azul[,1] ,azul[,2],col = ' blue ' , xlim = c(0,6),ylim = c(0,6),xlab = ' xs ' ,ylab= ' y ' )
par(new=T)

plot(verde[,1] ,verde[,2],col = ' green ' , xlim = c(0,6),ylim = c(0,6),xlab = ' xs ' ,ylab= ' y ' )
par(new=T)

plot(preto[,1] ,preto[,2],col = ' black ' , xlim = c(0,6),ylim = c(0,6),xlab = ' xs ' ,ylab= ' y ' )
par(new=T)

#plotando as superfícies de contorno
contour2D(M1,seqi,seqj,colkey = FALSE, add=TRUE)



