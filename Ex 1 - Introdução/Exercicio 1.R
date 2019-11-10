rm(list=ls())
library('plot3D')

#--------------------------------

# Tem-se como objetivo:
#   Estimar a densidade para as duas classes.
#   Apresentar o gráfico de densidade de probabilidade para duas variáveis.
#   Apresentar o gráfico de superfície de probabilidade para duas variáveis.

# definição dos dados duas dimensoes
s1<-0.6
s2<-0.6
nc<-100
p_user<-0.9
xc1<-matrix(rnorm(nc*2),ncol=2)*s1 + t(matrix((c(2,2)),ncol=nc,nrow=2))
xc2<-matrix(rnorm(nc*2),ncol=2)*s2 + t(matrix((c(4,4)),ncol=nc,nrow=2))
plot(xc1[,1],xc1[,2],col = ' red ' , xlim = c(0,6),ylim = c(0,6),xlab = ' xs ' ,ylab= ' y ' )
par(new=T)
plot(xc2[,1],xc2[,2],col = ' blue ' , xlim = c(0,6),ylim = c(0,6),xlab = '' ,ylab= '' )

#Ir mudando esse valor de p


#funcao para estimativa da densidade de 2 variáveis
pdf2var<-function(x,y,u1,u2,s1,s2,p) {(1/(2*pi*s1*s2*sqrt(1-(p^2))))*
    exp((-(1)/(2*(1-(p^2))))*((((x-u1)^2)/((s1)^2))+(((y-u2)^2)/((s2)^2))-
                                ((2*p*(x-u1)*(y-u2))/(s1*s2))))
}


#cáculo da média e desvio padrão das classes
u1<-rbind(mean(xc1[,1]),mean(xc1[,2]))
u2<-rbind(mean(xc2[,1]),mean(xc2[,2]))
s1<-rbind(sd(xc1[,1]),sd(xc1[,2]))
s2<-rbind(sd(xc2[,1]),sd(xc2[,2]))



#estimando as densidades em cada ponto de um grid
seqi<-seq(0.06,6,0.06)
seqj<-seq(0.06,6,0.06)
M1 <- matrix(0,nrow=length(seqi),ncol=length(seqj)) 
M2 <- matrix(0,nrow=length(seqi),ncol=length(seqj))
ci<-0
for (i in seqi){
  ci<-ci+1
  cj<-0
  for(j in seqj)
  {
    cj<-cj+1
    M1[ci,cj]<- pdf2var(i,j,mean(xc1[,1]),mean(xc1[,2]),sd(xc1[,1]),sd(xc1[,2]),p_user)
    M2[ci,cj]<- pdf2var(i,j,mean(xc2[,1]),mean(xc2[,2]),sd(xc2[,1]),sd(xc2[,2]),p_user)
    
  }
}


#plotando as densidades

persp3D(seqi,seqj,M1,counter=T,theta = 55, phi = 30, r = 40, d = 0.1, expand = 0.5,
        ltheta = 90, lphi = 180, shade = 0.4, ticktype = "detailed", nticks=5)
persp3D(seqi,seqj,M2,counter=T,theta = 55, phi = 30, r = 40, d = 0.1, expand = 0.5,
        ltheta = 90, lphi = 180, shade = 0.4, ticktype = "detailed", nticks=5, add=T)



#plotando as superfícies de contorno
contour2D(M1,seqi,seqj,colkey = NULL)
par(new=T)
contour2D(M2,seqi,seqj,colkey = NULL)
