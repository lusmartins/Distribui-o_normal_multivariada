#Distribuiçao Normal Multivariada

# limpar todas as variáveis
rm(list = ls(all.names = TRUE))

# biblioteca

library(ellipse)
library(MASS)
library(xtable)
library(corrplot)
library(knitr)
library(stats)

file.save <-"C:/Users/Luciana/OneDrive/Documentos/MetodosEstatistiscos"

#Elipsoide de confiança
# Dados simulados
n<-1000 # tamanho da amostra

# vetor de médias
mu <- c(10,5)

# matriz de covariâncias
rho<-0.9
var1<-100
var2<-25
Sigma<- rbind(cbind(var1,sqrt(var1*var2)*rho),cbind(sqrt(var1*var2)*rho,var2))

# gerando uma amostra
mx <- mvrnorm(n, mu, Sigma)
par(mfrow=c(1,1))
plot(mx[,1],mx[,2],xlab="variável 1", ylab="variável 2")

# estimativas de média, correlação e variâncias
ma <- apply(mx,2,mean)
rhoa <- cor(mx)[1,2]
vara1<-cov(mx)[1,1]
vara2<-cov(mx)[2,2]

# elipse com parâmetros estimados
lines(ellipse(rhoa, scale = c(sqrt(vara1), sqrt(vara2)),
              centre = rbind(ma[1], ma[2]), level = 0.95))

# Dados da Íris de Fisher
iris<-as.matrix(iris[,1:4])

inames <- c("CS","LS","CP","LP")
colnames(iris)<-inames
par(mfrow=c(1,1))
pairs(iris,pch=19,cex.labels=1.4)


# Matriz de correlações
cor(iris)
#xtable(cor(iris))
rho <-cor(iris) #lowerTriangle((cor(iris)))
xb <- apply(iris,2,mean)
s2<-cov(iris)
vara <-diag(s2)
nvar <- 4

# Gráfico de correlações
par(mfrow=c(1,1))
corrplot(cor(iris), order = "hclust", tl.col='black', tl.cex=.75)

# Boxplot
par(mfrow=c(1,1))
boxplot(iris,cex=1.2,cex.axis=1.2,cex.lab=1.2,names=inames)

par(mfrow=c(2,2))
for (i in 1:4) {
  ks_result <- ks.test(iris[,i], "pnorm", mean(iris[,i]), sd(iris[,i]))
  cat("Variável:", inames[i], "\n")
  cat("p-valor do teste de Kolmogorov-Smirnov:", ks_result$p.value, "\n\n")
}
# Histogramas
par(mfrow=c(2,2))
hist(iris[,1],xlab=inames[1],cex=1.2,cex.axis=1.2,cex.lab=1.2,
     main="",probability=TRUE,ylab="densidade")
hist(iris[,2],xlab=inames[2],cex=1.2,cex.axis=1.2,cex.lab=1.2,
     main="",probability=TRUE,ylab="densidade")
hist(iris[,3],xlab=inames[3],cex=1.2,cex.axis=1.2,cex.lab=1.2,
     main="",probability=TRUE,ylab="densidade")
hist(iris[,4],xlab=inames[4],cex=1.2,cex.axis=1.2,cex.lab=1.2,
     main="",probability=TRUE,ylab="densidade")


# Elipsóides de Confiança
par(mfrow=c(2,3))
for (i in 1:(nvar-1)){
  for (j in (i+1):nvar){
    plot(ellipse(rho[i,j], scale = c(sqrt(vara[i]), sqrt(vara[j])),
                 centre = rbind(xb[i], xb[j]), level = 0.95),type="l",lwd=2,col=2,
         xlab=inames[i],ylab=inames[j])
    lines(iris[,i],iris[,j],pch=19,cex=1.2,cex.lab=1.2,cex.axis=1.2,type="p",col=4)
  }
}

# Gráfico Q-Q Plot
par(mfrow=c(2,2))
for (i in 1:4){
  qqnorm(iris[,i], main = paste('Q-Q Plot para', inames[i]), xlab = 'Quantis teóricos', ylab = 'Quantis observados')
  qqline(iris[,i], col = 2)
  
  qq_corr <- cor(qqnorm(iris[,i], plot.it = FALSE)$x, qqnorm(iris[,i], plot.it = FALSE)$y)
  cat('Coeficiente de Correlação de Pearson para', inames[i], ":", qq_corr, '\n\n')
  
}

# Gráfico Q-Q Plot para forma quadrática
par(mfrow=c(2,2))
for (i in 1:4){
  dados_quadraticos <- iris[,i]^2
  qqnorm(dados_quadraticos, main = paste('Q-Q Plot para', inames[i]), xlab = 'Quantis teóricos', ylab = 'Quantis observados')
  qqline(dados_quadraticos, col = 2)
  
  qq_corr <- cor(qqnorm(dados_quadraticos, plot.it = FALSE)$x, qqnorm(dados_quadraticos, plot.it = FALSE)$y)
  cat('Coeficiente de Correlação de Pearson para', inames[i], ":", qq_corr, '\n\n')
  
}







