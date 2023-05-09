# Aproximacao da V.A. Gaussiana

# Sorteio de n numeros aleatorios ao acaso
# U1, U2,...,Un IID

# Relembrando
# TCL (X-linha - mi)/sqrt(sigma²/n) para n -> Inf segue N(0,1)

# Seja Y = sqrt(12/n)*(sum(-n/2))
# Entao Y ~ (assintoticamente) N(0,1)
# Assintoticamente: quando n cresce

# Gerar 100 normais

# 100 amostras de tamanho 12

funcao <- function(n){
  
   amostra<-runif(n)
   y<- sqrt(12/n)*(sum(amostra)-(n/2))
   return(y)
}

valores<-replicate(100,funcao(12))
hist(valores, freq = FALSE, ylim = c(0,0.4), col = 'lightgreen')
curve(dnorm,add = T, col = 'red')


Fn<-ecdf(valores)
plot(Fn, col = 'lightgreen')
plot(pnorm, add= TRUE)

# -------------------------------------------------------------------

cont<-1
pvalor<-numeric(100)

while (cont <= 100){

valores2<-sapply(1:30, function(x)funcao(12))
res<-ks.test(valores2, 'pnorm')
pvalor[cont]<-res$p.value

cont<-cont+1

}
pvalor

hist(valores2, col = 'lightblue', freq = FALSE)
curve(dnorm,add = T, col = 'red')

quantile(pvalor,0.30)





     