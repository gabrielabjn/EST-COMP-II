# ------------------ Estatistica Computacional II - EST066 ------------------ #
# Data: 18/04/2023

xis.p <- function(x, a = 1664525, b = 1013904223, m = 2**32){
  (a*x + b)%%m
}

runif.my <- function(n, x0 = 2, m = 2**32){
  antes <- x0
  vetor <- numeric(n)
  for(i in 1:n){
    xis <- xis.p(antes)
    antes <- xis
    u <- xis/m
    vetor[i] <- u
  }
  vetor
}

valores.2 <- runif.my(100)
ks.test(valores.2, "punif", 0, 1)

mean(valores.2)
sum(valores.2)

histog <- hist(valores.2, breaks = 10, freq = F)
str(histog)
resumo <- cbind(1:10, histog$counts)
resumo
tabela <- table(resumo)
tabela

sum((resumo[,2] - 10)**2)/10
# Probabilidade da qui-quadrado com nove graus de liberdade ser maior que 3.2
1 - pchisq(3.2, df = 9)
chisq.test(resumo[,2])

# Novo metodo de geracao de numeros aleatorios
# f: fdp de interesse
# g: fdp candidata(simples gerar valores de g)

# Ha uma constante M tal que f(x)/g(x) <= M, para todo x e Ry

# (1) Gerar Y de acordo com g
# (2) Gerar U~Unif(0,1)
# Se U <= f(y)/(M*g(y)), entao X = Y e pare, caso contrario repita o passo 1

# Gerar 2*N(0,1)
f.x <- function(x) (2*exp(-x**2 / 2))/sqrt(2*pi)
curve(f.x, from = 0, to = 5)
M <- exp(0.5)*2 / sqrt(2*pi)
g.y <- function(y) 2 * dexp(y) # TROCAR 2 POR M
curve(g.y, from = 0, to = 5, add = T, col = "red")

set.seed(666)
u1 <- runif(1)
y <- -log(u1)

u2 <- runif(1)

f.x(y)/g.y(y) > u2

set.seed(666)
u1 <- runif(1)
y <- -log(u1)

u2 <- runif(1)

f.x(y)/g.y(y)
f.x(y)/g.y(y) > u2


i<- 1
cont<-0
vet <- numeric(100)
while(i<=100){
  
  u1<-runif(1)
  y<--log(u1)
  u2<-runif(1)
  
  if(f.x(y)/g.y(y)>u2){
    vet[i]<-y
    i<-i+1
  }
cont<-cont+1
}
cont
100/cont

hist(vet, freq = F)

# Funcao que gera o modulo do valor absoluto de uma normal

Fx<-function(x){
  2* (pnorm(x)-1/2)
}

# Empirica de vet

Fn<-ecdf(vet)
plot(Fn, col = 'lightblue')

curve(Fx, add = T, col = "red")  
ks.test(vet, "Fx") 

vet.norm<- function(){
  
  for (i in 1: length(vet)){
  
  u3<-runif(1)
  
 if (u3<0.5)
  vet[i]<-(-1)*vet[i]
 
  }


}

ks.test(vet, "vet.norm") 

# Gerar gamas e betas

gamma(100) 
lgamma(100)
gamma(1/2)

