# ------------------ Estatistica Computacional II - EST066 ------------------ #
# Data: 11/05/2022

# X e uma mistura de populacoes continuas
# fx(x) = sum(pi*fi(x))
# f1(x), f2(x), ..., fm(x) sao fdps (fps)

# pi > 0, i = 1,2,...,m  (onde i eh indice)
# sum(pi) = 1

# Expected value is sum(pi*E(Xi))

# Mistura de Normais --------------------------------------------------------

# Populacao 1 - define-se media mi = -2 e var sigma² = 1
# Populacao 2 - define-se mi = 2 e sigma² = 1

# Transformar Normal Padrao para Normais(mi,sigma²) -------------------------

# sqrt(sigma²)* Z + mi, 

# onde Z ~ N(mi,sigma²)


# Exercicio -----------------------------------------------------------------

# Quero gerar va's N(-2,1) e N(2,1)

# Criar matriz cujas linhas representam os parametros de cada populacao 
# (media e variancia)

matriz<-matrix(c(-2,1,2,1), byrow = TRUE, nrow = 2)
matriz

# Sorteio de va's normais padrao por Box-Miller (Prof NAO quer que use rnorm())

box.muller <-function(n = 100){
  
  rodados <- n/2
  
  U1<-runif(rodados)
  U2<-runif(rodados)
  
  erre<- sqrt(-2*log(U1))
  theta<-(2*pi*U2)
  
  Z1 <- erre*cos(theta)
  Z2 <- erre*sin(theta)
  
  c(Z1, Z2)
  
}

valores<-box.muller()
valores

# Sortear se as v.a.'s de valores irao pertencer a Populacao 1 ou Populacao 2

pop<-sample(c(1,2), size = length(valores), replace =TRUE, prob = c(0.3, 0.7))
pop

# Transformar as va's normais de valores nas populacoes requeridas

normal<-numeric(length(valores))
length(valores)

for (i in 1:length(valores)){
    normal[i] <- (sqrt(matriz[pop[i], 2])*valores[i])+ matriz[pop[i],1]
}

dmistura <- function(x){
    
     (0.3*dnorm(x,mean=-2,sd=1)) + ((1-0.3)* dnorm(x,mean=2,sd=1))
}

hist(normal, freq = FALSE, col = 'lightpink')
curve(dmistura, add= T, col = 'red')

pmistura<-function(x){
  
  (0.3*pnorm(x,mean=-2,sd=1)) + ((1-0.3)* pnorm(x,mean=2,sd=1))
}

# Os dados seguem de fato uma normal? ---------------------------------------

ks.test(normal, 'pmistura') 

# pmistura eh a fda exata
# ecdf(normal) fornece a fda empirica ou aproximada (com base nos dados)

# Vamos comparar as duas ---------------------------------------------------- 

plot(ecdf(normal))
curve(pmistura, add = T, lwd = 2, col = 'red')

# Dever de Casa -------------------------------------------------------------

# gerar 100 numeros, verificar histograma, verificar com densidade, verificar
# com empirica, fazer teste kolmogorov. Tudo vetorizado.










