# 30/03/2023

# Quadratic Distribution

# You've gotta create: pene, qene, rene and dene;

# Wiki: In probability theory and statistics, the U-quadratic distribution is a 
# continuous probability distribution defined by a unique convex quadratic function
#with lower limit a and upper limit b.

# In our case, a = 0 and b = 1;

# Suppose given values for x are between 0 and 1 (with 0 and 1 included);

# pdf --------------------------------------------------------------------------

d2u <- function(x, a = 0, b = 1){
  
  B = (b + a)/2
  A = 12/(b-a)**3
  
  
  return(A*(x-B)**2) 
  
  
}

d2u (0.5)

# cdf --------------------------------------------------------------------------

p2u <- function(x, a = 0, b = 1){
  
  if(x>=0 & x<1){ 
  
  B = (b + a)/2
  A = 12/(b-a)**3
  
  return((A/3)*((x-B)**3+(B-a)**3))
  
  }
  
  else if (x>=1)
    return(1)
  
  else # if x < 0 
    return(0)
  
}

p2u(1)
p2u(0)
p2u(0.5)
p2u(0.25)
p2u(c(0,0.25,0.5,0.75,1)) # funcao nao esta vetorizada

# tem q tirar os condicionais 'if' ou usar Vectorize(function)

p2u<-Vectorize(p2u)

# quantilica -------------------------------------------------------------------

# soh funciona p a = 0 e b = 1

q2u <- function(p){
  
  return(((2*p-1)**(1/3)+1)/2)
  
}

q2u(1)
q2u(0)
q2u(0.5)
q2u(0.4375)

# vamos tentar solucionar o problema dos NaN acima

RaizCub<-function(x) sign(x)*(abs(x))**(1/3)

q2u<-function(p)(RaizCub(2*p-1)+1)/2

q2u(1)
q2u(0)
q2u(0.5)
q2u(0.4375)
q2u(0.5625)

q2u(c(1,0,0.5,0.4375,0.5625))

# sortear ----------------------------------------------------------------------

r2u <- function(n){ # tam da amostra
  
  u = runif(n)
  x = q2u(u)
  return(x)
} # joga o valor de amostra q a unif gera na quantilica na funcao de interesse
# para gerar amostras da funcao de interesse.

r2u(10)

# Histograma -------------------------------------------------------------------

run = 1e4

valores <- r2u(run)

hist(valores, freq = F, breaks = 50, main = 'Histograma de r2u')


# Criar 5 mil amostras de tamanho 2 --------------------------------------------

amostras <- matrix(valores, ncol = 2)
dim(amostras)

medias <- apply(amostras,1,mean)
hist(medias, freq = F, breaks = 50)

mean(medias) # a media de amostras vai, a medida q o tam da amostra cresce, em
# direcao a media da variavel aleatoria (que eh a da populacao) - TLC

sd(medias)

# alterar valor de n para 3, 10...

terceira<-r2u(5000)
amostras3<-cbind(amostras,terceira)
medias3<-apply(amostras3,1,mean)
hist(medias3, freq = F, breaks = 50)
dim(amostras3)

# apareceram mais modas

decima<-r2u(50000)
amostras10<-matrix(decima, ncol = 10) #tam de cada amostra eh 10 (sao 5000 no total)
medias10<-apply(amostras10,1,mean)
hist(medias10, freq = F, breaks = 50)
dim(amostras10)

amostras10
nrow(amostras10)

curve(dnorm(x, mean = 0.5, sd = 0.1224), add = T) # parametros (populacionais)

sd(amostras10)/sqrt(10)
# note que o erro padrao (sd/sqrt(n)) reduz a medida que n aumenta

# ------------------------------------------------------------------------------

# Notas de Aula

# (professor resolveu descobrindo a formula da fdp pela area
# da figura. Foto anexada)

funk <- function(x) (x-1/2)*12

padrao <- integrate(funk, lower = 0, upper = 1)$value

R = 1/padrao**2

d4u <- function(x)80*(x-1/2)**4 # mais concentrado em 0 e 1 (compare com a forma
# anterior)
curve(d4u, add = T, col = 'blue')

d2u <- function(x)12*(x-1/2)**2
curve(d2u, add = T, col = 'red')

# Como usar integrate
# Procurar como fazer o teste de normalidade
# Proporcao de pontos dentro do ultimo grafico feito?

