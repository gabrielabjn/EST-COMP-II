# 28/03/23

# Teorema da Transformacao Inversa

# Seja U ~  uniforme(0,1). Para qualquer fda continua Fx, a variavel aleatoria
# eh definida por

# X = F^(-1)(U), com fda Fx
# Devolve o ponto da acumulada até onde se dá determinada probabilidade (area) 

# Fx(-1)(u) = x ---> Fx(x) = u

# Exemplo ----------------------------------------------------------------------

# Densidade de  X --------------------------------------------------------------

# nx^(n-1), x e (0,1)
# 0, cc

# considerar que o input sera x entre 0 e 1 !!!!!!!!!!!

dene <- function(x, n){
  
    return(n*x**(n-1))
 
}

dene(0,2)
dene(1,2)

# FDA de X ------------- -------------------------------------------------------

# x^n, x e (0,1)
# 1, x >= 1
# 0 x < 0

# considerar que o input sera x entre 0 e 1 !!!!!!!!!!!

pene <- function(x, n){
  
    return(x**n)
  
}

pene(0,2)
pene(1,2)
pene(0.9862,100)

# Quantílica de X --------------------------------------------------------------

# Fx^(-1)(p) = p^(1/n)

qene <- function(p, n){
  
    return(p**(1/n))
  
}

qene(0,2)
qene(0.25,2)
qene(0.5,2)
qene(0.75,2)
qene(1,2)

# graficos n = {2,3,5,10} ------------------------------------------------------

par(mfrow = c(2,2), mar = c(3,3,1,1)+0.1) # aumenta 0.1 a todos os elem. do vetor
# default margin: mar = (5, 4, 4, 2) (norte, oeste, sul, leste)

curve(dene(x,2), from = 0, to = 1, ylim = c(0,10))
curve(dene(x,3), from = 0, to = 1, ylim = c(0,10))
curve(dene(x,5), from = 0, to = 1, ylim = c(0,10))
curve(dene(x,10), from = 0, to = 1, ylim = c(0,10))


# vamos aprimorar o codigo acima? ----------------------------------------------

par(mfrow = c(2,2), mar = c(3,3,1,1)+0.1)

for (i in c(2,3,5,10)){
  
  curve(dene(x,i), from = 0, to = 1, ylim = c(0,10))
  
}

# ------------------------------------------------------------------------------

# Verificar se a funcao esta vetorizada testando as entradas abaixo

for (i in c(0,0.6299,0.7937,0.9085,1)){
  
  print(pene(i,3))
  
} # resultado correto aproximado : c(0, 0.25,0.5,0.75,1)

# Gerar 1000 numeros aleatorios p nossa funcao ---------------------------------

rene <- function(tam = 1, n =2){
  
  u = runif(tam)
  x = qene(u,n)
  return(x)
}

# Grafico!!!!!!!!! -------------------------------------------------------------

par(mfrow = c(2,2))
valores <- replicate(1000, rene(1, 2))
length(valores)

hist(valores, freq = F)
curve(dene(x,2), from = 0, to = 1, add = T)




