# 11/04/2023
# Aritmetica Modular -----------------------------------------------------------

# Dois numeros sao ditos equivalentes (ou congruentes) modulo m se suas diferencas
# sao um numero inteiro divisivel uniformemente por m.

# a (tres linhas paralelas na horizontal) b mod m

# Ex: 5 e 14 sao congruentes mod 3
# 5 mod 3 = 2
# 14 mod 3 = 2

# Aritmetica Modular com V.A.'s uniformes --------------------------------------
# V ~ U(0,1)

# X (tres linhas paralelas na horizontal) (kY + c) mod 1

# k eh uma cte inteiro
# c eh uma cte real

# Gerador Congruencial ---------------------------------------------------------

# X0 = nº inicial    X0 e {0,1,...,m-1}
# A,B numeros grandes

# Define-se a seq de numeros ---------------------------------------------------

#Xn e {0,1,...,m-1}, n = 0,1,...
#Xn+1 = AXn + B mod m
#Un = Xn/m

# Exercicio pratico ------------------------------------------------------------

# m = 10
# A = 103
# B = 17
# x_0 = 2 # valor inicial
# X = (AX+ B)%%m    // Xn+1

X_func<-function(x,A = 103,B = 17,m = 10){
  
  (A*x+B)%%m 
  
}

X_func(2) # eh hj que a jiripoca vai piar

# ------------------------------------------------------------------------------

m = 2**32
A = 1664525
B = 1013904223
X0 = 2 # valor inicial

# Funcao* que cria os valores seguindo os criterios acima ----------------------
X_func<-function(x,A = 1664525,B = 1013904223, m = 2**32){ 
  
  (A*x+B)%%m 
  
}

X_func(5)



# Gera valores como se os dados seguissem uma  uniforme ------------------------
xis <- c(1017233273, 1975575172, 811535379, 3186434646)
U= xis/2**32
U



# Funcao que gera um vetor de valores que segue a primeira funcao* -------------

r.unif.my <- function (tam,seed = 2, m = 2**32){
  
  antes<-seed
  vetor<-numeric(tam) # aloca espaco p um vetor de tamanho tam -----------------
  
 for (i in 1:tam){
 
   xis<-X_func(antes)
   antes<-xis
   u<-xis/m
   
   vetor[i]<-u
   
 }
 
 vetor
  
}

# Funcao q gera um vetor de valores que segue a uniforme -----------------------
valoressunif<-runif(1000)
hist(valoressunif, freq = F, breaks = 100)


# gerando 1000 observacoes da minha distribuicao
valoress<-r.unif.my(1000, seed = 666)

# plotando histograma
hist(valoress, freq = F, breaks = 100)

# particiona conjunto em pedacos iguais
cut(valoress, breaks = 100) 

# brincando
mean(valoress>0.5) # media dos valores acima de 0.5
mean(valoress>0.9) 
mean(valoress<0.1)
mean(valoress>0.99)

# Funcao de probabilidade acumulada

# Comparando grafico da uniforme e da minha funcao

Fn<- ecdf(valoress) # empirical cumulative dist function
plot(Fn)
abline(0,1,col='red')

Fn_unif<-ecdf(valoressunif)
plot(Fn_unif, add = TRUE, col = 'blue')











