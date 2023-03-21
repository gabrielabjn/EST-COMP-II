# 16/03/2023

integrate(dnorm, lower = - Inf, upper = 0.42)
pnorm(0.42)

# criacao de funcao p calcular a acumulada de uma normal padrao de
# - Inf a 0.42

phi.app<-function(z){

val<-0.5 + 0.5*((exp(2*0.8*z)-1)/(exp(2*0.8*z)+1))
  
  return(val)
  
}

phi.app(0.42)

# verificar se a aproximacao eh boa

# maneira grafica

curve(pnorm, from = -4, to = 4)
curve(phi.app, from = - 4, to = 4, lty = 2, add = T)

# maneira analitica

erro<- function(x){
  
pnorm(x) - phi.app(x)
}

curve(erro, from = -4, to = 4)

optimize(erro, interval = (-3:0)) # devolve 'x' e o erro


# para calcular o val maximo entre (-3 e 3), "inverta" o intervalo
# (-3:0) ---> (0,3)

optimize(erro, interval = (0:3))

# ------------------------------------------------------------------------------


p<-0.6627573

equacao <- function(x){
  
  
  pnorm(x) - p
  
}

# uniroot encontra qual valor de x zera a equacao acima

uniroot(equacao, interval = c(0,1))

# ------------------------------------------------------------------------------

# resolver: acumulada, densidade e quantilica para a funcao triangular de 
# parametros (0,1,2)

# nota: a funcao quantilica eh o inverso da funcao acumulada, isto eh:
# dada uma area (em porcentagem), ela encontra o menor valor de x que determina essa area.

# execmplos abaixo para a funcao tri(0,1,2)

ptri(2, min = 0, max = 2, mode = 1) # funcao densidade acumulada
# encontra a area acumulada ate x = 2

dtri(2, min = 0, max = 2, mode = 1) # funcao densidade de probabilidade
# encontra a probabilidade de ocorrer x = 2 

qtri(0.5, min = 0, max = 2, mode = 1) # funcao quantilica
# encontra a menor coordenada x ate a qual se acumula uma area de 50% do total
# da distribuicao acumulada





