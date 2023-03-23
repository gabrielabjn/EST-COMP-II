# EST066
# 21/03/23
# Aplicando a funcao triangular vista em aula.

p_tri <- function(x){
  
  if (x<=0)
    return(0)
  
  if (x>0 && x<=1)
    return (x)
  
  if (x>1 && x<=2)
    
    if (x>2)
      return (0)
  
}

d_tri <- function(x){
  
  if (x<=0)
    return(0)
  
  if (x>0 && x<=1)
    return(power(x,2)/2)
  
  if (x>1 && x<2)
    return(power(x,2)/2 + 2*x -1)
  
  if (x>=2)
    return(1)
  
  
  
}

q_tri<- function(x){
  
  if (x<=0)
    return(0)
  
  if (x>0 && x<=1/2)
    return (power(2*x,1/2))
  
  if (x>1/2 && x<1)
    return(2-(power(2*(1-x),1/2)))
  
  else
    print("entrada invalida em q_tri")
  
}

# abaixo: funcao criada pelo professor pprofessorara gerar amostras aleatorias da funcao 
# triangular (equivalente a funcao rtri do pacote {EnvStats})

rtri.pri<-function(){ 
  
  U<-runif(1)
  y<-qtri(u)
  
  return(y)
  
  
}


# gerar 1000 numeros aleatorios da funcao triangular

# fazer o histograma (densidade) dos 1000 numeros aleatorios.

# plotar a densidade da triangular no histograma (use 'curve')

# calcular: media dos 1000 numeros aleatorios proporcao < 0.5

# ptri.vec <- vectorize(ptri)

# ptri.vec(c(0,0.5,1,1.5,2))
# gerados<-replicate(1000,rtri.pri())
# -----------------------------------------------------------

# 23/03/23

amostra1 <-runif(1000,0,1)
amostra2 <- runif(1000,0,1)

amostra3<-amostra1+amostra2

hist(amostra3, freq = F, breaks = 50)

p_tri(1.01) - p_tri(0.99)

# dados entre 0.99 e 1.01

mean(amostra3<=1.01 & amostra3>=0.99)