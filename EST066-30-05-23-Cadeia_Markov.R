
# Cadeia de Markov - Passeio Aleatorio

# Matriz P de Probabilidade de Transicao

P<-matrix(data = 0, nrow = 6, ncol = 6)

P[1,]<-c(0.5,0.25,0,0,0,0.25)
P[2,]<-c(0.25,0.5,0.25,0,0,0)
P[3,]<-c(0,0.25,0.5,0.25,0,0)
P[4,]<-c(0,0,0.25,0.5,0.25,0) 
P[5,]<-c(0,0,0,0.25,0.5,0.25)
P[6,]<-c(0.25,0,0,0,0.25,0.5)

P2<-P%*%P
P2 # matriz ao quadrado (2 passos)

apply(P2,1,sum) # soma das linhas eh 1, conforme esperado.

P4<- P2%*% P2
P4 # matriz a quarta (4 passos)

P8<-P4%*%P4 
P8

P16<-P8%*%P8
P16

P32<-P16%*%P16
P32 

P64<-P32%*%P32
P64 # distribuicao invariante da cadeia encontrada (pode ter ocorrido ate antes
# de 64 passos na verdade)

# Calcular cadeia em 10 passos

library(expm)
P%^%10

val<-numeric(10)
val[1]<-sample(1:6,prob=P[6,], size = 1)

for (i in 2:10){
  
  val[i]<-sample(1:6, prob = P[val[i-1],], size = 1)
  
  
}

val

colnames(P)<-paste0("Est",1:6)->rownames(P)

# Gerar uma funcao para repetir o que foi feito anteriormente

passeio <- function(n, ini = 1){
  
  vet<-numeric(n)
  vet[1]<- sample(1:6, prob = P[ini,], size = 1)
  
  i<-2
  
  while(i <= n){
    vet[i]<-sample(1:6, prob = P[val[i-1],], size = 1)
    i<-i+1
  }
  
  return(vet)
  
}

passeio(1000, ini = 6)

# abaixo, era p ser com 'passeio' no lugar de val mas a minha funcao deu erro
# por algum motivo (na maquina da maria rodou)

sapply(1:10,function(x)table(val[1:x]))

val == 1

freq<-cumsum(val==1)/(1:1000)

plot(freq, type = 'l', ylim = c(0,0.4))




