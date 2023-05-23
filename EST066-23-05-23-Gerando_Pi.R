# N = min{n:sum(Ui>i)}
# N = max{n:produtoria(Ui >=e⁽⁻³)} //falta isso na ultima questao da lista 03

# Estimacao de Pi --------------------------------------------------------------

# (X,Y) eh um vetor aleatorio distribuido em um quadrado de area H, 
#com centro na origem.

# Seja um quadrado de lado 2 e um circulo de area Pi centrado nele 

# Quero saber a probabilidade de (X,Y) pertencer ao circulo
# P{(X,Y) pertencer ao circulo } = Pi/4

# X ~ Uniforme(-1,1)
# Y ~ Uniforme(-1,1)

# Gerar U1 e U2 IID onde Ui ~ U(0,1)

# X = 2*U1 - 1
# Y = 2*U2 - 1

# Calcula Ii = 1 , se xi²+yi²<=1
#            = 0 , cc

# Quero calcular E(I) e (1/m)*sum(Ii)
# Repetir M = 1000

set.seed(666)

M <- 1000 # numero de repeticoes

U1<-runif(M)
U2<-runif(M)

X = 2*U1 - 1
Y = 2*U2 - 1

I<-(X**2 + Y**2 <= 1)

pi_sobre_4<-mean(I) # expected value

pi_sobre_4*4

# Passar a operacao acima para uma funcao

estima.pi <- function(M = 1000){
  
  U1<-runif(M)
  U2<-runif(M)
  
  X = 2*U1 - 1
  Y = 2*U2 - 1
  
  I<-(X**2 + Y**2 <= 1)
  
  pi_sobre_4<-mean(I) # expected value
  
  pi_sobre_4*4
  
}

# Gerar mil estimativas para pi pela nossa funcao

mil_estimativas<- numeric(1000)
cont<-1

while (cont<=1000){
  mil_estimativas[cont]<-estima.pi()
  cont<- cont + 1
}

# Plotar histograma das 1000 estimativas

hist(mil_estimativas, col ='lightblue', freq = FALSE)

# Erro relativo da funcao estima.pi()

media<-mean(mil_estimativas)
media

erro_relativo<-(abs(mean(mil_estimativas)-pi)/pi)
erro_relativo

# Verificar o intervalo de 95% dos valores (defini esse intervalo 
#entre 2.5 e 97.5%, o padrao do R eh de 0 a 95%)

int.conf.95<-quantile(mil_estimativas,probs = c(0.025,0.975))
int.conf.95

# Verificar erro para diferentes valores de M

emes<-c(10,100,1000,10000,100000)
erros<-numeric(length(emes))
cont<-1

while(cont <= length(emes)){
  
  valores<-estima.pi(emes[cont])
  erros[cont]<-(abs(mean(mil_estimativas)-pi)/pi)
  cont<-cont+1
  
}

erros

