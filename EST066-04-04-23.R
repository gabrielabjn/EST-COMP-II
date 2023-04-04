
# 04-04-2023

# Funcao de Distribuicao Empirica ----------------------------------------------

# Seja X1,X2,...,XN uma amostra i.i.d. com funcao de distribuicao acumulada F(x).
# Entao a funcao de distribuicao empirica eh definida como

# Fn_chapeu(x) = (1/n)*sum1{xi<=x}, em que

# 1a: indicadora do evento A

# 1{xi<=x} = 1 (se x1<=x) ou 0 (cc)

# Fn_chapeu: R -----> [0,1] (dominio/contradominio)

# Exemplo ----------------------------------------------------------------------

# X1 = 3, X2 = 2, X3 = 5, X4 = 2

# F4(4) = 3/4 (faca as contas!)
# compare os valores o obtidos pela distribuicao real e pela verdadeira.

# 1) Fn_chapeu(x) eh estimador nao viciado  do verdadeiro valor Fx(x)

# 2) A variancia de Fn_chapeu(x) tende a zero quando o tamanho amostral cresce
# (n ---> inf)

# var[Fn_chapeu(x)] = (1/n) Fx(x) [1 - Fx(x)]

#-------------------------------------------------------------------------------

# Conjunto de Dados 'rock'

summary(rock) # panorama da 'cara' do conjunto de dados
?rock

#par(mfrow = c(2,2)) # plotar histogramas
hist(rock$area, freq = F)
hist(rock$area, freq = F, breaks = 100, main = 'breaks = 100') # testa com breaks maiores
hist(rock$area, freq = F, breaks = 30, main = 'breaks = 30')

ecdf(rock$area) # empirical cumulative distribution function
?ecdf

Fn<-ecdf(rock$area)
str(Fn)
plot(Fn)
summary(Fn)

summary(rock$area)
Fn(8000)

fda_empirica<-Fn(c(4000, 6000, 8000))

Fn(c(4000, 6000, 8000)) # eh vetorizada?

# sim, pois a funcao acima teve retorno.

# Como ajustar uma curva normal ao conjunto de dados? --------------------------

?pnorm

# Inserimos a media e o desvio padrao do conjunto de dados em pnorm.

dados<-c(4000, 6000, 8000)

fda_normal<-pnorm(dados, mean = mean(rock$area), sd = sd(rock$area))

# Supomos que os dados seguem uma distribuicao normal e obtemos a fdp para esses 
# dados com a funcao de distribuicao acumulada da normal.

# O proximo passo eh estabelecer um comparativo entre os valores obtidos com a
# funcao empirica e a pnorm.

fda_empirica # ajustada a 'dados'
fda_normal # ajustada a 'dados'

fda_empirica - fda_normal

hist(rock$area, freq = F)
media = mean(rock$area)
dp = sd(rock$area)

plot(Fn)
curve(pnorm(x, mean = media, sd = dp), add = T, lty = 2, 
            col = 'red', lwd = 2)
abline(h = 0.5, lty = 3, col = 'blue')

Xis<-rock$area
eqm<-sum((Fn(Xis)-pnorm(Xis,mean = media, sd = sd))**2)
eqm # erro quadratico medio
eqm/48
log(eqm/48)

# Funcao que calcula a distancia media quadratica em qualquer vetor de pontos

min(rock$area)
max(rock$area)

calcula_eqm <- function(vet){ # vetor de tam n 

   m <- mean(vet)
   d<- sd(vet)
  
   return( sum((Fn(vet)-pnorm(vet,mean = m , sd = d))**2) )
 


}

calcula_eqm(c(1,2,3,4,5))


  




