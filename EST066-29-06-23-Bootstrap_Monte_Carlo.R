
# Reamostragem

# Nao Parametrica -> TH
#                 -> IC

# Reamostragem (bootstrap, jacknife, validacao cruzada)

# Estimacao -> vies e precisao -> conclusao

# Funcao Distrribuicao Empirica da Amostra Original

original<-c(7,5,3,9,6)

Fn<-ecdf(original)
summary(Fn)

knots(Fn)

plot(Fn, ylim = c(0,1.1), main = "")
text(knots(Fn),1:5/5,knots(Fn), cex = 0.8, pos = 3)

# Geracao de Amostra Bootstrap 

set.seed(666)

amostra.boot<-sample(original, 5, replace = 'T')
amostra.boot
mean(amostra.boot)

amostra.boot.2<-sample(original, 5, replace = 'T')
amostra.boot.2

mean(amostra.boot.2)

library(gtools)
amostras.boot<-permutations(n=5,r=5,v=original,repeats.allowed = T)

dim(amostras.boot)

medias.boot <- apply(amostras.boot,1,mean)
mean(medias.boot)

hist(medias.boot, freq = F, ylab = "Densidade", main = "Todas possiveis")
lines(density(medias.boot), col = 'blue')

# Aprox Monte Carlo da Distribuicao Bootstrap
medias.boot2<-replicate(1000, mean(sample(original,5,replace=T)))
mean(medias.boot2)

hist(medias.boot2, freq = F)
lines(density(medias.boot2), col ='blue')


