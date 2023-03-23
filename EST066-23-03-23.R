# 23 de março de 2023

# Cauchy
# funcao "patologica"
# nao tem nenhum momento (nem fgm)
# (nao tem E(X), E(X²), etc)

# quantis da normal padrao

qnorm(0.25)
qnorm(0.75)

# funcoes acumuladas 

curve(dnorm, -6, to = 6, add = T)
curve(dcauchy, -6, to = 6, add = T, col = 'red')

curve(dnorm, from = -10, to = 10, col = 'gray', add= T)

# comparando massas nas caudas das distribuicoes

pnorm(3) - pnorm(-3)

pcauchy(3) - pcauchy(-3)

# porcentagem que o valor de uma equacao tem a mais que a outra 

(1-0.9973)/2

qcauchy(0.00135)

# sorteio de 1000 valores da cauchy

amostra_cauchy<-rcauchy(1000)
mean(amostra_cauchy)
median(amostra_cauchy)
diff((range(amostra_cauchy)))
range(amostra_cauchy)
IQR(amostra_cauchy) # amostra interquantilica (entre terceiro e primeiro 
# quantis)

# comando do r base que gera a funcao de distribuicao empirica



