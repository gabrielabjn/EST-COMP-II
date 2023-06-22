# -------------------------------------------------------------------------
# Regressao Linear Multipla - CONTINUACAO 

# c/ variaveis dependentes continuas

# Y = B0 + Bixi + B2x2 + e
# n = 100 observacoes de y e de x_vetor

# x_1 ~ N(50, 9²)
x_1<-rnorm(100,50,9)

# x_2 ~ N(200, 64²)
x_2<-rnorm(100,200,64)

# e ~ N(0,16²)
e<-rnorm(100,0,16)

B0 <- 150
B1 <- (-4)
B2<- 2.5

# Formula Regressao Multipla

y<- B0 + B1*x_1 + B2*x_2 + e
y

# Ajuste da regressao aos dados via lm()

mL.3 <- lm(y~x_1+x_2)
mL.3

summary(mL.3)

# Grafico de x1 e x2

plot(x_1~x_2, col = c('red','blue'), pch = 19)
# dados nao correlacionados!
abline(h=mean(x_1), v = mean(x_2))

cor(x_1,x_2)

# Exercicio para proxima aula ---------------------------------------------

# Repetir mil vezes o processo anterior (criar funcao). plotar graficos
# com os valores gerados de B0,B1,B2 (cada) (grafico density ou lowess).

# Density smoothing em R base

# Quantas vezes o verdadeiro valor pertence ao intervalo de confianca (intervalo)


# -------------------------------------------------------------------------

# Dados de serie temporal

# Nascimentos em nova york
nascimentos <- scan('http://robjhyndman.com/tsdldata/data/nybirths.dat')

nasc.NY<-ts(nascimentos,start = c(1946,1), frequency = 12)

str(nasc.NY) # informacoes sobre o objeto

plot(nasc.NY, col = 'blue', lwd = 1.5)

acf(nasc.NY)

nNY.d12 <- diff(nasc.NY,differences = 12)
plot(nNY.d12, col = 'pink2')

length(nNY.d12)

# Decomposicao da serie temporal

nasc.NY.componentes<-decompose(nasc.NY)
plot(nasc.NY.componentes)

str(nasc.NY.componentes)
plot(nasc.NY.componentes$trend)

# Modelos Arima de Series Temporais

# Y_t = mi + phi_1*y_t-1 + ...+ phi_n*y_t-n + e 

# Y_t = mi + et + theta_1*e_t-1 + theta_2*e_t-2 + ... + theta_m*e_t-m 

mi <- 0 
phi_1 <- 0.18828
phi_2 <- 0.05861
n<-1e3 # 1000
e<-rnorm(1e3)

y_1 <- rnorm(1e3)
y_2<-rnorm(1e3)

# consertar funcao abaixo

Y_t <- mi + phi_1*y_1 + phi_2*y_2 + e

ts<-ts(Y_t)
plot(ts)

pacf(ts)

