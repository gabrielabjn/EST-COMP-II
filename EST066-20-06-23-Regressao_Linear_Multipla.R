
# ---------------------------------------------------------------------------

# Regressao Linear

# Gerar conjunto de dados que atenda as suposicoes do modelo

# 1- Especificacao do modelo
# 2- Determinacao das variaveis independentes (explicativas)
# 3- Simulacao da variavel independente e do erro aleatorio
# 4- Calculo da variavel dependente (ou resposta)

# erro ε possui distribuicao N(0,sigma²)



# Pratica -------------------------------------------------------------------

ajusta_reta<-function(x,e) 150 - 4*x + e


x<-rnorm(100,50,9)
e<-rnorm(100,0,10)

y<-ajusta_reta(x,e)

cor(y,x)
plot(y,x, col = 'pink2', pch = 19)

mL<-lm(y~x)
summary(mL)

cor(y,x)**2

# Calcular IC

cota_sup<-mL$coefficients[2] + qt(0.975,98)* 0.1203
cota_inf<-mL$coefficients[2] - qt(0.975,98)* 0.1203
# nota: ultimo valor na formula eh o erro padrao de x

# Grafico dos residuos

library(ggfortify)

op<-par()
par(mfrow = c(2,2))
for(i in 1:4) plot(mL)
par(op)

autoplot(mL) # nao sei pq nao foi, o dos colegas foi

confint(mL, 'x', level = 0.95)
confint(mL,'(Intercep)',level = 0.75)

# vetor dos coeficientes do modelo linear (mL)
mL$coefficients

# tabelinha que apareceu no resultado do ajuste do modelo 
summary(mL)$coefficients # da para acessar os elementos como se acessa os de uma matriz

summary(mL)$sigma # desvio padrao estimado do modelo (acho)

# Regressao com Variavel Categorica -----------------------------------------

# y = B0 * B1x * B1I 
# onde I eh uma indicadora de variavel categorica dicotomica, 
# assumindo valor 1 ou 0.



# Pratica II ----------------------------------------------------------------

# x ~ Normal(0,1)
# e ~ N(0,2)

n<-100

grupo<- rep(c('A','B'), each = n/2) # repete 50 vezes cada letra

B_0 <- 5
B_1 <- -2

x <- rnorm(n)
e <- rnorm(n,0,2)

y<- B_0 + B_1*x + B_1*(grupo == "A") + e
y

plot(y,x, col = c('lightblue3', 'blue'), pch = 19)

mL1<-lm(y~x+grupo)
mL1

summary(mL1)

abline(2.9641,-1.9942, col = 'lightblue3')
abline(2.9641 + 1.9138,-1.9942, col = 'lightblue3')


