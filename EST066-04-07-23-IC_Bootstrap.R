# ------------------ EST066 - Estatistica Computacional II ------------------ #
# Data: 04/07/2023

# Bootstrap para intervalo de confianca
# Não são exatos

# amostra de 40 observações de resistência à deformação
surimi <- c(41.28, 45.16, 34.75, 40.76, 43.61, 39.05, 41.20, 41.02, 41.33,
            40.61, 40.49, 41.77, 42.07, 44.83, 29.12, 45.59, 41.95, 45.78,
            42.89, 40.42, 49.31, 44.01, 34.87, 38.60, 39.63, 38.52, 38.52,
            43.95, 49.08, 50.52, 43.85, 40.64, 45.86, 41.25, 50.35, 45.18,
            39.67, 43.89, 43.89, 42.16)
summary(surimi)
c(variancia = var(surimi), desvio = sd(surimi))

hist(surimi, freq = F, col = "lightblue")
lines(density(surimi), col = "blue")

# teste de normalidade dos dados
(surimi.test <- shapiro.test(surimi))

# qq-plot
qqnorm(surimi); qqline(surimi)
text(1, 35, cex = 0.8, paste0(surimi.test$method,"\np = ", round(surimi.test$p.value, 3)))

# IC t com 95% - assumindo normalidade (tamanho amostral)
t.test(surimi)$conf.int[1:2]

# Vamos construir um IC --------------------------------------------------
set.seed(666)
u0<-mean(surimi)
ep<-sd(surimi/sqrt(n)) # erro padrao

# qute de amostras bootstrap 
B <- 1000

# Transformar distribuicao da amostra numa padrao

# T = [(x_linha - mi)/ var_amostral]*n^(1/2)

# cálculo média e estatística t por amostra bootstrap
est.boot <- function(x) {list(med = mean(x), te = sqrt(n)*(mean(x)-u0)/sd(x))}
matriz <- replicate(B, est.boot(sample(surimi, n, replace = T)))
amostras.boot <- matrix(unlist(matriz),ncol = 2, byrow=T)
colnames(amostras.boot) <- c("theta", "t")
theta.star <- amostras.boot[, "theta"]
t.star <- amostras.boot[, "t"]

# compara média amostral com as médias bootstrap
c(u0, mean(theta.star))

# comapara erro padrão da amostra com erro padrão bootstrap
c(Sh, sd(theta.star))

# aparenta simetria e normalidade?
summary(t.star, col = 'lightpink')

# qqnrom da t.star
qqnorm(t.star, col = 'lightpink')
qqline(t.star, col = 'red', lwd = '2.5')

# quantis dos percentis t bootstrap 2.5% 97.5%
quantile(t.star, probs = c(0.025,0.975))

# quantis de t com n-1 graus de liberdade
qt(c(0.025,0.975), n - 1)

# intervalo de confiança bootstrap percentil t
u0 + quantile(t.star, probs = c(0.025,0.975)) * Sh

# intervalo de confiança t de student
u0 + qt(c(0.025,0.975), n - 1) * Sh


# Regressao por Bootstrap

# reamostragem de mais de uma variável
set.seed(666)
(xy <- data.frame(x = runif(10), y = runif(10)))

plot(y ~x, data = xy, xlim = c(0, 1), ylim = c(0, 1))
# correlação original
cor(xy$x, xy$y)

# IC paramétrico para correlação
cor.test(xy$x, xy$y)$conf.int[1:2]

