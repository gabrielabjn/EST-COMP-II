
# Bootstrap parametrico

# Distribuicao amostral     
# IC e TH                         

# -------------------------------------------------------------------------#

# Sejam y1, y2, ..., yn a.a. de populacao exponencial f(y) = e⁻(y-theta)

# Dessa amostra, calculo a media y_barra

# W = Y_barra - theta tem distribuicao exponencial padrao

# P {Y_barra - W1 < theta < Y - W2} = gamma


# -------- Mediana amostral de amostra exponencial -------------------------#

options(digits = 7)
n <- 21
taxa <- 1

# Gunção densidade da mediana (tamanho amostral ímpar)
f.median <- function(x){
  cte <- 1/beta((n + 1)/2, (n + 1)/2)
  cte * pexp(x)^((n-1)/2)*(1 - pexp(x))^((n - 1)/2) * dexp(x)
} 
# Gráfico da densidade
curve(f.median, from = 0.2, to = 1.5)

esperanca_expo<-function(w) w*f.median(w)

integrando<-integrate(f = esperanca_expo, lower = 0, upper = Inf)$value

esperanca<-str(integrando)
esperanca<-as.array(integrando)

esperanca_expo_q<-function(w) (w**2)*f.median(w)
integrando_ao_quadrado<- integrate(f = esperanca_expo_q, lower = 0, upper = Inf)$value

esperanca_w_quadrado<-str(integrando_ao_quadrado)
esperanca_w_quadrado<-as.array(integrando_ao_quadrado)


# Var(W) = E(W²) - [E[W]]²

var<-(esperanca_w_quadrado - esperanca**2)
var

# Aplicar bootstrap e comparar resultados ---------------------------------

# taxa de falha = f(x)/ (1- F(x)) 
#              ou f(x)/S(x) # sobrevivencia


# ------ distribuição empírica da mediana amostral

# valor estimado da esperança e da variância da mediana amostral
#set.seed(666)
# qte. de amostras bootstrap
B <- 10000
n <- 21

# medianas amostrais boostrap
medianas <- replicate(B, median(rexp(n = n, rate = taxa)))

# média e variância das medianas amostrais bootstraps
(media <- mean(medianas))

(variancia <- var(medianas)) 

# histograma das B amostras bootstrap
hist(medianas, freq = F, xlab = "Valores observados", ylab = "Densidade",
     main = "Histograma das medianas simuladas")
text(0.2, 1.65, "Valores exatos", cex = 0.8, pos = 3, col = "blue")
text(0.2, 1.50, expression(lambda == 1), cex = 0.8, pos = 3, 
     col = "blue")
text(0.2, 1.30, expression(mu == 0.70286), cex = 0.8, pos = 3, 
     col = "blue")
text(0.2, 1.10, expression(sigma^2 == 0.01978), pos = 3, cex = 0.8, 
     col = "blue")
text(1.2, 1.65, "Valores estimados", pos = 3, cex = 0.8)
text(1.2, 1.5, paste("n =", n), pos = 3, cex = 0.8)
text(1.2, 1.30, bquote(bar(x)[med] == .(round(media, 5))), pos = 3, 
     cex = 0.8)
text(1.2, 1.10, bquote(s[med]^2 == .(round(variancia, 5))), pos = 3, 
     cex = 0.8)

# densidade exata das medianas amostrais bootstrap
curve(f.median, from = 0.2, to = 1.5, col = "blue", add = T)

# ---- Percentis estimados de W
percentis <- quantile(medianas, prob = c(0.05, 0.95))
percentis


#v_exp<-rexp(21)+4.5 # exponencial deslocada

y.til <- replicate(100,median(rexp(21)+4.5))
matriz<- matrix(0, nrow = 100, ncol = 2)

for (i in 1:100) matriz[i,]<-y.til[i] - percentis

cobre <- (matriz[,2] <= 4.5 & matriz[,1] >= 4.5)
mean(cobre)
