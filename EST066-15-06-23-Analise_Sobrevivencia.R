
# ---------- dados ---------- #

# dados categoricos e multivariados (quantitativos e mistos)

# dados no tempo -  series temporais e longitudinais

# dados no espaco

# dados de sobrevida (analise de sobrevida, confiabilidade)

# dados incompletos


# ---------- representacao ---------- #

# t : v.a. representando tempo de sobrevida de individuos.
# c : v.a. representando o tempo de censura associado a este individuo.

# valor observado y = min(t,c)

# sigma { 1, se yi eh tempo de falha (sobrevida)
#       { 0, se yi eh tempo censurado

# (yi, si) 
# xi_vetor = (sexo, idade, tratamento)

# -------------------------------------------------------------------

# Amostra de tamanho n 

# ti : Tempo ate desfecho (falha) do individuo i

# ci : tempo de perda acompanhamento

# Valores efetivamente observados

# yi = min{ti, ci}

# si = I{ti<=ci} (termo entre chaves eh indice neste caso)

# Assuma que -----------------------------------------------------------

# Ti ~ Weibull(delta,gama)
# Ci ~ Weibull(delta,theta)

# Ti e Ci sao independentes

# Pode-se provar que a proporcao de observacoes censuradas eh dada por

# Pc = theta/(theta+gama)

# Executar para  { delta = 2,5
#                { gama = 5

# Pc = theta/(theta + gama)

# Fazer para... ---------------------------------------------------

# Pc = 1%, 10% e 30%

p_c = 0.1
delta = 2.5
gama = 5

# Encontrar theta

encontra_theta<-function(p_c, gama){ 
 
  theta <- p_c*gama/(1-p_c)
  
  return(theta)


}

encontra_theta(p_c,gama)


# 2. Gerar pares (ti,ci) a funcao de distribuicao acumulada

# F(ti) = 1 - exp(-gama*ti)^delta

# F(ci) = 1 - exp(-theta*ci)^delta

u1<-runif(50)
u2 <- runif(50)

t<-1-exp(-(gama*u1))
c<-1-exp(-(delta*u2))

y<-min(t,c)
delta_i <- as.numeric(t<=c)
