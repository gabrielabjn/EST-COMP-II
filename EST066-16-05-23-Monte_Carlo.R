# INTEGRACAO MONTE CARLO ------------------------------------------------------
 
# Resolucao de integral definida usando numero aleatorio
# 1º tipo de integral definida

# Theta: integral de g(x) entre 0 e 1 

# Analise de integrais com a finalidade de transforma-las em experanca

# Criando densidade requerida -------------------------------------------------

g<-function(u)((1-u)**4)*u  # acumulada ?

theta.hat<-function(n = 1000, m = 500){
  
m<-50

medias<-numeric(m)
cont<-1

while (cont<=m){
  
  u<-runif(1000)
  
  medias[cont]<-mean(g(u)) 
  
  cont<-cont+1
  
}

return(medias)

}

medias<-theta.hat()

sum(medias-beta(2,5))**2/500

exato<- beta(2,5)
log(mean((medias-exato)**2),10)

#-------------------------------------------------------------------------


vetor_log<-numeric(8)
tam<-c(50,100,250,500,1000,1500,2000,5000)

cont<-1


for (i in tam){

medias<-theta.hat(i,500)

vetor_log[cont]<- log(mean((medias-exato)**2),10)

cont<-cont+1

}

plot(tam, vetor_log, col = 'blue', pch = 19)




