# Circuito Eletrico
set.seed(666)
# funciona quando as correntes estao fechadas (ou funcionando)

# a probabilidade do sistema funcionar eh dada pelo sistema abaixo

funcao<-function(p) 2*p**2 + 2*p**3 -5*p**4 + 2*p**5

funcao(1/2) # se p = 0.5, a prob do sistema funcionar eh 0.5

# modelar se a chave funciona ou nao funciona
# 1 - chave funciona
# 0 - chave nao funciona

# 1-p prob de chave estar fechada
p<-0.5

# chaves estao abertas ou fechadas?
chaves<-sample(c(0,1),5, replace=T,prob=c(1-p,p))
chaves

# matriz de adjacencias (usada para modelar um sistema de grafos) 
M<-matrix(data = c(1,1,0,0,1,1,0,0,0,0,0,1,0,1,0,0,1,1,0,0,1,0,0,0,1), byrow = TRUE, ncol=5,nrow=5)

# nome das linhas/colunas da matriz: a,b,c,d,e

M2<-M %*% M # tres caminhos de a para a
M2

M4<- M2 %*% M2
M4

C<-sample(c(0,1),5, replace=T,prob=c(1-p,p))

# matriz representando vertices e chaves de um circuito eletrico simples

# nome das linhas/colunas da matriz: l,d,u,r (vertices do circuito)

# primeiro caso

C<-c(1,0,1,0,1)

M_2<-matrix(c(0,C[4],C[1],0,0,0,C[3],C[5],0,C[3],0,C[2],0,0,0,1), byrow=TRUE, nrow=4)
M_2

M_2_cubo<-M_2%*%M_2%*%M_2
M_2_cubo


# segundo caso

C<-c(1,0,1,1,0)

M_2<-matrix(c(0,C[4],C[1],0,0,0,C[3],C[5],0,C[3],0,C[2],0,0,0,1), byrow=TRUE, nrow=4)
M_2


M_2_cubo<-M_2%*%M_2%*%M_2
M_2_cubo

situacao<-min(1,M_2_cubo[1,4])
if(M_2[1,4] == 0) situacao <- 0 else {situacao <-1}


# colocar o acima numa funcao
simula.sys <- function(p){
  
  C<-sample(c(0,1),5, replace=T,prob=c(1-p,p))
  
  M_2<-matrix(c(0,C[4],C[1],0,0,0,C[3],C[5],0,C[3],0,C[2],0,0,0,1), byrow=TRUE, nrow=4)
  M_2
  
  M_2_cubo<-M_2%*%M_2%*%M_2
  
  situacao<-min(1,M_2_cubo[1,4])
  
  return(situacao)
  
  
}

repeticoes<-replicate(1e3,simula.sys(0.9))

sum(repeticoes)/1e3


# Variacoes do Sistema

# (1) Componente 3     p = 0.5

#     Demais componentes 1,2,4,5    p = 0.8


# (2) Nessa situacao,

#     Qtd de componentes 1,2,4 e 5

#     P{sistema funcionar} > 0.99








