dados = read.table("dados.txt", header=TRUE)
dados

names(dados)
attach(dados)



################################################
			BIBLIOTECAS
################################################
library(tseries)
library(MASS)
library(lmtest)
library(car)
library(sandwich)



################################################
		    ANÁLISE DESCRITIVA
################################################
# Descritiva da variável resposta
summary(price)
sd(price)
par(mfrow=c(1,2))
boxplot(price, col="gray", xlab="Preço de venda da casa")
hist(price, freq=FALSE, col="gray", xlab="Preço de venda da casa", ylab="Densidade", main="")
jarque.bera.test(price) # a variável resposta não é normal

# Descritiva da variável explicativa quantitativa
# Transformando a variável lotsize de pés quadrados para metros quadrados
lotsize = 0.09*lotsize
summary(lotsize)
sd(lotsize)

# Tabela de frequência das outras variáveis explicativas
table(bedrooms)
table(bathrms)
table(stories)
table(driveway)
table(recroom)
table(fullbase)
table(gashw)
table(airco)
table(garagepl)
table(prefarea)

# Gráfico de dispersão entre a variável resposta e a variável contínua 'lotsize'
plot(lotsize, price, xlab="Tamanho do lote", ylab="Preço de venda da casa")

# Construindo as variáveis dummy
# Como a variável bedrooms apresenta poucas observações nas categorias 1 e 6, agrupamos as categorias 1 e 2, assim como as categorias 5 e 6
bedrooms12 = ifelse(bedrooms==1 | bedrooms==2, 1, 0)
bedrooms3 = ifelse(bedrooms==3, 1, 0)
bedrooms4 = ifelse(bedrooms==4, 1, 0)
bedrooms56 = ifelse(bedrooms==5 | bedrooms==6, 1, 0) 
bathrms2 = ifelse(bathrms==2, 1, 0)
# Como a categoria bathrms = 4 só tem uma observação, agrupamos bathrms = 3 e = 4
bathrms34 = ifelse(bathrms==3 | bathrms==4, 1, 0) 
stories2 = ifelse(stories==2, 1, 0)
stories3 = ifelse(stories==3, 1, 0)
stories4 = ifelse(stories==4, 1, 0)
garagepl1 = ifelse(garagepl==1, 1, 0)
garagepl2 = ifelse(garagepl==2, 1, 0)
garagepl3 = ifelse(garagepl==3, 1, 0)

bedrooms12 = factor(bedrooms12, labels=c("Não", "Sim"))
bedrooms3 = factor(bedrooms3, labels=c("Não", "Sim"))
bedrooms4 = factor(bedrooms4, labels=c("Não", "Sim"))
bedrooms56 = factor(bedrooms56, labels=c("Não", "Sim"))
bathrms2 = factor(bathrms2, labels=c("Não", "Sim"))
bathrms34 = factor(bathrms34, labels=c("Não", "Sim"))
stories2 = factor(stories2, labels=c("Não", "Sim"))
stories3 = factor(stories3, labels=c("Não", "Sim"))
stories4 = factor(stories4, labels=c("Não", "Sim"))
garagepl1 = factor(garagepl1, labels=c("Não", "Sim"))
garagepl2 = factor(garagepl2, labels=c("Não", "Sim"))
garagepl3 = factor(garagepl3, labels=c("Não", "Sim"))



################################################
		    AJUSTE DO MODELO
################################################
# Problema: a variável resposta não é normal
# Possível solução: Transformação de Box-Cox
BC = boxcox(price~lotsize + bedrooms3 + bedrooms4 + bedrooms56 + bathrms2 + bathrms34 + stories2 + stories3 + stories4 + driveway + recroom + fullbase + gashw + airco + garagepl1 + garagepl2 + garagepl3 + prefarea)
lambda = BC$x[BC$y==max(BC$y)] ##dá o lambda##
price.bc = (price^(lambda)-1)/(lambda)
modelo.bc  =  lm(price.bc~bedrooms3 + bedrooms4 + bedrooms56 + bathrms2 + bathrms34 + stories2 + stories3 + stories4 + driveway + recroom + fullbase + gashw + airco + garagepl1 + garagepl2 + garagepl3 + prefarea)
summary(modelo.bc)
# como IC 95% de lambda contem 0, usaremos a transformação log na variável resposta

# Aplicando a transformação log na variável resposta
logprice = log(price)
hist(logprice, freq=FALSE, col="gray", xlab="log(Preço de venda da casa)", ylab="Densidade", main="")
jarque.bera.test(logprice) # com o log, a var resposta ficou normal


# Verificando se há multicolinearidade
# Matriz contendo apenas os regressores
modelo_completo1 = lm(logprice~lotsize + bedrooms3 + bedrooms4 + bedrooms56 + bathrms2 + bathrms34 + stories2 + stories3 + stories4 + driveway + recroom + fullbase + gashw + airco + garagepl1 + garagepl2 + garagepl3 + prefarea)
# R^2 ajustado
R2_1 = summary(modelo_completo1)$adj.r.squared;R2_1
# VIF
vif(modelo_completo1)

# Aplicando a transformação log na variável explicativa contínua lotsize
loglotsize = log(lotsize)
modelo_completo2 = lm(logprice~loglotsize + bedrooms3 + bedrooms4 + bedrooms56 + bathrms2 + bathrms34 + stories2 + stories3 + stories4 + driveway + recroom + fullbase + gashw + airco + garagepl1 + garagepl2 + garagepl3 + prefarea)
# R^2 ajustado
R2_2 = summary(modelo_completo2)$adj.r.squared;R2_2
# VIF
vif(modelo_completo2)

# modelo selecionado até agora: modelo_completo2, por possuir maior R^2 ajustado


########################################################################################
# Testando modelo completo com todas as interações
modelo.BIC = stepAIC(modelo_completo2,scope=list(lower = ~1,upper = ~.^2),k=log(546))

# Teste de heteroscedasticidade - teste de Koenker
bptest(modelo.BIC)

jarque.bera.test(modelo.BIC$resid)

# Identificando se há pontos de alavanca
chapeu=hatvalues(modelo.BIC)
plot(chapeu, xlab="Índice", ylab="Alavancagem", main="")
X = model.matrix(modelo.BIC)
cut = 3*(ncol(X))/(nrow(X))
abline(cut,0,lty=2)
identify(chapeu, n=2)

influence.measures(modelo.BIC) ##Medidas: DFBETA, DFFITS, D. de Cook e cov.ratio.
summary(influence.measures(modelo.BIC))

# Como o modelo.BIC é heteroscedástico e ainda há pontos de alavanca, usaremos o estimador HC4 para testar a significância dos parâmetros sob heteroscedasticidade
coeftest(modelo.BIC, vcov=vcovHC (modelo.BIC, type="HC4"))

# Seleção do modelo final
# tirando prefarea e interação de prefarea com driveway
modelo1 = lm(logprice~loglotsize + bathrms2 + bathrms34 + stories2 + stories3 + stories4 + driveway + fullbase + gashw + airco + garagepl1 + garagepl2 + gashw*garagepl2)
coeftest(modelo1, vcov=vcovHC (modelo1, type="HC4"))

# tirando a interação de gashw com garagepl2
modelo2 = lm(logprice~loglotsize + bathrms2 + bathrms34 + stories2 + stories3 + stories4 + driveway + fullbase + gashw + airco + garagepl1 + garagepl2)
coeftest(modelo2, vcov=vcovHC (modelo2, type="HC4"))


# Modelo final: modelo2
# Teste de heteroscedasticidade - Teste de Koenker
bptest(modelo2)

# Normalidade dos residuos do modelo
# Teste de normalidade dos resíduos
# H0: normalidade dos resíduos
jarque.bera.test(modelo2$resid)
par(mfrow=c(1,2))
hist(modelo2$resid, freq=FALSE, col="gray", xlab="Resíduos do modelo", ylab="Densidade", main="")
qqnorm(modelo2$resid);qqline(modelo2$resid, col=2)

# Identificando se há pontos de alavanca
chapeu2=hatvalues(modelo2)
plot(chapeu2, xlab="Índice", ylab="Alavancagem", main="")
X2 = model.matrix(modelo2)
cut2 = 3*(ncol(X2))/(nrow(X2))
abline(cut2,0,lty=2)

influence.measures(modelo2) ##Medidas: DFBETA, DFFITS, D. de Cook e cov.ratio.
summary(influence.measures(modelo2))

# Teste de presença de outliers
#H0: não há outliers
outlier.test(modelo2) 

# Teste de especificidade do modelo
# H0: o modelo está corretamente especificado 
resettest(modelo2)


##### FUNÇÃO DIAG #####
fit.model = modelo2
lms = summary(fit.model)
X = model.matrix(fit.model)
n = nrow(X)
p = ncol(X)
H = X%*%solve(t(X)%*%X)%*%t(X)
h = diag(H)
lms = summary(fit.model)
s = lms$sigma
r = resid(lms)
ts = r/(s*sqrt(1-h))
di = (1/p)*(h/(1-h))*(ts^2)
si = lm.influence(fit.model)$sigma
tsi = r/(si*sqrt(1-h))
a = max(tsi)
b = min(tsi)
par(mfrow=c(2,2))
#PONTOS DE ALAVANCA
plot(fitted(fit.model),h,xlab="Indice", ylab="Leverage", pch=16,
main="Pontos de Alavanca", ylim=c(0,1))
cut = 3*p/n
abline(cut,0,lty=2)
#identify(fitted(fit.model),h, n=2)
#PONTOS INFLUENTES
plot(di,xlab="Indice", ylab="Distancia de Cook", pch=16,
main="Pontos Influentes", ylim=c(0,1))
#identify(di, n=2)
#OUTLIERS
plot(tsi,xlab="Indice", ylab="Resíduo Studentizado",
ylim=c(b-1,a+1), pch=16, main="Outliers")
abline(2,0,lty=2)
abline(-2,0,lty=2)
#identify(tsi, n=3)
#par(mfrow=c(1,1))
#Homoscedasticidade
plot(fitted(fit.model),tsi,xlab="Valores Ajustados", 
ylab="Residuo Studentizado", ylim=c(b-1,a+1), pch=16,
main="Homoscedasticidade")
abline(2,0,lty=2)
abline(-2,0,lty=2)
#identify(fitted(fit.model),tsi, n=3)

##### ENVELOPE DA NORMAL #####
par(mfrow=c(1,1))
X = model.matrix(fit.model)
n = nrow(X)
p = ncol(X)
H = X%*%solve(t(X)%*%X)%*%t(X)
h = diag(H)
si = lm.influence(fit.model)$sigma
r = resid(fit.model)
tsi = r/(si*sqrt(1-h))
#
ident = diag(n)
epsilon = matrix(0,n,100)
e = matrix(0,n,100)
e1 = numeric(n)
e2 = numeric(n)
#
for(i in 1:100){
     epsilon[,i] = rnorm(n,0,1)
     e[,i] = (ident - H)%*%epsilon[,i]
     u = diag(ident - H)
     e[,i] = e[,i]/sqrt(u)
     e[,i] = sort(e[,i]) }
#
for(i in 1:n){
     eo = sort(e[i,])
     e1[i] = (eo[2]+eo[3])/2
     e2[i] = (eo[97]+eo[98])/2 }
#
med = apply(e,1,mean)
faixa = range(tsi,e1,e2)
#
par(pty="s")
qqnorm(tsi,xlab="Percentis da N(0,1)",
ylab="Residuo Studentizado", ylim=faixa, pch=16)
par(new=T)
qqnorm(e1,axes=F,xlab="",ylab="",type="l",ylim=faixa,lty=1)
par(new=T)
qqnorm(e2,axes=F,xlab="",ylab="", type="l",ylim=faixa,lty=1)
par(new=T)
qqnorm(med,axes=F,xlab="",ylab="",type="l",ylim=faixa,lty=2)