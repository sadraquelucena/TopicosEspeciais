dados = read.table("dados.txt", header=TRUE)
dados

names(dados)
attach(dados)



##### BIBLIOTECAS #####
library(tseries)
library(nortest)
library(MASS)
library(lmtest)
library(car)
library(sandwich)


##### ANÁLISE DESCRITIVA #####
# Descritiva da variável resposta
summary(price)
boxplot(price, col="gray")
hist(price, freq=FALSE, col="gray", xlab="Preço de venda da casa", ylab="Densidade", main="")
jarque.bera.test(price) # a variável resposta não é normal

# Descritiva das variáveis explicativas quantitativas (discretas e contínuas)
summary(lotsize)

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
cor(lotsize, price)
legend("topleft", legend="correlação=0.536")


# Construindo as variáveis dummy
# Como a variável bedrooms apresenta poucas observações nas categorias 1 e 6, agrupamos as categorias 1 e 2, assim como as categorias 5 e 6
bedrooms12 = rep(0,length(dados[,1]))
for(i in 1:length(dados[,1]))
{

	if(bedrooms[i] == 1 || bedrooms[i] == 2)
		bedrooms12[i] = 1
}

# Como a categoria bathrms = 4 só tem uma observação, agrupamos bathrms = 3 e = 4
bedrooms3 = bedrooms4 = bedrooms56 = rep(0,length(dados[,1]))
bathrms2 = bathrms34 = rep(0,length(dados[,1]))
stories2 = stories3 = stories4 = rep(0,length(dados[,1]))
garagepl1 = garagepl2 = garagepl3 = rep(0,length(dados[,1]))

for(i in 1:length(dados[,1]))
{
	if(bedrooms[i] == 3)
		bedrooms3[i] = 1
	if(bedrooms[i] == 4)
		bedrooms4[i] = 1
	if(bedrooms[i] == 5 || bedrooms[i] == 6)
		bedrooms56[i] = 1
	if(bathrms[i] == 2)
		bathrms2[i] = 1
	if(bathrms[i] == 3 || bathrms[i] == 4)
		bathrms34[i] = 1
	if(stories[i] == 2)
		stories2[i] = 1
	if(stories[i] == 3)
		stories3[i] = 1
	if(stories[i] == 4)
		stories4[i] = 1
	if(garagepl[i] == 1)
		garagepl1[i] = 1
	if(garagepl[i] == 2)
		garagepl2[i] = 1
	if(garagepl[i] == 3)
		garagepl3[i] = 1
}

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



##### AJUSTE DO MODELO #####
# 1º problema: a variável resposta não é normal
# Possível solução: Transformação de Box-Cox
BC = boxcox(price~lotsize + bedrooms3 + bedrooms4 + bedrooms56 + bathrms2 + bathrms34 + stories2 + stories3 + stories4 + driveway + recroom + fullbase + gashw + airco + garagepl1 + garagepl2 + garagepl3 + prefarea)
lambda = BC$x[BC$y==max(BC$y)] ##dá o lambda##
price.bc = (price^(lambda)-1)/(lambda)
modelo.bc  =  lm(price.bc~bedrooms3 + bedrooms4 + bedrooms56 + bathrms2 + bathrms34 + stories2 + stories3 + stories4 + driveway + recroom + fullbase + gashw + airco + garagepl1 + garagepl2 + garagepl3 + prefarea)
summary(modelo.bc)
# como IC 95% de lambda contem 0, usaremos a transformação log na variável resposta
# para o IC 95% lambda=1 está no intervalo? Resposta: Não! ##
# Lambda igual à 1 significa que modelo Box-Cox é igual ao modelo linear ##

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

# Aplicando a transformação log à variável explicativa contínua lotsize
loglotsize = log(lotsize)
modelo_completo2 = lm(logprice~loglotsize + bedrooms3 + bedrooms4 + bedrooms56 + bathrms2 + bathrms34 + stories2 + stories3 + stories4 + driveway + recroom + fullbase + gashw + airco + garagepl1 + garagepl2 + garagepl3 + prefarea)
# R^2 ajustado
R2_2 = summary(modelo_completo2)$adj.r.squared;R2_2
# VIF
vif(modelo_completo2)

# modelo selecionado até agora: modelo_completo2, por possuir maior R^2 ajustado


########################################################################################
# Modelo inicial: log-log
modelo0 = lm(logprice~loglotsize + bedrooms3 + bedrooms4 + bedrooms56 + bathrms2 + bathrms34 + stories2 + stories3 + stories4 + driveway + recroom + fullbase + gashw + airco + garagepl1 + garagepl2 + garagepl3 + prefarea)
summary(modelo0)
bptest(modelo0)



# Teste de especificação correta - teste RESET
# H0: modelo está corretamente especificado. Se p-valor<0,05, rejeito H0
resettest(modelo0)

# Seleção do modelo via critério de seleção AIC
modelo1 = stepAIC(modelo0)
summary(modelo1)
resettest(modelo1)
bptest(modelo1)
coeftest(modelo1, vcov=vcovHC (modelo1, type="HC4"))

# tirando bedrooms56
modelo2 = lm(logprice~loglotsize + bedrooms3 + bedrooms4 + bathrms2 + bathrms34 + stories2 + stories3 + stories4 + driveway + recroom + fullbase + gashw + airco + garagepl1 + garagepl2 + prefarea)
coeftest(modelo2, vcov=vcovHC (modelo2, type="HC4"))

# tirando bedrooms4
modelo3 = lm(logprice~loglotsize + bedrooms3 + bathrms2 + bathrms34 + stories2 + stories3 + stories4 + driveway + recroom + fullbase + gashw + airco + garagepl1 + garagepl2 + prefarea)
coeftest(modelo3, vcov=vcovHC (modelo3, type="HC4"))

# tirando bedrooms3
modelo4 = lm(logprice~loglotsize + bathrms2 + bathrms34 + stories2 + stories3 + stories4 + driveway + recroom + fullbase + gashw + airco + garagepl1 + garagepl2 + prefarea)
coeftest(modelo4, vcov=vcovHC (modelo4, type="HC4"))

# tirando recroom
modelo5 = lm(logprice~loglotsize + bathrms2 + bathrms34 + stories2 + stories3 + stories4 + driveway + fullbase + gashw + airco + garagepl1 + garagepl2 + prefarea)
coeftest(modelo5, vcov=vcovHC (modelo5, type="HC4"))

# Nessa 1ª etapa, o melhor modelo foi: modelo5!


########################################################################################
# Testando modelo completo com todas as interações
#X = data.frame(loglotsize, bedrooms3, bedrooms4, bedrooms56, bathrms2, bathrms34, stories2, stories3, stories4, driveway, recroom, fullbase, gashw, airco, garagepl1, garagepl2, garagepl3, prefarea)
#modelo_int = lm(logprice ~ .^2, data=X)
#modelo.AIC = stepAIC(modelo_int, k=log(546))
#summary(modelo.AIC)
#bptest(modelo.AIC)
#jarque.bera.test(modelo.AIC$resid)
#coeftest(modelo.AIC, vcov=vcovHC (modelo.AIC, type="HC4"))

modelo.AIC2 = stepAIC(modelo0,scope=list(lower = ~1,upper = ~.^2),k=log(546))
coeftest(modelo.AIC2, vcov=vcovHC (modelo.AIC2, type="HC4"))

# tirando prefarea e interação de prefarea com driveway
modelo6 = lm(logprice~loglotsize + bathrms2 + bathrms34 + stories2 + stories3 + stories4 + driveway + fullbase + gashw + airco + garagepl1 + garagepl2 + gashw*garagepl2)
coeftest(modelo6, vcov=vcovHC (modelo6, type="HC4"))

# tirando gashw e interação de gashw com garagepl2
modelo7 = lm(logprice~loglotsize + bathrms2 + bathrms34 + stories2 + stories3 + stories4 + driveway + fullbase + airco + garagepl1 + garagepl2)
coeftest(modelo7, vcov=vcovHC (modelo7, type="HC4"))

# Nessa 2ª etapa, o melhor modelo foi: modelo7!

resettest(modelo5)
bptest(modelo5)
jarque.bera.test(modelo5$resid)

resettest(modelo7)
bptest(modelo7)
jarque.bera.test(modelo7$resid)

#Anova
anova(modelo7, modelo5)
#Escolha final: ?


# Como há pontos de alavanca, usaremos HC4 para testar a significância dos parâmetros sob heteroscedasticidade
modelo.AIC = lm(logprice~loglotsize + bathrms2 + bathrms34 + stories2 + stories3 + stories4 + driveway + fullbase + gashw + airco + garagepl1 + garagepl2 + prefarea + driveway*prefarea + gashw*garagepl2)
modelo2 = lm(logprice~loglotsize + bathrms2 + bathrms34 + stories2 + stories3 + stories4 + driveway + fullbase + gashw + airco + garagepl1 + garagepl2 + gashw*garagepl2)
coeftest(modelo2, vcov=vcovHC (modelo2, type="HC4"))
modelo3 = lm(logprice~loglotsize + bathrms2 + bathrms34 + stories2 + stories3 + stories4 + driveway + fullbase + airco + garagepl1)
coeftest(modelo3, vcov=vcovHC (modelo3, type="HC4"))






# Normalidade dos residuos do modelo
jarque.bera.test(modelo3$resid)
hist(modelo3$resid, freq=FALSE, col="gray", xlab="Resíduos do modelo", ylab="Densidade", main="")
plot(modelo3$residuals)
lillie.test(modelo3$resid)
qqnorm(modelo3$resid);qqline(modelo3$resid, col=2)


# Teste de homoscedasticidade - teste de Koenker
bptest(modelo0) ##H0: modelo é homoscedástico. Se p-valor<0,05 , rejeito H0##



##### ANÁLISE DE DIAGNÓSTICO #####
# Identificando se há pontos de alavanca
chapeu.AIC=hatvalues(modelo.AIC)
plot(chapeu.AIC, xlab="Índice", main="Pontos de alavanca do modelo AIC")
abline(h=c(1,2)*mean(chapeu.AIC), col=2)
id.AIC=which(chapeu.AIC>2*mean(chapeu.AIC))
text(id.AIC, chapeu.AIC[id.AIC], rownames(dados)[id.AIC], pos=c(1,2,2), col="red")

influence.measures(modelo.AIC) ##Medidas: DFBETA, DFFITS, D. de Cook e cov.ratio. Pontos de influência:2, 24 e 48##
summary(influence.measures(modelo0))


# Teste de presença de outliers
outlier.test(modelo0) #H0: não há outliers. Se p-valor<0,05, rejeito H0


##### FUNÇÃO DIAG #####
fit.model = modelo0
lms = summary(fit.model)
X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
H <- X%*%solve(t(X)%*%X)%*%t(X)
h <- diag(H)
lms <- summary(fit.model)
s <- lms$sigma
r <- resid(lms)
ts <- r/(s*sqrt(1-h))
di <- (1/p)*(h/(1-h))*(ts^2)
si <- lm.influence(fit.model)$sigma
tsi <- r/(si*sqrt(1-h))
a <- max(tsi)
b <- min(tsi)
par(mfrow=c(2,2))
#PONTOS DE ALAVANCA
plot(fitted(fit.model),h,xlab="Indice", ylab="Leverage", pch=16,
main="Pontos de Alavanca", ylim=c(0,1))
cut <- 3*p/n
abline(cut,0,lty=2)
identify(fitted(fit.model),h, n=2)
#PONTOS INFLUENTES
plot(di,xlab="Indice", ylab="Distancia de Cook", pch=16,
main="Pontos Influentes", ylim=c(0,1))
identify(di, n=2)
#PONTOS ABERRANTES
plot(tsi,xlab="Indice", ylab="Resíduo Studentizado",
ylim=c(b-1,a+1), pch=16, main="Pontos Aberrantes")
abline(2,0,lty=2)
abline(-2,0,lty=2)
identify(tsi, n=3)
par(mfrow=c(1,1))
#Homocedasticidade
plot(fitted(fit.model),tsi,xlab="Valores Ajustados", 
ylab="Residuo Studentizado", ylim=c(b-1,a+1), pch=16,
main="Homocedasticidade")
abline(2,0,lty=2)
abline(-2,0,lty=2)
identify(fitted(fit.model),tsi, n=3)

##### ENVELOPE DA NORMAL #####
par(mfrow=c(1,1))
X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
H <- X%*%solve(t(X)%*%X)%*%t(X)
h <- diag(H)
si <- lm.influence(fit.model)$sigma
r <- resid(fit.model)
tsi <- r/(si*sqrt(1-h))
#
ident <- diag(n)
epsilon <- matrix(0,n,100)
e <- matrix(0,n,100)
e1 <- numeric(n)
e2 <- numeric(n)
#
for(i in 1:100){
     epsilon[,i] <- rnorm(n,0,1)
     e[,i] <- (ident - H)%*%epsilon[,i]
     u <- diag(ident - H)
     e[,i] <- e[,i]/sqrt(u)
     e[,i] <- sort(e[,i]) }
#
for(i in 1:n){
     eo <- sort(e[i,])
     e1[i] <- (eo[2]+eo[3])/2
     e2[i] <- (eo[97]+eo[98])/2 }
#
med <- apply(e,1,mean)
faixa <- range(tsi,e1,e2)
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