library (car)

mov <- read.csv(file='movie_final.csv', sep=',')
attach(mov)


#####
# Come si evince dai grafici, la tendenza tra budget e i regressori presi in considerazione non è lineare.
# La conseguenza di tale risultato ha spinto alla realizzazione di diverse trasformazioni non lineari 
# dei nostri predittori: trasformazione quadratica, cubica e di ordine 4. 
# Per ognuna, i risultati prodotti sono stati valutati e confrontati tra di loro, per 
# capire quale modello interpretasse nel miglior modo possibile i dati.
#####


### Modello lineare con tutte le variabili
fit <- lm(linear_model, data = mov)

summary(fit)
vif(fit)

## N.B. Valore piccolo del p-value significa che c'è associazione tra predittore e risposta

## R2 statistic che è vicino a 1 indica che una larga proporzione della variabilità nella risposta
## è spiegata dalla regressione

## Valori elevati del VIF indicano presenza di colinearità


# Mostra l'intervallo di confidenza delle variabili
confint(fit)

dev.new()
par(mfrow=c(2,2))
plot(fit)

### Modello lineare con senza variabili con VIF alto
fit_b <- lm(linear_model_b, data = mov)

summary(fit_b)
vif(fit_b)

dev.new()
par(mfrow=c(2,2))
plot(fit_b)

anova(fit,fit_b)
# dal valore dell RSS si deduce che è migliore il modello con tutti i predittori



### Modello polinomiale 2o grado non lineare
fit2 <- lm(poly2_model, data = mov)

summary(fit2)
vif(fit2)

confint(fit2)

dev.new()
par(mfrow=c(2,2))
plot(fit2)

# Compara i modelli
anova(fit,fit2)



#### Modello polinomiale 3o grado non lineare
fit3 <- lm(poly3_model, data = mov)

summary(fit3)
vif(fit3)

confint(fit3)

dev.new()
par(mfrow=c(2,2))
plot(fit3)

anova(fit2,fit3)



#### Modello polinomiale 4o grado non lineare
fit4 <- lm(poly4_model, data = mov)

summary(fit4)
vif(fit4)

confint(fit4)

dev.new()
par(mfrow=c(2,2))
plot(fit4)

anova(fit3,fit4)

# La trasformazione migliore è quella polinomiale di 4o grado


