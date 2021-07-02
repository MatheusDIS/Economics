######## Teste de Heterocedasticidade e Autocorrelação Serial ###############
####### Autor: Matheus Dias, Economista e Data Scientist ####################
#############################################################################

############### Carregar os packages necessários ###########################

library(foreign)
library(dynlm)
library(sandwich)
library(car)
library(wooldridge)
library(tidyverse)
library(broom)
library(skedastic)
library(lmtest)

###########################################################################
##################### Carregar data set ##################################

gpa3 <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/gpa3.dta") #### Data set do pacote Wooldridge 

########### Estimar modelos de regressão ##################################

reg1 <- lm(cumgpa ~ sat + hsperc + hsrank  + tothrs + female + black + white, data = gpa3, subset = spring ==1)

reg2 <- lm(cumgpa ~ sat  + female + black + white, data = gpa3, subset = spring ==1) #### Modelo sem a variável hsperc, hsrank, tothrs

summary(reg1)
summary(reg2)

############## Especificação do Conjunto de Regressores ##############


######### Akaike´s Information Criteria (AIC) ###################

glance(reg1) %>%
  select(adj.r.squared, sigma, AIC, BIC, p.value)

glance(reg2) %>%
  select(adj.r.squared, sigma, AIC, BIC, p.value)

### AIC, BIC menores são preferíveis
### Com certo parcimônicia, quanto maior o adj.r.squared , melhor é o ajuste do modelo
### Quanto menor o sigma, melhor é o ajuste do modelo

###################################################################
########## Testes de heterocedasticidade #########################

##### Teste BP (Breush-Pagan) ######

breusch_pagan(reg1)

##### Teste White ########

white_lm(reg1)

# A hipótese nula dos testes BP e White é a de existência de homocedasticidade
# Caso haja evidência de heterocedasticidade, ou seja, p.value < 0.05
# Uma forma de inferência é a obtenção de erros-padrão robustos


############ Teste de Autocorrelação ##############################

### Teste de Durbin-Watson

dwtest(reg1)

# a interpretação do output é: para p-value < nível de significância 
# é possível rejeitar a hipótese nula de inexistência de autocorrelação.


############ Obter Erros-Padrão Robusto ###############################

### Estimação da Matriz de Covariância utilizando método de Newey-West ######

coeftest(reg1, vcov = vcovHAC)

