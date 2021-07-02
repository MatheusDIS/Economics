################Brazil GDP Forecast Model ###################################


################ Load the required packages #################################

library(sidrar)
library(RcppRoll)
library(BMR)
library(forecast)
library(urca)
library(stargazer)
library(ggplot2)
library(xtable)

#############################################################################
################### Import GDP data from Sidra - IBGE API #######################

tabela_gdp <- get_sidra(api ="/t/1620/n1/all/v/all/p/all/c11255/90687,90691,90696,90707,93404,93405,93406,93407,93408/d/v583%202")   #### GDP in 1996 prices
tabela_gdp_sa <- get_sidra(api="/t/1621/n1/all/v/all/p/all/c11255/90687,90691,90696,90707,93404,93405,93406,93407,93408/d/v584%202") #### Seasonally adjusted in 1996 prices


###### Cleaning and manipulating the data ###########################

series <- c(90687, 90691, 90696, 90707, 93404, 93405, 93406, 93407, 93408)
names <- c('Agr', 'Ind', 'Serv', 'GDP', 'Consumption', 'Gov', 'FBCF', 'Export', 'Import' )

gdp <- matrix(NA, ncol = length(series), nrow = nrow(tabela_gdp)/length(series))

gdp_sa <- matrix(NA, ncol = length(series), nrow = nrow(tabela_gdp_sa)/length(series))

####### For Loop to insert the data in the GDP matrix ##############

for(i in 1:length(series)) {
  
  gdp[,i] <- tabela_gdp$Valor[tabela_gdp$`Setores e subsetores (Código)` == series[i]]
  
  gdp <- ts(gdp, start = c(1996, 01), freq = 4) 
  colnames(gdp) <- names
  
  
}

####### For Loop to insert the data in the Seasonally Adjusted GDP matrix ##############

for(i in 1:length(series)) {
  
  gdp_sa[,i] <- tabela_gdp_sa$Valor[tabela_gdp_sa$`Setores e subsetores (Código)` == series[i]]
  
  gdp_sa <- ts(gdp_sa, start = c(1996, 01), freq = 4) 
  colnames(gdp_sa) <- names
  
  
}

############# GDP Quarter over Quarter #####################

gdp_qoq <- (gdp_sa/lag(gdp, -1)-1)*100

colnames(gdp_qoq) <- colnames(gdp_sa)

dates <- seq(as.Date('1996-06-01'), as.Date('2021-03-01'), by = '3 month')

############### Graphs ##################################

gtsplot(gdp_qoq, dates = dates, rowdates = TRUE)

############# GDP Year over Year ############################


gdp_yoy <- (gdp/lag(gdp,-4)-1)*100 

colnames(gdp_yoy) <- colnames(gdp)
dates2 <- seq(as.Date('1997-03-01'), as.Date('2021-03-01'), by = '3 month')

################# Graphs #####################################

gtsplot(gdp_yoy, dates = date2, rowdates = TRUE)



############ Cumulative Quarterly GDP ##########################

gdp_annual <- (((gdp + lag(gdp,-1)+lag(gdp,-2)+lag(gdp,-3))/4)/
                   ((lag(gdp,-4)+lag(gdp,-5)+lag(gdp,-6)+lag(gdp,-7))/4)-1)*100


colnames(gdp_annual) <- colnames(gdp)
dates3 <- seq(as.Date('1997-12-01'), as.Date('2021-03-01'), by = '3 month')

################## Graphs #######################

gtsplot(gdp_annual, dates=dates3, rowdates = TRUE)




