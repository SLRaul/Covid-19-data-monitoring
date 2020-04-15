#
rm(list=ls())

#change the directory the code
setwd("/home/silva/R_Diretorio/sars-covid-19")


library(data.table)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)

# extract the data from the repository from the computer
new_cases <- fread("~/Repositorios/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
                   header = T)
new_cases_date <- fread("~/Repositorios/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
                        header = F)[1,c(-1,-2,-3,-4)]  

cases_date <- mdy(new_cases_date)

Brazil <- as.numeric(new_cases %>% filter(`Country/Region` == 'Brazil') %>% select(-`Province/State`, -`Country/Region`,-Lat, -Long))

Brazil <- Brazil[Brazil > 0]
########## previsão #########
#rm(list = ls())
library (deSolve)

sis_model = function (current_timepoint, state_values, parameters)
{
  # create state variables (local variables)
  S = state_values [1]        # susceptibles
  I = state_values [2]        # infectious
  
  with ( 
    as.list (parameters),     # variable names within parameters can be used 
    {
      # compute derivatives
      dS = (-beta * S * I) + (gamma * I)
      dI = ( beta * S * I) - (gamma * I)
      
      # combine results
      results = c (dS, dI)
      list (results)
    }
  )
}

#parametros
contact_rate = 8                    # number of contacts per day
transmission_probability = 0.07       # transmission probability
infectious_period = 5.55                 # infectious period

#computando os valores de tranmisção e de recuperação
beta_value = 2.4/5.55#contact_rate * transmission_probability
gamma_value = ((2.4/5.55)/2.4)#1 / infectious_period

#numero reprodutivo
Ro = beta_value / gamma_value

#parametros de dinamica da doença
parameter_list = c (beta = beta_value, gamma = gamma_value)

#valores iniciais das sub pop
X = 210000000      # susceptible hosts
Y = 1           # infectious hosts

#pop total
N = X + Y 

#valores inicial da eq diferenci
initial_values = c (S = X/N, I = Y/N)

#chamando os dias-ponto
timepoints = seq (0, 100, by=1)

#simulando um epidemia sis
output = lsoda (initial_values, timepoints, sis_model, parameter_list)

###### plot
# susceptible hosts over time
plot (I ~ time, data = output, type='l', ylim = c(0,1),
      #,xlim=c(0,50),
      col = 'red', ylab = 'S, I, S', main = 'SIS epidemic') 

# remain on same frame
par (new = TRUE)    

# infectious hosts over time
#plot (S ~ time, data = output, type='b', ylim = c(0,1), col = 'blue', ylab = '', axes = FALSE) 

lines(Brazil/210000000)
# http://desolve.r-forge.r-project.org/
# https://rpubs.com/choisy/sir

## https://www.imperial.ac.uk/media/imperial-college/medicine/sph/ide/gida-fellowships/Imperial-College-COVID19-NPI-modelling-16-03-2020.pdf
## https://www.imperial.ac.uk/mrc-global-infectious-disease-analysis/news--wuhan-coronavirus/
## https://www.imperial.ac.uk/media/imperial-college/medicine/sph/ide/gida-fellowships/Imperial-College-COVID19-Global-Impact-26-03-2020.pdf

## https://www.r-bloggers.com/sir-model-with-desolve-ggplot2/
# https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0185528

## https://rpubs.com/docblount/111138 #SIS

## https://www.lewuathe.com/covid-19-dynamics-with-sir-model.html