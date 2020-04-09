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
#definir função
sir_equations <- function(time, variables, parameters) {
  with(as.list(c(variables, parameters)), {
    dS <- -beta * I * S
    dI <-  beta * I * S - gamma * I
    dR <-  gamma * I
    return(list(c(dS, dI, dR)))
  })
}

#definir valores dos parametros
parameters_values <- c(
  beta  =  2.4/5.55, # infectious contact rate (/person/day)
  gamma = (2.4/5.55)/2.4 #rgamma(1,shape = 0.25)   # recovery rate (/day)
)

#defiir valore siiciais para parametros
initial_values <- c(
  S = 310000,  # number of susceptibles at time = 0
  I =   1,  # number of infectious at time = 0
  R =   0   # number of recovered (and immune) at time = 0
)

# perio de tempo de interesse
time_values <- seq(0, 30) # days

# utilizando a função ode para resolver a função
library(deSolve)
sir_values_1 <- ode(
  y = initial_values,
  times = time_values,
  func = sir_equations,
  parms = parameters_values 
)
sir_values_1

sir_values_1 <- as.data.frame(sir_values_1)

with(sir_values_1, {
  # plotting the time series of susceptibles:
  plot(time, S, type = "l", col = "blue",
       xlab = "time (days)", ylab = "number of people")
  # adding the time series of infectious:
  lines(time, I, col = "red")
  # adding the time series of recovered:
  lines(time, R, col = "green")
})

legend("right", c("susceptibles", "infectious", "recovered"),
       col = c("blue", "red", "green"), lty = 1, bty = "n")

# http://desolve.r-forge.r-project.org/
# https://rpubs.com/choisy/sir

## https://www.imperial.ac.uk/media/imperial-college/medicine/sph/ide/gida-fellowships/Imperial-College-COVID19-NPI-modelling-16-03-2020.pdf
## https://www.imperial.ac.uk/mrc-global-infectious-disease-analysis/news--wuhan-coronavirus/
## https://www.imperial.ac.uk/media/imperial-college/medicine/sph/ide/gida-fellowships/Imperial-College-COVID19-Global-Impact-26-03-2020.pdf

## https://www.r-bloggers.com/sir-model-with-desolve-ggplot2/
# https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0185528