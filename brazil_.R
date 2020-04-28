#
rm(list=ls())

#change the directory the code
#setwd("/home/silva/R_Diretorio/sars-covid-19")


library(data.table)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)

# extract the data from the repository from the computer
new_cases <- fread("C:/Users/silva/Documents/Repositorio/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
                   header = T)
death_cases <- fread("C:/Users/silva/Documents/Repositorio/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",
                     header = T)

cases_date <- fread("C:/Users/silva/Documents/Repositorio/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
                    header = F)[1,c(-1,-2,-3,-4)]  
cases_date <- mdy(cases_date)


Brazil_cases <- as.numeric(new_cases %>% filter(`Country/Region` == 'Brazil') %>% select(-`Province/State`, -`Country/Region`,-Lat, -Long))

Brazil_death_cases <- as.numeric(death_cases %>% filter(`Country/Region` == 'Brazil') %>% select(-`Province/State`, -`Country/Region`,-Lat, -Long))

Brazil_cases_fd <- Brazil_cases[Brazil_cases > 0]

########## new cases per day ########## 
NCPD <- data.frame(diff(Brazil_cases), (cases_date[-1]))
colnames(NCPD) <- c("New_cases", "Date")

ggplot(NCPD, aes(x = Date, y = New_cases)) + geom_col(col = "white", fill = "darkblue") + 
  ggtitle("New cases per day in Brazil") + ylab("New cases") + theme_bw()

# ggplot(NCPD, aes(x = Date, y = New_cases)) + geom_point() + geom_line()+
#   ggtitle("New cases per day in Brazil") + ylab("New cases") + theme_bw()
#barplot(NCPD$New_cases)
########## new death cases per day ########## 
NDCPD <- data.frame(diff(Brazil_death_cases), (cases_date[-1]))
colnames(NDCPD) <- c("New_death_cases", "Date")

ggplot(NDCPD, aes(x = Date, y = New_death_cases)) + geom_col(col = "white", fill = "black") + 
  ggtitle("New deaths cases per day in Brazil") + ylab("Deaths per day") + theme_bw()

########## previs�o #########
########## prediction #########
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
r_0 <- 2.03 # imperial college r_0 = 2.0 - 2.4 - 2.6
beta_value = r_0/5.55#contact_rate * transmission_probability
gamma_value = ((r_0/5.55)/r_0)#1 / infectious_period

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
#par (new = TRUE)    

# infectious hosts over time
#plot (S ~ time, data = output, type='b', ylim = c(0,1), col = 'blue', ylab = '', axes = FALSE) 

lines(Brazil_cases_fd/210000000)

cbind(output[1:length(Brazil_cases_fd),],Brazil_cases_fd/210000000)

# http://desolve.r-forge.r-project.org/
# https://rpubs.com/choisy/sir

## https://www.imperial.ac.uk/media/imperial-college/medicine/sph/ide/gida-fellowships/Imperial-College-COVID19-NPI-modelling-16-03-2020.pdf
## https://www.imperial.ac.uk/mrc-global-infectious-disease-analysis/news--wuhan-coronavirus/
## https://www.imperial.ac.uk/media/imperial-college/medicine/sph/ide/gida-fellowships/Imperial-College-COVID19-Global-Impact-26-03-2020.pdf

## https://www.r-bloggers.com/sir-model-with-desolve-ggplot2/
# https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0185528

## https://rpubs.com/docblount/111138 #SIS

## https://www.lewuathe.com/covid-19-dynamics-with-sir-model.html