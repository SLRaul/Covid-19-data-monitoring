rm(list=ls())

#change the directory the code
setwd("/home/silva/R_Diretorio/sars-covid-19")


library(data.table)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)

###############################################################################
############################# For new cases ################################
###############################################################################

## extract the data from the repository from the computer
new_cases <- fread("~/Repositorios/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
                   header = T)
cases_date <- fread("~/Repositorios/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
                        header = F)[1,c(-1,-2,-3,-4)]  

## ajusting the data
cases_date <- mdy(cases_date)

## select the countrys
France_all <- as.numeric(new_cases %>% select(-Lat, -Long) %>%  filter(`Country/Region` == "France") %>%
  select(-`Province/State`, -`Country/Region`) %>% apply( 2, sum) )

Brazil <- as.numeric(new_cases %>% filter(`Country/Region` == 'Brazil') %>% select(-`Province/State`, -`Country/Region`,-Lat, -Long))

Italy <- as.numeric(new_cases %>% filter(`Country/Region` == 'Italy') %>% select(-`Province/State`, -`Country/Region`,-Lat, -Long))

Korea <- as.numeric(new_cases %>% filter(`Country/Region` == 'Korea, South') %>% select(-`Province/State`, -`Country/Region`,-Lat, -Long))

Germany <- as.numeric(new_cases %>% filter(`Country/Region` == 'Germany') %>% select(-`Province/State`, -`Country/Region`,-Lat, -Long))

US <- as.numeric(new_cases %>% filter(`Country/Region` == 'US') %>% select(-`Province/State`, -`Country/Region`,-Lat, -Long))


## ajusting the data for plotting
dados <- as.data.frame(cbind(c(France_all, Italy, Brazil, Korea, Germany, US),
               (c(as.character(cases_date),
                         as.character(cases_date),
                          as.character(cases_date),
                  as.character(cases_date),
                  as.character(cases_date),
                         as.character(cases_date))),
               rep(c("France", "Italy", "Brazil", "Korea","Germany", "US"),each= length(Brazil))))
colnames(dados)<- c("Cases", "Date", "Country")
dados$Cases <- c(France_all, Italy, Brazil, Korea, Germany, US)

## the plot
dados %>% ggplot( aes(x=Date, y=Cases, color = Country, group= Country)) + geom_point() +geom_line() +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+ 
  ggtitle("New cases from 01/22/2020")


##################### Beging from the fist infecion detect ###########################
######################## dados a partir do primeiro dia ##############################

## ajusting the data for a plot
dados <- dados %>% filter(Cases >0)

tabela <- dados %>% spread(Country, Cases) 

br <- na.omit(tabela$Brazil)
fr <- na.omit(tabela$France)
it <- na.omit(tabela$Italy)
ko <- na.omit(tabela$Korea)
ger <- na.omit(tabela$Germany)
us <- na.omit(tabela$US)

dados_ <- as.data.frame(cbind(c(fr, it, br, ko, ger, us),
                             (c(1:length(fr),1:length(it),1:length(br),1:length(ko),1:length(ger),1:length(us))),
                             
                             rep(c("France", "Italy", "Brazil", "Korea", "Germany", "US"),
                                 c(length(fr),length(it),length(br),length(ko),length(ger), length(us)))
                             
                             ))

colnames(dados_)<- c("Cases", "Day", "Country")
dados_$Cases <-c(fr,it,br,ko, ger, us)
dados_$Day <- c(1:length(fr),1:length(it),1:length(br),1:length(ko), 1:length(ger), 1:length(us))

dados_ %>% ggplot(aes(x=Day, y=Cases, group= Country, color= Country)) +
  geom_point() + geom_line() + theme_bw()+ 
  ggtitle("New cases from the fist confirmed")

###############################################################################
############################# For deaths cases ################################
###############################################################################

death_cases <-  fread("~/Repositorios/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",
                     header = T)

## select the countrys
France_all <- as.numeric(death_cases %>% select(-Lat, -Long) %>%  filter(`Country/Region` == "France") %>%
                           select(-`Province/State`, -`Country/Region`) %>% apply( 2, sum) )

Brazil <- as.numeric(death_cases %>% filter(`Country/Region` == 'Brazil') %>% select(-`Province/State`, -`Country/Region`,-Lat, -Long))

Italy <- as.numeric(death_cases %>% filter(`Country/Region` == 'Italy') %>% select(-`Province/State`, -`Country/Region`,-Lat, -Long))

Korea <- as.numeric(death_cases %>% filter(`Country/Region` == 'Korea, South') %>% select(-`Province/State`, -`Country/Region`,-Lat, -Long))

Germany <- as.numeric(death_cases %>% filter(`Country/Region` == 'Germany') %>% select(-`Province/State`, -`Country/Region`,-Lat, -Long))

US <- as.numeric(death_cases %>% filter(`Country/Region` == 'US') %>% select(-`Province/State`, -`Country/Region`,-Lat, -Long))


## ajusting the data for plotting
dados <- as.data.frame(cbind(c(France_all, Italy, Brazil, Korea, Germany, US),
                             (c(as.character(cases_date),
                                as.character(cases_date),
                                as.character(cases_date),
                                as.character(cases_date),
                                as.character(cases_date),
                                as.character(cases_date))),
                             rep(c("France", "Italy", "Brazil", "Korea","Germany", "US"),each= length(Brazil))))
colnames(dados)<- c("Cases", "Date", "Country")
dados$Cases <- c(France_all, Italy, Brazil, Korea, Germany, US)

## the plot
dados %>% ggplot( aes(x=Date, y=Cases, color = Country, group= Country)) + geom_point() +geom_line() +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+ 
  ggtitle("Dead cases from 01/22/2020")


##################### Beging from the fist infecion detect ###########################
######################## dados a partir do primeiro dia ##############################

## ajusting the data for a plot
dados <- dados %>% filter(Cases >0)

tabela <- dados %>% spread(Country, Cases) 

br <- na.omit(tabela$Brazil)
fr <- na.omit(tabela$France)
it <- na.omit(tabela$Italy)
ko <- na.omit(tabela$Korea)
ger <- na.omit(tabela$Germany)
us <- na.omit(tabela$US)

dados_ <- as.data.frame(cbind(c(fr, it, br, ko, ger, us),
                              (c(1:length(fr),1:length(it),1:length(br),1:length(ko),1:length(ger),1:length(us))),
                              
                              rep(c("France", "Italy", "Brazil", "Korea", "Germany", "US"),
                                  c(length(fr),length(it),length(br),length(ko),length(ger), length(us)))
                              
))

colnames(dados_)<- c("Cases", "Day", "Country")
dados_$Cases <-c(fr,it,br,ko, ger, us)
dados_$Day <- c(1:length(fr),1:length(it),1:length(br),1:length(ko), 1:length(ger), 1:length(us))

dados_ %>% ggplot(aes(x=Day, y=Cases, group= Country, color= Country)) +
  geom_point() + geom_line() + theme_bw()+ 
  ggtitle("Dead cases from the fist death")



# fonte;
## https://towardsdatascience.com/the-impact-of-covid-19-data-analysis-and-visualization-560e54262dc