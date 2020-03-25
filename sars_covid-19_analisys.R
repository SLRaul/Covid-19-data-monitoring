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

new_cases_date <- mdy(new_cases_date)

new_cases_france <- new_cases %>% select(-Lat, -Long) %>%  filter(`Country/Region` == "France")
France_all <-  as.numeric(apply(new_cases_france[,c(-1,-2)], 2, sum))

Brazil <- as.numeric(new_cases %>% filter(`Country/Region` == 'Brazil') %>% select(-`Province/State`, -`Country/Region`,-Lat, -Long))

Italy <- as.numeric(new_cases %>% filter(`Country/Region` == 'Italy') %>% select(-`Province/State`, -`Country/Region`,-Lat, -Long))

Korea <- as.numeric(new_cases %>% filter(`Country/Region` == 'Korea, South') %>% select(-`Province/State`, -`Country/Region`,-Lat, -Long))

Germany <- as.numeric(new_cases %>% filter(`Country/Region` == 'Germany') %>% select(-`Province/State`, -`Country/Region`,-Lat, -Long))

US <- as.numeric(new_cases %>% filter(`Country/Region` == 'US') %>% select(-`Province/State`, -`Country/Region`,-Lat, -Long))


#utliizando ggplot
dados <- as.data.frame(cbind(c(France_all, Italy, Brazil, Korea, Germany, US),
               (c(as.character(new_cases_date),
                         as.character(new_cases_date),
                          as.character(new_cases_date),
                  as.character(new_cases_date),
                  as.character(new_cases_date),
                         as.character(new_cases_date))),
               rep(c("France", "Italy", "Brazil", "Korea","Germany", "US"),each= length(Brazil))))
colnames(dados)<- c("Cases", "Date", "Country")
dados$Cases <- c(France_all, Italy, Brazil, Korea, Germany, US)

dados %>% ggplot( aes(x=Date, y=Cases, color = Country, group= Country)) + geom_point() +geom_line() +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))



## tabela comparando a partir do primeiro dia de infecção
dados <- dados %>% filter(Cases >0)

tabela <- dados %>% spread(Country, Cases) 

##################### Beging from the fist infecion detect ###########################
##################### dados a partir do primeiro dia ###########################
br <- na.omit(tabela$Brazil)
fr <- na.omit(tabela$France)
it <- na.omit(tabela$Italy)
ko <- na.omit(tabela$Korea)
ger <- na.omit(tabela$Germany)
us <- na.omit(tabela$US)

plot(fr, type = "lines", col = "blue", xlab = "Dias a partir do primeiro dia de infecção", ylab = "Casos registrados")
lines(br, col = "Black", lty = 3)
lines(it, col = "darkgreen", lty = 2)
lines(ko,col= "orange", lty=4)
lines(ger, col="red", lty=5)
legend("topleft",legend=c("França","Itália","Brasil", "Coreia sul", "Alemanha"), col=c("blue","darkgreen","black", "orange", "red"),
       lty=c(1,2,3,4, 5), ncol=1)

dados_ <- as.data.frame(cbind(c(fr, it, br, ko, ger, us),
                             (c(1:length(fr),1:length(it),1:length(br),1:length(ko),1:length(ger),1:length(us))),
                             
                             rep(c("France", "Italy", "Brazil", "Korea", "Germany", "US"),
                                 c(length(fr),length(it),length(br),length(ko),length(ger), length(us)))
                             
                             ))

colnames(dados_)<- c("Cases", "Day", "Country")
dados_$Cases <-c(fr,it,br,ko, ger, us)
dados_$Day <- c(1:length(fr),1:length(it),1:length(br),1:length(ko), 1:length(ger), 1:length(us))

dados_ %>% ggplot(aes(x=Day, y=Cases, group= Country, color= Country)) +
  geom_point() + geom_line()


########### por meio de ts #########
library(forecast)
#transformando em series temporais
teste <- ts(Brazil, start = c(2020,01,2), frequency = 365.25)
#plot(teste)
teste %>% forecast::autoplot() + autolayer(meanf(Brazil, h=length(new_cases_date)),
          series="Mean", PI=FALSE) +
  autolayer(rwf(Brazil, h=length(new_cases_date)),
            series="Naïve", PI=FALSE) +
  autolayer(rwf(Brazil, h=length(new_cases_date), drift=TRUE, ),
            series="Drift", PI=FALSE)

# fonte;
## https://towardsdatascience.com/the-impact-of-covid-19-data-analysis-and-visualization-560e54262dc



rm(list=ls())

#change the directory the code
setwd("/home/silva/R_Diretorio/sars-covid-19")


library(data.table)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(forecast)
library(stringr)

# extract the data from the repository from the computer
new_cases <- fread("~/Repositorios/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
                   header = T)
new_cases_date <- fread("~/Repositorios/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
                        header = F)[1,c(-1,-2,-3,-4)]  

new_cases_date <- mdy(new_cases_date)

#### choose the coutrys ###


France_all <-  as.numeric(apply(
  (new_cases %>% select(-`Province/State`,-Lat, -Long) %>% filter(str_detect(`Country/Region`, "France")))[,c(-1)], 2, sum))

Brazil <- as.numeric(new_cases %>% filter(str_detect(`Country/Region`, 'Brazil')) %>% select(-`Province/State`, -`Country/Region`,-Lat, -Long))

Italy <- as.numeric(new_cases %>% filter(str_detect(`Country/Region`, 'Italy')) %>% select(-`Province/State`, -`Country/Region`,-Lat, -Long))

Korea <- as.numeric(new_cases %>% filter(str_detect(`Country/Region`, 'Korea, South')) %>% select(-`Province/State`, -`Country/Region`,-Lat, -Long))

Germany <- as.numeric(new_cases %>% filter(str_detect(`Country/Region`, 'Germany')) %>% select(-`Province/State`, -`Country/Region`,-Lat, -Long))

US <- as.numeric(new_cases %>% filter(str_detect(`Country/Region`, 'US')) %>% select(-`Province/State`, -`Country/Region`,-Lat, -Long))
###

### tranforming in time series

France <- ts(France_all, start = c(2020,01,2), frequency = 365.25)
Brazil <- ts(Brazil, start = c(2020,01,2), frequency = 365.25)
Italy <- ts(Italy, start = c(2020,01,2), frequency = 365.25)
Korea <- ts(Korea, start = c(2020,01,2), frequency = 365.25)
Germany <- ts(Germany, start = c(2020,01,2), frequency = 365.25)
US <- ts(US, start = c(2020,01,2), frequency = 365.25)

data_ts <- ts.union(France, Brazil, Italy, Korea, Germany, US)
data_ts <- ts.union(Italy, US)
### ploting the data
data_ts %>% forecast::autoplot(facets=F)  + geom_point()+ theme_classic() +
  ylab("Confirmed cases") + ggtitle("Confimerd numbers since 01/22/2020")

###


# fonte;
## https://towardsdatascience.com/the-impact-of-covid-19-data-analysis-and-visualization-560e54262dc