library(tidyverse)
library(janitor)
library(gridExtra)
library(DescTools)
library(viridis)
library(RColorBrewer)
theme_set(theme_linedraw())



dados <- read_csv('C:/Users/Victor/OneDrive - Insper - Institudo de Ensino e Pesquisa/Estudos/Programação e Data Science/InsperData/processo_seletivo/base_17.csv') %>% 
  clean_names() %>% 
  rename(
    'id_par' = 'halter_id',
    'own_age' = 'alter',
    'sex_own' = 'geschlecht',
    'macro_dist' = 'stadtkreis',
    'dist' = 'stadtquartier',
    'race1' = 'rasse1',
    'race2' = 'rasse2',
    'dog_birth' = 'geburtsjahr_hund',
    'dog_sex' = 'geschlecht_hund',
    'dog_color' = 'hundefarbe'
  ) %>% 
  select(-c(x1, race2)) %>% 
  mutate(dog_sex = ifelse(dog_sex == 'm', 'male', 'female'), 
         sex_own = ifelse(sex_own == 'm', 'man', 'woman'),
         dog_age = 2017-dog_birth,
         dog_age_interval = ifelse(dog_age >= 0 & dog_age <= 5, '0 - 5', 
                                   ifelse(dog_age >=6 & dog_age <= 10, '6 - 10',
                                          ifelse(dog_age >= 11 & dog_age <= 15, '11 - 15',
                                                 ifelse(dog_age >=16 & dog_age <= 21, '16 - 21', '0')))))


quant_race <- as_tibble(dados %>% 
                          group_by(race1) %>% 
                          summarise(quant = n()))
quant_race_age <- as_tibble(dados %>% 
                              group_by(race1,own_age) %>% 
                              summarise(quant = n()))
quant_race_sexown <- as_tibble(dados %>% 
                                 group_by(race1, sex_own) %>% 
                                 summarise(quant = n()))
quant_race_dist <- as_tibble(dados %>% 
                               group_by(race1, dist) %>% 
                               summarise(quant = n()))
quant_race_dogsex <- as_tibble(dados %>% 
                                 group_by(race1, dog_sex) %>% 
                                 summarise(quant = n()))
quant_race_birth <- as_tibble(dados %>% 
                                group_by(race1, dog_birth) %>% 
                                summarise(quant = n()))

quant_sexown_dogsex <- as_tibble(dados %>% group_by(sex_own, dog_sex) %>% summarise(quant = n()))

quant_age_own <- as_tibble(dados %>%  group_by(own_age) %>% summarise(quant = n()))


dados %>% ggplot(aes(sex_own, fill = dog_sex)) + 
  geom_bar(position = 'dodge') + 
  ggtitle('Distribuição do sexo dos cães por sexo dos donos') + 
  xlab('Sexo do dono') + ylab('Frequência') +
  scale_fill_grey(start = 0.8, end = 0.2, name = 'Sexo do cachorro') +
  theme(plot.title = element_text(hjust = 0.5))

dados %>% ggplot(aes(dog_sex, fill = own_age)) + 
  geom_bar(position = 'dodge') + 
  ggtitle('Faixa etária dos donos por sexo do cachorro') + 
  xlab('Sexo do cachorro') + ylab('Frequência') +
  scale_fill_grey(start = 0.8, end = 0.2, name = 'Idade do dono') +
  theme(plot.title = element_text(hjust = 0.5))

diversidade_raca <- dados %>% group_by(own_age, race1) %>% summarise(quant = n()) %>%
  group_by(own_age) %>% summarise(quant = n())

diversidade_raca %>% ggplot(aes(own_age, quant, fill = own_age)) + 
  geom_col(position = 'dodge') + 
  ggtitle('Diversidade de raças ao longo das faixas etárias') + 
  xlab('Faixa etária') + ylab('Frequência de raças') +
  scale_fill_brewer(palette = 'Dark2', guide = F) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x=element_text(angle=90))

dados %>% group_by(dog_age, own_age) %>% 
  summarise(quant = n()) %>%
  ggplot(aes(dog_age, quant, fill = own_age)) + 
  geom_col(position = 'dodge') + 
  ggtitle('Idade dos cães  etárias') + 
  xlab('Idade do cachorro') + ylab('Frequência') +
  scale_fill_brewer(palette = 'Reds', name = 'Idade do dono') +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x=element_text(angle=90))

dados %>% ggplot(aes(own_age)) + 
  geom_bar(position = 'dodge') + 
  ggtitle('Distribuição da idade dos donos') + 
  xlab('Idade do dono') + ylab('Frequência') +
  scale_fill_grey(start = 0.8, end = 0.2, name = 'Idade do dono', guide = F) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x=element_text(angle=90))


dados %>% ggplot(aes(sex_own)) + 
  geom_bar(position = 'dodge') + 
  ggtitle('Distribuição do sexo dos donos') + 
  xlab('Sexo dos donos') + ylab('Frequência') +
  scale_fill_grey(start = 0.8, end = 0.2, name = 'Sexo do dono', guide = F) +
  theme(plot.title = element_text(hjust = 0.5))


dados %>% ggplot(aes(dog_sex)) + 
  geom_bar(position = 'dodge') + 
  ggtitle('Distribuição do sexo do cachorro') + 
  xlab('Sexo do cachorro') + ylab('Frequência') +
  scale_fill_grey(start = 0.8, end = 0.2, name = 'Idade do dono', guide = F) +
  theme(plot.title = element_text(hjust = 0.5))

dados %>% ggplot(aes(dog_age), alpha = .25) + 
  geom_density(position = 'dodge') + 
  ggtitle('Distribuição da idade dos cachorros') + 
  xlab('Idade do cachorro') + ylab('Frequência') +
  scale_colour_grey(start = 0.8, end = 0.2, name = 'Idade do dono', guide = F) +
  theme(plot.title = element_text(hjust = 0.5))


dados %>% ggplot(aes(dog_age_interval, fill = own_age)) + 
  geom_bar(position = 'dodge') + 
  ggtitle('Distribuição da idade dos cachorros por idade do dono') + 
  xlab('Idade do cachorro') + ylab('Frequência') +
  scale_fill_grey(start = 0.8, end = 0.2, name = 'Idade do dono') +
  theme(plot.title = element_text(hjust = 0.5))
