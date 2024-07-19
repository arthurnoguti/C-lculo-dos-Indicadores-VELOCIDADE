library(devtools)
library(tidyverse)
devtools::install_github("pabsantos/roadtrafficdeaths")
devtools::install_github("jotasaraiva/fleetbr")
library(roadtrafficdeaths)
library(fleetbr)

mortes_2022 <- rtdeaths %>% 
  filter(ano_ocorrencia == "2022")

mortes_2022_porUF <- mortes_2022 %>% 
  group_by(nome_uf_ocor) %>% 
  summarise(Quantidade_de_obitos = n())

mortes_2022_porUF <- na.omit(mortes_2022_porUF)

mortes_2022_porUF <- mortes_2022_porUF %>% 
  mutate(uf = c("AC", "AL", "AP", "AM", "BA",
                "CE", "DF", "ES", "GO", "MA",
                "MT", "MS", "MG", "PR", "PB",
                "PA", "PE", "PI", "RN", "RS",
                "RJ", "RO", "RR", "SC", "SE", 
                "SP", "TO"))

frota_2022_TOTAL <- fleetbr %>% 
  filter(ano == "2022",
         mes == "12",
         modal == "TOTAL")

Indicador_mortal <- (full_join(mortes_2022_porUF, frota_2022_TOTAL, by = "uf" ))

Indicador_mortal <- Indice_mortal %>% 
  mutate(Inicador = (Quantidade_de_obitos/frota)*(10^4))
