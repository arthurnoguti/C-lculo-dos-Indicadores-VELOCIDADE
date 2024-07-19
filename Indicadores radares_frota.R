library(tidyverse)
library(readxl)
library(fleetbr)

# Importando os dados dos radares no PC para manter privacidade 
radares <- read_xlsx("C:\\Users\\UFPR\\Downloads\\INDICADORES_RADARES_VELOCIDADE_UF (1).xlsx", sheet = 2)

frota_2023_TOTAL <- fleetbr %>% 
  filter(ano == "2023",
         mes == "12",
         modal == "TOTAL")

frota_2023_TOTAL_renomeada <- rename(frota_2023_TOTAL, SiglaUf = uf)
#calculo dos indicadores
# i1 (aprovados+reparados)/frota ####

aprov_repari1 <- radares %>% 
  filter(UltimoResultado != "Reprovado") %>% 
  group_by(SiglaUf) %>% 
  summarise(quantidade = n())

i1 <- right_join(frota_2023_TOTAL_renomeada, aprov_repari1, by = "SiglaUf")
i1 <- i1 %>% 
  mutate(i1 = (quantidade/frota)*10^4)
# i2 (urb aprov + urb repar / frota) ####

aprov_repari2 <- radares %>% 
  filter(UltimoResultado != "Reprovado",
         Tipo == "Via Urbana") %>% 
  group_by(SiglaUf) %>% 
  summarise(quantidade = n())

i2 <- right_join(frota_2023_TOTAL_renomeada, aprov_repari2, by = "SiglaUf")
i2 <- i2 %>% 
  mutate(i2 = (quantidade/frota)*10^4)

# i3 (rod aprov + rod repar / frota) ####

aprov_repari3 <- radares %>% 
  filter(UltimoResultado != "Reprovado",
         Tipo == "Rodovia") %>% 
  group_by(SiglaUf) %>% 
  summarise(quantidade = n())


i3 <- right_join(frota_2023_TOTAL_renomeada, aprov_repari3, by = "SiglaUf")
i3 <- i3 %>% 
  mutate(i3 = (quantidade/frota)*10^4)

# i4 (aprovados/(aprovados+reparados)) ####

aprov <- radares %>% 
  filter(UltimoResultado == "Aprovado") %>% 
  group_by(SiglaUf) %>% 
  summarise(quantidade_aprov = n())


i4 <- inner_join(aprov, aprov_repari1, by = "SiglaUf")
i4 <- i4 %>% 
  mutate(i4 = quantidade_aprov/quantidade)

# i5 (aprovados+reparados)/frota com v = 40 km/h ####
aprov_repari5 <- radares %>% 
  filter(UltimoResultado != "Reprovado",
         VelocidadeNominal == "40") %>% 
  group_by(SiglaUf) %>% 
  summarise(quantidade = n())


i5 <- right_join(frota_2023_TOTAL_renomeada, aprov_repari5, by = "SiglaUf")
i5 <- i5 %>% 
  mutate(i5 = (quantidade/frota)*10^4)

# i6 (aprovados+reparados)/frota com v = 50 km/h ####
aprov_repari6 <- radares %>% 
  filter(UltimoResultado != "Reprovado",
         VelocidadeNominal == "50") %>% 
  group_by(SiglaUf) %>% 
  summarise(quantidade = n())


i6 <- right_join(frota_2023_TOTAL_renomeada, aprov_repari6, by = "SiglaUf")
i6 <- i6 %>% 
  mutate(i6 = (quantidade/frota)*10^4)

# i7 (aprovados+reparados)/frota com v = 60 km/h ####
aprov_repari7 <- radares %>% 
  filter(UltimoResultado != "Reprovado",
         VelocidadeNominal == "60") %>% 
  group_by(SiglaUf) %>% 
  summarise(quantidade = n())


i7 <- right_join(frota_2023_TOTAL_renomeada, aprov_repari7, by = "SiglaUf")
i7 <- i7 %>% 
  mutate(i7 = (quantidade/frota)*10^4)

# i8 (aprovados+reparados)/frota com v = 70 km/h ####
aprov_repari8 <- radares %>% 
  filter(UltimoResultado != "Reprovado",
         VelocidadeNominal == "70") %>% 
  group_by(SiglaUf) %>% 
  summarise(quantidade = n())


i8 <- right_join(frota_2023_TOTAL_renomeada, aprov_repari8, by = "SiglaUf")
i8 <- i8 %>% 
  mutate(i8 = (quantidade/frota)*10^4)

# i9 (aprovados+reparados)/frota com v >= 80 km/h ####
aprov_repari9 <- radares %>% 
  filter(UltimoResultado != "Reprovado",
         VelocidadeNominal >= 80) %>% 
  group_by(SiglaUf) %>% 
  summarise(quantidade = n())


i9 <- right_join(frota_2023_TOTAL_renomeada, aprov_repari9, by = "SiglaUf")
i9 <- i9 %>% 
  mutate(i9 = (quantidade/frota)*10^4)

