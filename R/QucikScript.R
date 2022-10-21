library(tidyverse)
library(trelliscopejs)
LPD2022_public <- read_csv("LivingPlanetIndex_2022_PublicData/LPD2022_public.csv")
#names(LPD2022_public)

LPDNorge<-LPD2022_public %>% 
  filter(Country=="Norway")

LPDNorge %>% view()

LPDNorge_long<-LPDNorge %>% 
  pivot_longer(!c(1:32),
               names_to="year",
               values_to="value") 

LPDNorge_long %>% 
  filter(Binomial=="Canis_lupus") %>%
  group_by(ID) %>%
  mutate(value=as.numeric(value)) %>%
  mutate(year=as.numeric(year)) %>% 
  drop_na(value) %>% 
  ggplot(aes(year, value, colour=as.factor(ID)))+
  geom_point()+
  geom_line()+
  theme_classic()+
  facet_wrap(~ID, scales="free")




LPDNorge_long %>% 
  group_by(Binomial, ID) %>% 
  mutate(value=as.numeric(value)) %>%
  mutate(year=as.numeric(year)) %>% 
  drop_na(value) %>% 
  ggplot(aes(year, value, colour=as.factor(ID)))+
  geom_point()+
  geom_line()+
  theme_classic()+
  facet_trelliscope(~ Binomial+ID, scales="free", nrow = 2, ncol = 7, width = 300, path = "output")
