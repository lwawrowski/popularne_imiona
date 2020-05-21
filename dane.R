# https://www.gov.pl/cyfryzacja/dodatkowe-materialy

library(tidyverse)
library(openxlsx)

r15m <- read.xlsx("data/imiona_2015r.xlsx", sheet = "m") %>%
  mutate(rok=2015,
         plec="m")

r15k <- read.xlsx("data/imiona_2015r.xlsx", sheet = "k") %>%
  mutate(rok=2015,
         plec="k")

r16m <- read.xlsx("data/imiona_2016r.xlsx", sheet = "m") %>%
  mutate(rok=2016,
         plec="m")

r16k <- read.xlsx("data/imiona_2016r.xlsx", sheet = "k") %>%
  mutate(rok=2016,
         plec="k")

r17m <- read.xlsx("data/imiona_2017r.xlsx", sheet = "m") %>%
  mutate(rok=2017,
         plec="m")

r17k <- read.xlsx("data/imiona_2017r.xlsx", sheet = "k") %>%
  mutate(rok=2017,
         plec="k")

imie_m <- union(r15m, union(r16m, r17m))
imie_k <- union(r15k, union(r16k, r17k))
imiona <- union(imie_m, imie_k)

save(imiona, file = "data/imiona.RData")

top_m <- top_n(r17m, 10, liczba) %>% .$imie
top_k <- top_n(r17k, 10, liczba) %>% .$imie

imiona %>%
  filter(imie %in% c(top_m, top_k)) %>%
  ggplot(., aes(x=imie, y=liczba, fill=as.factor(rok))) + 
  geom_bar(position = position_dodge(), stat = "identity") +
  facet_wrap(~ plec, scales = "free_x", nrow = 2) + 
  theme_light() +
  xlab("Imię") + ylab("Liczba") +
  theme(legend.position = "bottom")