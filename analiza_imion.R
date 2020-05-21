# https://akademiaporodu.pl/top-news/najpopularniejsze-imiona-2016-ranking-imion-2016/

# https://www.gov.pl/cyfryzacja/dane-za-2016-rok

library(tidyverse)
library(rvest)

devtools::install_github("exploratory-io/exploratory_func")
library(exploratory)

zrodlo <- "https://akademiaporodu.pl/top-news/najpopularniejsze-imiona-2016-ranking-imion-2016/"

tmp <- zrodlo %>%
  read_html %>%
  html_nodes("table") %>%
  html_table(fill=T)

imiona_meskie <- rbind(tmp[[1]], tmp[[2]], tmp[[3]], tmp[[4]])
names(imiona_meskie) <- c("imie", "liczba")

imiona_zenskie <- rbind(tmp[[5]], tmp[[6]], tmp[[7]], tmp[[8]])
names(imiona_zenskie) <- c("imie", "liczba")

imiona_meskie$typ <- "męskie"
imiona_zenskie$typ <- "żeńskie"

imiona_zenskie <- filter(imiona_zenskie, !is.na(liczba))

imiona <- rbind(imiona_meskie, imiona_zenskie)

save(imiona, file="imiona.RData")

# wszystkie imiona

max_dl <- imiona %>%
  mutate(dl=nchar(imie)) %>%
  summarise(m=max(dl)) %>%
  as.numeric()

imiona_litery <- as.data.frame(str_split_fixed(tolower(imiona$imie), "", max_dl))
names(imiona_litery) <- paste0("lit",seq(1:max_dl))

imiona_litery <- cbind(imiona, imiona_litery)

czestosc_liter <- imiona_litery %>%
  select(starts_with("lit")) %>%
  gather(nr, litera) %>%
  filter(litera!="") %>%
  count(litera) %>%
  mutate(procent=round(n/sum(n)*100)) %>%
  arrange(desc(n,procent))

imiona_litery_w <- splitstackshape::expandRows(imiona_litery, "liczba")

czestosc_liter_w <- imiona_litery_w %>%
  select(starts_with("lit")) %>%
  gather(nr, litera) %>%
  filter(litera!="") %>%
  count(litera) %>%
  mutate(procent=round(n/sum(n)*100)) %>%
  arrange(desc(n,procent))


# top 50 ------------------------------------------------------------------

imiona_top50 <- top_n(imiona, 100, liczba)

imiona_litery_top50 <- as.data.frame(str_split_fixed(tolower(imiona_top50$imie), "", max_dl))
names(imiona_litery_top50) <- paste0("lit",seq(1:max_dl))

imiona_litery_top50 <- cbind(imiona_top50, imiona_litery_top50)

imiona_litery_top50_w <- splitstackshape::expandRows(imiona_litery_top50, "liczba")

czestosc_liter_top50_w <- imiona_litery_top50_w %>%
  select(starts_with("lit")) %>%
  gather(nr, litera) %>%
  filter(litera!="") %>%
  count(litera) %>%
  mutate(procent=round(n/sum(n)*100)) %>%
  arrange(desc(n,procent))

# top 100 ------------------------------------------------------------------

imiona_top100 <- top_n(imiona, 200, liczba)

imiona_litery_top100 <- as.data.frame(str_split_fixed(tolower(imiona_top100$imie), "", max_dl))
names(imiona_litery_top100) <- paste0("lit",seq(1:max_dl))

imiona_litery_top100 <- cbind(imiona_top100, imiona_litery_top100)

imiona_litery_top100_w <- splitstackshape::expandRows(imiona_litery_top100, "liczba")

czestosc_liter_top100_w <- imiona_litery_top100_w %>%
  select(starts_with("lit")) %>%
  gather(nr, litera) %>%
  filter(litera!="") %>%
  count(litera) %>%
  mutate(procent=round(n/sum(n)*100)) %>%
  arrange(desc(n,procent))


# wg płci -----------------------------------------------------------------


# mężczyźni ---------------------------------------------------------------

imiona_top100_m <- imiona %>%
  filter(typ=="męskie") %>%
  top_n(100, liczba)

imiona_litery_top100_m <- as.data.frame(str_split_fixed(tolower(imiona_top100_m$imie), "", max_dl))
names(imiona_litery_top100_m) <- paste0("lit",seq(1:max_dl))

imiona_litery_top100_m <- cbind(imiona_top100_m, imiona_litery_top100_m)

imiona_litery_top100_m_w <- splitstackshape::expandRows(imiona_litery_top100_m, "liczba")

czestosc_liter_top100_m_w <- imiona_litery_top100_m_w %>%
  select(starts_with("lit")) %>%
  gather(nr, litera) %>%
  filter(litera!="") %>%
  count(litera) %>%
  mutate(procent=round(n/sum(n)*100)) %>%
  arrange(desc(n,procent))

# kobiety ---------------------------------------------------------------

imiona_top100_k <- imiona %>%
  filter(typ=="żeńskie") %>%
  top_n(100, liczba)

imiona_litery_top100_k <- as.data.frame(str_split_fixed(tolower(imiona_top100_k$imie), "", max_dl))
names(imiona_litery_top100_k) <- paste0("lit",seq(1:max_dl))

imiona_litery_top100_k <- cbind(imiona_top100_k, imiona_litery_top100_k)

imiona_litery_top100_k_w <- splitstackshape::expandRows(imiona_litery_top100_k, "liczba")

czestosc_liter_top100_k_w <- imiona_litery_top100_k_w %>%
  select(starts_with("lit")) %>%
  gather(nr, litera) %>%
  filter(litera!="") %>%
  count(litera) %>%
  mutate(procent=round(n/sum(n)*100)) %>%
  arrange(desc(n,procent))


