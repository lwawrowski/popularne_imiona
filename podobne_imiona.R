# devtools::install_github("mattflor/chorddiag")

library(tidyverse)
library(stringdist)
library(chorddiag)
library(grDevices)
library(RColorBrewer)

kolory <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "Set3"))

load("data/imiona.RData")

imiona %>%
  group_by(plec, rok) %>%
  count() %>%
  ungroup() %>%
  mutate(plec=factor(plec, labels=c("Kobiety", "Mężczyźni")),
         rok=as.factor(rok)) %>%
  ggplot(., aes(x=plec, y=n, fill=rok)) + 
  geom_bar(position = position_dodge(), stat = "identity") +
  theme_light() +
  xlab("Płeć") + ylab("Liczba unikalnych imion") +
  theme(legend.position = "bottom") + 
  guides(fill=guide_legend(title="Rok"))

d <- imiona %>%
  filter(rok == 2015, plec == "m")

podobne_teksty <- function(dane, metoda, granica){
  
  # policzenie odległości dla wszystkich par
  
  m <- as.matrix(stringdistmatrix(a = dane, b = dane, method = metoda))
  
  rownames(m) <- dane
  colnames(m) <- dane
  
  # wybór obserwacji z odległością poniżej określonej granicy
  
  m_g <- as.data.frame(which(m <= granica, arr.ind=TRUE)) %>%
    mutate(tekst1=rownames(.)) %>%
    filter(!row==col) %>%
    mutate(tekst2=colnames(m)[col])
  
  d <- numeric(nrow(m_g))
  
  for(i in 1:length(d)){
    d[i] <- m[m_g$row[i],m_g$col[i]]
  }
  
  m_g$d <- d
  
  # usunięcie duplikatów
  
  m_g_bd <- as.data.frame(unique(t(apply(m_g[,1:2], 1, sort))))
  names(m_g_bd) <- c("row", "col")
  
  m_g_bd_tekst <- inner_join(m_g_bd, m_g, by=c("row"="row", "col"="col"))
  
  # utworzenie macierzy do funkcji chorddiag
  
  # wybór unikalnych numerów kolumn i wierszy
  
  rc <- unique(c(m_g_bd_tekst$row, m_g_bd_tekst$col))
  
  m_g_tekst <- m[rc, rc]
  
  # zamiana odległości większej niż podana granica na 0
  
  m_g_tekst0 <- m_g_tekst
  
  m_g_tekst0[m_g_tekst0 > granica] <- 0
  
  wynik <- list(zbior=m_g_bd_tekst[-c(1,2)],
                macierz=m_g_tekst0)
  
  return(wynik)
  
}

# Damerau-Levenshtein distance

dl <- podobne_teksty(dane = d$imie, metoda = "osa", granica = 2)

dl_df <- dl$zbior
dl_m <- dl$macierz

liczebnosc <- colSums(dl_m != 0)

sort <- order(liczebnosc)

chorddiag(dl_m[sort, sort], 
          margin = 60, 
          palette = "Set3", 
          showTicks = F,
          groupPadding = 2,
          groupnameFontsize = 10,
          groupnamePadding = 5,
          groupThickness = .05,
          showZeroTooltips = F,
          chordedgeColor = "gray90",
          groupColors = kolory(nrow(dl_m)))

# Longest common substring distance

lcs <- podobne_teksty(dane = d$imie, metoda = "lcs", granica = 3)

lcs_df <- lcs$zbior
lcs_m <- lcs$macierz

liczebnosc <- colSums(lcs_m != 0)

tekst_max <- liczebnosc[which(liczebnosc==max(liczebnosc))]

sort <- order(liczebnosc)

chorddiag(lcs_m[sort, sort], 
          margin = 60, 
          palette = "Set3", 
          tickInterval = 10, 
          showTicks = F,
          groupPadding = 1,
          groupnameFontsize = 10,      
          groupnamePadding = 5,
          groupThickness = .05,
          chordedgeColor = "gray90",
          groupColors = kolory(nrow(lcs_m)))

# Jaro distance

jd <- podobne_teksty(dane = d$imie, metoda = "jw", granica = 0.2)

jd_df <- jd$zbior
jd_m <- jd$macierz

liczebnosc <- colSums(jd_m != 0)

tekst_max <- liczebnosc[which(liczebnosc==max(liczebnosc))]

sort <- order(liczebnosc)

chorddiag(jd_m[sort, sort], 
          margin = 60, 
          palette = "Set3", 
          tickInterval = 10, 
          showTicks = F,
          groupPadding = 1,
          groupnameFontsize = 10,       
          groupnamePadding = 5,
          groupThickness = .05,
          chordedgeColor = "gray90",
          groupColors = kolory(nrow(jd_m)))

# rok 2017

d <- imiona %>%
  filter(rok == 2017, plec == "k")

jd <- podobne_teksty(dane = d$imie, metoda = "jw", granica = 0.2)

jd_df <- jd$zbior
jd_m <- jd$macierz

liczebnosc <- colSums(jd_m != 0)

tekst_max <- liczebnosc[which(liczebnosc==max(liczebnosc))]
tekst_max

jd_df %>%
  filter(tekst1 %in% names(tekst_max) | tekst2 %in% names(tekst_max)) %>%
  head(n=12) %>%
  knitr::kable()

sort <- order(liczebnosc)

chorddiag(jd_m[sort, sort],
          margin = 60,
          palette = "Set3",
          tickInterval = 10,
          showTicks = F,
          groupPadding = 1,
          groupnameFontsize = 10,
          groupnamePadding = 5,
          groupThickness = .05,
          chordedgeColor = "gray90",
          groupColors = kolory(nrow(jd_m)))