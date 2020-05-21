library(tidyverse)

# drewiane literki po 100
# silikonowe literki po 30
# brak polskich znaków - letters
# częstość imion w zależności od liczby paczek
# sposoby doboru literek - losowy, losowy z prawd, proporcjonalnie
# według płci

load("imiona.RData")

# analiza częstości

top100 <- top_n(imiona, 100, liczba) %>%
  mutate(p=liczba/sum(liczba))

# długość imienia

dl <- top100 %>%
  mutate(dl=nchar(imie)) %>%
  summarise(min=min(dl),
            max=max(dl))

litery <- as.data.frame(str_split_fixed(tolower(top100$imie), "", dl$max))
names(litery) <- paste0("lit",seq(1:dl$max))

im_litery <- cbind(top100, litery)

im_litery_w <- splitstackshape::expandRows(im_litery, "liczba")

# wybór n literek

liczba_liter <- seq(from = 30, by = 30, length.out = 10)

liczba_imion_df <- data.frame(l_lit=numeric(), l_imion=numeric())

for(j in liczba_liter){
  
  #j <- 1
  
  # n symulacji
  
  liczba_imion <- numeric()
  imiona_wszystkie <- character()
  
  cz_liter_w <- im_litery_w %>%
    select(starts_with("lit")) %>%
    gather(nr, litera) %>%
    filter(litera!="") %>%
    count(litera) %>%
    mutate(p=n/sum(n),
           proc=round(p*j))
  
  for(i in 1:100){
    
    l <- select(cz_liter_w, litera, proc)
    
    # losowanie imienia
    
    wynik_litery <- numeric()
    wynik_imiona <- character()
    
    # pętla nr 1 - układanie imion z n literek
    
    # while
    
    suma_50 <- 1
    
    while(suma_50!=0){
      
      s_imie <- tolower(sample(x = im_litery$imie, size = 1, replace = F, prob = im_litery$p))
      s_imie_split <- unlist(str_split(s_imie, pattern = ""))
      
      cz_imie_s <- as.data.frame(table(s_imie_split))
      names(cz_imie_s) <- c("litera", "cz")
      
      wynik1 <- ifelse(mean(s_imie_split %in% l$litera)==1,1,0)
      
      if(wynik1==1){
        
        l_w <- left_join(l, cz_imie_s, by="litera") %>%
          mutate(cz=ifelse(is.na(cz),0,cz),
                 proc=proc-cz) %>%
          select(-cz)
        
        wynik2 <- ifelse(sum(l_w$proc < 0)==0,1,0)
        
        wynik <- ifelse(sum(wynik1,wynik2)==2,1,0)
        
        wynik_litery <- c(wynik_litery, wynik)
        
        if(wynik==1){
          
          l <- left_join(l, cz_imie_s, by="litera") %>%
            mutate(cz=ifelse(is.na(cz),0,cz),
                   proc=proc-cz) %>%
            select(-cz) %>%
            filter(proc!=0)
          
          pozostale_lit <- sum(l$proc)
          wynik_imiona <- c(wynik_imiona, s_imie)
        }
        
      } else {
        wynik_litery <- c(wynik_litery, wynik1)
      }
      
      if(length(wynik_litery)>50){
        suma_50 <- sum(tail(wynik_litery, n = 20))
      }
      
    }
    
    liczba_imion <- c(liczba_imion, length(wynik_imiona))
    imiona_wszystkie <- c(imiona_wszystkie, wynik_imiona)
    
  }
  
  liczba_imion_df_j <- data.frame(l_lit=j, l_imion=liczba_imion)
  liczba_imion_df <- rbind(liczba_imion_df, liczba_imion_df_j)
  
}

liczba_imion_df %>%
  ggplot(., aes(x=as.factor(l_lit), y=l_imion)) +
  geom_boxplot()




