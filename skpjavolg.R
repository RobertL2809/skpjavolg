library(dplyr)
library(ggplot2)
library(knitr)
library(dplyr)


#Ülesanne reprodutseerida tulemusi. "Growth in a time of debt" artikli põhjal

# RR ja HAP tulemuste reprodutseerimine
skp_andmed <- read.csv("skp_ja_volg.csv")

skp_andmed$grupp <- cut(skp_andmed$vola_skp_suhe, breaks = c(-Inf, 30, 60, 90, Inf), labels = c("<30%","30-60%","60-90%",">90%"))



#sum(is.na(skp_andmed$vola_skp_suhe))

HAP <- skp_andmed %>% 
  group_by(grupp) %>% 
  summarise(
    HAP_mean = round(mean(skp_kasv, na.rm= TRUE),1),
    HAP_median = round(median(skp_kasv, na.rm= TRUE),1)
  ) %>% 
  filter(!is.na(grupp))



RR <- skp_andmed %>% 
  filter(exceli_viga ==0 & valikuline ==0) %>% 
  group_by(grupp, riik) %>% 
  summarise(
    RR_mean = mean(skp_kasv, na.rm=T),
    RR_median = median(skp_kasv, na.rm = T)
  ) %>% 
  mutate(RR_mean = ifelse(grupp== ">90%" & riik == "New Zealand", -7.9, RR_mean)) %>% 
  group_by(grupp) %>% 
  summarise(
    RR_mean = round(mean(RR_mean, na.rm=T),1),
    RR_median = round(median(RR_median, na.rm = T),1)
  ) %>% filter(!is.na(grupp))

tulemused <- inner_join(RR, HAP, by = "grupp")
kable(tulemused)

#Visualiseerin võlakoorma muutumist ajas
skp_andmed %>% 
  ggplot(aes(x = aasta, y = vola_skp_suhe)) + geom_line(color = "darkgreen") + facet_wrap(~ riik) + theme_minimal() + labs(title = "Võlakoorma muutus aastate lõikes",x = "Aasta",y = "Võla ja SKP suhe (%)")

#Millised vaatlused jäid välja
skp_andmed %>% 
  mutate(valja_jaetud = exceli_viga == 1 | valikuline == 1) %>% 
  ggplot(aes(x = aasta, y = riik, colour = valja_jaetud)) + geom_point() + labs(title = "RR analüüsist välja jäänud vaatlused", x = "Aasta", y = "Riik",colour = "Vaatluse staatus")

#Kas 90% piir on maagiline või ei?
#uus grupp
skp_andmed <- skp_andmed %>% 
  mutate(grupp_uus = cut(vola_skp_suhe, breaks = c(-Inf, 30, 60, 90, 120, Inf), labels = c("<30%", "30-60%", "60-90%", "90-120%", ">120%")))

# Arvutame RR meetodil keskmised ja mediaanid, seekord KÕIKI andmeid kaasates
RR_uus <- skp_andmed %>% 
  filter(!is.na(grupp_uus)) %>% 
  group_by(grupp_uus, riik) %>% 
  summarise(riigi_keskmine = mean(skp_kasv, na.rm = TRUE),riigi_mediaan = median(skp_kasv, na.rm = TRUE), .groups = "drop") %>% #võtan pärast rühmitamise ära
  group_by(grupp_uus) %>%
  summarise(RR_mean = round(mean(riigi_keskmine, na.rm = TRUE), 1),RR_median = round(median(riigi_mediaan, na.rm = TRUE), 1))

kable(RR_uus)

#Kas seos on juhuslik või mitte

baasjoonis <- ggplot(skp_andmed, aes(x = vola_skp_suhe, y = skp_kasv)) +
  geom_point(alpha = 0.4, size = 1)

for(i in 1:100){
  #ajame andmed segi
  skp_andmedi <- skp_andmed %>% mutate(skp_kasv = sample(skp_kasv))
  #lisada stat smooth joon baasjoonisele
  baasjoonis <- baasjoonis + stat_smooth(data = skp_andmedi, mapping = aes(x = vola_skp_suhe, y = skp_kasv), se = FALSE, color = "darkgray", linewidth = 0.3)
}

baasjoonis <- baasjoonis + stat_smooth(data = skp_andmed, mapping = aes(x = vola_skp_suhe, y = skp_kasv), color = "blue",linewidth = 1.2, se = FALSE) + theme_minimal() + labs(x = "Võla ja SKP suhe", y = "SKP kasv", title = "Permutaatsioonitesti graafik") + coord_cartesian(ylim = c(-15, 25))

baasjoonis
