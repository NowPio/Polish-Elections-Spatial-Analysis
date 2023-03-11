A spatial analysis of the Polish elections in 2019
================
Piotr Nowicki
3/11/23

## Introduction

``` r
libraries <- c("spdep", "sfdep", "rgdal", "maptools", "sp", "sf",
               "ggplot2", "tidyverse", "gridExtra", "spatialreg",
               "vtable", "cartography", "readxl", "texreg")
lapply(libraries, require, character.only = T)

wd <- getwd()
setwd(paste0(wd, "/dane"))

POW <- st_read("powiaty.shp")
POW <- st_transform(POW, 4326)
results <- read.csv("wyniki_powiaty.csv", sep = ";")
results$Kod.TERYT <- as.integer(results$Kod.TERYT/100)
results <- results[results$Powiat != c("zagranica", "statki"), 
                   c(1, 2, 3, 4, 10, 12, 13, 15, 17)]
colnames(results)[5:9] <- c("KO", "Konfederacja", "PSL", "PiS", "Lewica")

for (i in 4:9) {
  results[i] <- as.numeric(stringr::str_replace(results[, i], ',', '.'))
}

POW$jpt_kod_je <- as.integer(POW$jpt_kod_je)
POW <- arrange(POW, jpt_kod_je)
POW$PiS <- results$PiS
POW$KO <- results$KO
POW$PSL <- results$PSL
POW$Lewica <- results$Lewica
POW$Konfederacja <- results$Konfederacja

cont.sf <- poly2nb(POW)
cont.listw <- nb2listw(cont.sf, style = "W")
st.cont <- st_contiguity(POW)
st.weights <- st_weights(st.cont)

results15 <- read_excel("wyniki2015.xls")
results15$TERYT <- as.integer(results15$TERYT)
results15 <- results15[!results15$Powiat %in% 
                         c("Zagranica", "Statki (Gdynia)", "Statki (Gdańsk)"), 
                       c(2, 3, 10:17)]
colnames(results15)[3:10] <- c("PiS", "PO", "Razem", "KORWiN", 
                               "PSL", "ZL", "Kukiz", "N")
results15$KO <- results15$PO + results15$N
results15$Lewica <- results15$ZL + results15$Razem
results15$PSL <- results15$PSL + results15$Kukiz
colnames(results15)[6] <- "Konfederacja" 

POW15 <- st_read("powiaty.shp")
POW15 <- st_transform(POW15, 4326)
POW15$jpt_kod_je <- as.integer(POW15$jpt_kod_je)
POW15 <- arrange(POW15, jpt_kod_je)
POW15 <- merge(POW15, results15, by.x = "jpt_kod_je", by.y = "TERYT", all.x = T)

data <- read_xlsx("zmienne.xlsx")
```

``` r
summary(results[c("PiS", "KO", "Lewica", "PSL", "Konfederacja")])
```

          PiS              KO            Lewica            PSL        
     Min.   :24.11   Min.   : 5.81   Min.   : 3.020   Min.   : 3.210  
     1st Qu.:38.62   1st Qu.:14.45   1st Qu.: 7.897   1st Qu.: 7.008  
     Median :45.69   Median :23.53   Median :11.065   Median :10.035  
     Mean   :47.76   Mean   :23.12   Mean   :11.077   Mean   :10.310  
     3rd Qu.:56.82   3rd Qu.:30.29   3rd Qu.:13.865   3rd Qu.:12.745  
     Max.   :77.19   Max.   :50.11   Max.   :26.800   Max.   :29.130  
      Konfederacja   
     Min.   : 3.870  
     1st Qu.: 5.630  
     Median : 6.395  
     Mean   : 6.476  
     3rd Qu.: 7.152  
     Max.   :14.200  

``` r
summary(results15[c("PiS", "KO", "Lewica", "PSL", "Konfederacja")])
```

          PiS              KO             Lewica            PSL        
     Min.   :17.76   Min.   : 6.997   Min.   : 3.403   Min.   : 5.682  
     1st Qu.:30.22   1st Qu.:19.327   1st Qu.: 8.276   1st Qu.:12.760  
     Median :37.08   Median :29.435   Median :10.768   Median :15.540  
     Mean   :39.13   Mean   :28.224   Mean   :10.779   Mean   :16.452  
     3rd Qu.:46.33   3rd Qu.:36.196   3rd Qu.:13.078   3rd Qu.:19.726  
     Max.   :69.87   Max.   :52.933   Max.   :23.466   Max.   :36.545  
      Konfederacja  
     Min.   :2.701  
     1st Qu.:3.825  
     Median :4.290  
     Mean   :4.390  
     3rd Qu.:4.903  
     Max.   :7.701  

``` r
par(mfrow=c(2,2))
choroLayer(POW, var = "PiS",
              col = carto.pal(pal1 = "blue.pal", n1 = 10))
choroLayer(POW, var = "KO",
              col = carto.pal(pal1 = "sand.pal", n1 = 10))
choroLayer(POW, var = "Lewica",
              col = carto.pal(pal1 = "red.pal", n1 = 10))
choroLayer(POW, var = "PSL",
              col = carto.pal(pal1 = "green.pal", n1 = 10))
```

![](spatial_analysis_files/figure-commonmark/vote%20shares%20for%20every%20political%20party-1.png)

``` r
par(mfrow=c(1,1))
choroLayer(POW, var = "Konfederacja",
              col = carto.pal(pal1 = "grey.pal", n1 = 10))
```

![](spatial_analysis_files/figure-commonmark/vote%20shares%20for%20every%20political%20party-2.png)

``` r
moran.test(POW$PiS, cont.listw, alternative = "two.sided")
```


        Moran I test under randomisation

    data:  POW$PiS  
    weights: cont.listw    

    Moran I statistic standard deviate = 21.759, p-value < 2.2e-16
    alternative hypothesis: two.sided
    sample estimates:
    Moran I statistic       Expectation          Variance 
          0.739112886      -0.002638522       0.001162139 

``` r
moran.test(POW$KO, cont.listw, alternative = "two.sided")
```


        Moran I test under randomisation

    data:  POW$KO  
    weights: cont.listw    

    Moran I statistic standard deviate = 19.567, p-value < 2.2e-16
    alternative hypothesis: two.sided
    sample estimates:
    Moran I statistic       Expectation          Variance 
          0.664331932      -0.002638522       0.001161896 

``` r
moran.test(POW$Lewica, cont.listw, alternative = "two.sided")
```


        Moran I test under randomisation

    data:  POW$Lewica  
    weights: cont.listw    

    Moran I statistic standard deviate = 16.496, p-value < 2.2e-16
    alternative hypothesis: two.sided
    sample estimates:
    Moran I statistic       Expectation          Variance 
          0.558774151      -0.002638522       0.001158252 

``` r
moran.test(POW$PSL, cont.listw, alternative = "two.sided")
```


        Moran I test under randomisation

    data:  POW$PSL  
    weights: cont.listw    

    Moran I statistic standard deviate = 11.732, p-value < 2.2e-16
    alternative hypothesis: two.sided
    sample estimates:
    Moran I statistic       Expectation          Variance 
          0.396488809      -0.002638522       0.001157434 

``` r
moran.test(POW$Konfederacja, cont.listw, alternative = "two.sided")
```


        Moran I test under randomisation

    data:  POW$Konfederacja  
    weights: cont.listw    

    Moran I statistic standard deviate = 10.678, p-value < 2.2e-16
    alternative hypothesis: two.sided
    sample estimates:
    Moran I statistic       Expectation          Variance 
          0.358380454      -0.002638522       0.001143011 

``` r
moran.test(results15$PiS, cont.listw, alternative = "two.sided")
```


        Moran I test under randomisation

    data:  results15$PiS  
    weights: cont.listw    

    Moran I statistic standard deviate = 23.556, p-value < 2.2e-16
    alternative hypothesis: two.sided
    sample estimates:
    Moran I statistic       Expectation          Variance 
          0.799836554      -0.002638522       0.001160580 

``` r
moran.test(results15$KO, cont.listw, alternative = "two.sided")
```


        Moran I test under randomisation

    data:  results15$KO  
    weights: cont.listw    

    Moran I statistic standard deviate = 22.459, p-value < 2.2e-16
    alternative hypothesis: two.sided
    sample estimates:
    Moran I statistic       Expectation          Variance 
          0.763143265      -0.002638522       0.001162611 

``` r
moran.test(results15$Lewica, cont.listw, alternative = "two.sided")
```


        Moran I test under randomisation

    data:  results15$Lewica  
    weights: cont.listw    

    Moran I statistic standard deviate = 16.635, p-value < 2.2e-16
    alternative hypothesis: two.sided
    sample estimates:
    Moran I statistic       Expectation          Variance 
          0.563581731      -0.002638522       0.001158574 

``` r
moran.test(results15$PSL, cont.listw, alternative = "two.sided")
```


        Moran I test under randomisation

    data:  results15$PSL  
    weights: cont.listw    

    Moran I statistic standard deviate = 10.583, p-value < 2.2e-16
    alternative hypothesis: two.sided
    sample estimates:
    Moran I statistic       Expectation          Variance 
          0.357365167      -0.002638522       0.001157122 

``` r
moran.test(results15$Konfederacja, cont.listw, alternative = "two.sided")
```


        Moran I test under randomisation

    data:  results15$Konfederacja  
    weights: cont.listw    

    Moran I statistic standard deviate = 8.795, p-value < 2.2e-16
    alternative hypothesis: two.sided
    sample estimates:
    Moran I statistic       Expectation          Variance 
          0.296677042      -0.002638522       0.001158205 

``` r
locM_PiS <- as.data.frame(localmoran(spNamedVec("PiS", results), 
                                     cont.listw))
locM_KO <- as.data.frame(localmoran(spNamedVec("KO", results), 
                                    cont.listw))
locM_Lewica <- as.data.frame(localmoran(spNamedVec("Lewica", results), 
                                        cont.listw))
locM_PSL <- as.data.frame(localmoran(spNamedVec("PSL", results), 
                                     cont.listw))
locM_Konf <- as.data.frame(localmoran(spNamedVec("Konfederacja", results), 
                                      cont.listw))

locM_PiS15 <- as.data.frame(localmoran(results15$PiS, cont.listw))
locM_KO15 <- as.data.frame(localmoran(results15$KO, cont.listw))
locM_Lewica15 <- as.data.frame(localmoran(results15$Lewica, cont.listw))
locM_PSL15 <- as.data.frame(localmoran(results15$PSL, cont.listw))
locM_Konf15 <- as.data.frame(localmoran(results15$Konfederacja, cont.listw))

brks_PiS <- c(min(locM_PiS[,5]), 0.05000, 0.95000, max(locM_PiS[,5]))
brks_PiS15 <- c(min(locM_PiS15[,5]), 0.05000, 0.95000, max(locM_PiS15[,5]))
cols_PiS <- c("darkblue", "grey90", "lightblue")
brks_KO <- c(min(locM_KO[,5]), 0.05000, 0.95000, max(locM_KO[,5]))
brks_KO15 <- c(min(locM_KO15[,5]), 0.05000, 0.95000, max(locM_KO15[,5]))
cols_KO <- c("orange4", "grey90", "orange")
brks_Lewica <- c(min(locM_Lewica[,5]), 0.05000, 0.95000, max(locM_Lewica[,5]))
brks_Lewica15 <- c(min(locM_Lewica15[,5]), 0.05000, 0.95000, max(locM_Lewica15[,5]))
cols_Lewica <- c("darkred", "grey90", "red")
brks_PSL <- c(min(locM_PSL[,5]), 0.05000, 0.95000, max(locM_PSL[,5]))
brks_PSL15 <- c(min(locM_PSL15[,5]), 0.05000, 0.95000, max(locM_PSL15[,5]))
cols_PSL <- c("darkgreen", "grey90", "lightgreen")
brks_Konf <- c(min(locM_Konf[,5]), 0.05000, 0.95000, max(locM_Konf[,5]))
brks_Konf15 <- c(min(locM_Konf15[,5]), 0.05000, 0.95000, max(locM_Konf15[,5]))
cols_Konf <- c("grey30", "grey90", "grey60")

ggplot(POW)+
  geom_sf(aes(fill=factor(cut(locM_PiS[,5], 
                              breaks=brks_PiS, 
                              include.lowest = T))),
          show.legend = F) +
  scale_fill_manual(values = cols_PiS) +
  ggtitle("PiS 2019") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) -> plotM_PiS
ggplot(POW)+
  geom_sf(aes(fill=factor(cut(locM_KO[,5], 
                              breaks=brks_KO, 
                              include.lowest = T))),
          show.legend = F) +
  scale_fill_manual(values = cols_KO) +
  ggtitle("KO 2019") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) -> plotM_KO
ggplot(POW)+
  geom_sf(aes(fill=factor(cut(locM_Lewica[,5], 
                              breaks=brks_Lewica, 
                              include.lowest = T))),
          show.legend = F) +
  scale_fill_manual(values = cols_Lewica) +
  ggtitle("Lewica 2019") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) -> plotM_Lewica
ggplot(POW)+
  geom_sf(aes(fill=factor(cut(locM_PSL[,5], 
                              breaks=brks_PSL, 
                              include.lowest = T))),
          show.legend = F) +
  scale_fill_manual(values = cols_PSL) +
  ggtitle("PSL 2019") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) -> plotM_PSL
ggplot(POW)+
  geom_sf(aes(fill=factor(cut(locM_Konf[,5], 
                              breaks=brks_Konf, 
                              include.lowest = T))),
          show.legend = F) +
  scale_fill_manual(values = cols_Konf) +
  ggtitle("Konfederacja 2019") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) -> plotM_Konf

ggplot(POW)+
  geom_sf(aes(fill=factor(cut(locM_PiS15[,5], 
                              breaks=brks_PiS15, 
                              include.lowest = T))),
          show.legend = F) +
  scale_fill_manual(values = cols_PiS) +
  ggtitle("PiS 2015") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) -> plotM_PiS15
ggplot(POW)+
  geom_sf(aes(fill=factor(cut(locM_KO15[,5], 
                              breaks=brks_KO15, 
                              include.lowest = T))),
          show.legend = F) +
  scale_fill_manual(values = cols_KO) +
  ggtitle("KO 2015") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) -> plotM_KO15
ggplot(POW)+
  geom_sf(aes(fill=factor(cut(locM_Lewica15[,5], 
                              breaks=brks_Lewica15, 
                              include.lowest = T))),
          show.legend = F) +
  scale_fill_manual(values = cols_Lewica) +
  ggtitle("Lewica 2015") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) -> plotM_Lewica15
ggplot(POW)+
  geom_sf(aes(fill=factor(cut(locM_PSL15[,5], 
                              breaks=brks_PSL15, 
                              include.lowest = T))),
          show.legend = F) +
  scale_fill_manual(values = cols_PSL) +
  ggtitle("PSL 2015") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) -> plotM_PSL15
ggplot(POW)+
  geom_sf(aes(fill=factor(cut(locM_Konf15[,5], 
                              breaks=brks_Konf15, 
                              include.lowest = T))),
          show.legend = F) +
  scale_fill_manual(values = cols_Konf) +
  ggtitle("Konfederacja 2015") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) -> plotM_Konf15

grid.arrange(plotM_PiS, plotM_PiS15,
             plotM_KO, plotM_KO15,
             plotM_Lewica, plotM_Lewica15, 
             ncol = 2)
```

![](spatial_analysis_files/figure-commonmark/local%20Moran%20plots-1.png)

``` r
grid.arrange(plotM_PSL, plotM_PSL15,
             plotM_Konf, plotM_Konf15,
             ncol = 2)
```

![](spatial_analysis_files/figure-commonmark/local%20Moran%20plots-2.png)

``` r
# local Gertis-Ord 2019
locG_PiS <- POW %>% 
  mutate(
    Gi = local_g_perm(PiS, st.cont, 
                      st.weights, 
                      nsim = 999)
  ) %>% 
  unnest(Gi) %>% 
  select(gi, p_folded_sim) %>% 
  mutate(Spots = case_when(
    gi > 0 & p_folded_sim <= 0.05 ~ "Hot",
    gi < 0 & p_folded_sim <= 0.05 ~ "Cold",
    TRUE ~ "Insignificant"
  ),
  Spots = factor(
    Spots,
    levels = c("Hot", "Insignificant", "Cold")
  ))
  
locG_KO <- POW %>% 
  mutate(
    Gi = local_g_perm(KO, st.cont, 
                      st.weights, 
                      nsim = 999)
  ) %>% 
  unnest(Gi) %>% 
  select(gi, p_folded_sim) %>% 
  mutate(Spots = case_when(
    gi > 0 & p_folded_sim <= 0.05 ~ "Hot",
    gi < 0 & p_folded_sim <= 0.05 ~ "Cold",
    TRUE ~ "Insignificant"
  ),
  Spots = factor(
    Spots,
    levels = c("Hot", "Insignificant", "Cold")
  ))

locG_Lewica <- POW %>% 
  mutate(
    Gi = local_g_perm(Lewica, st.cont, 
                      st.weights, 
                      nsim = 999)
  ) %>% 
  unnest(Gi) %>% 
  select(gi, p_folded_sim) %>% 
  mutate(Spots = case_when(
    gi > 0 & p_folded_sim <= 0.05 ~ "Hot",
    gi < 0 & p_folded_sim <= 0.05 ~ "Cold",
    TRUE ~ "Insignificant"
  ),
  Spots = factor(
    Spots,
    levels = c("Hot", "Insignificant", "Cold")
  ))

locG_PSL <- POW %>% 
  mutate(
    Gi = local_g_perm(PSL, st.cont, 
                      st.weights, 
                      nsim = 999)
  ) %>% 
  unnest(Gi) %>% 
  select(gi, p_folded_sim) %>% 
  mutate(Spots = case_when(
    gi > 0 & p_folded_sim <= 0.05 ~ "Hot",
    gi < 0 & p_folded_sim <= 0.05 ~ "Cold",
    TRUE ~ "Insignificant"
  ),
  Spots = factor(
    Spots,
    levels = c("Hot", "Insignificant", "Cold")
  ))

locG_Konfederacja <- POW %>% 
  mutate(
    Gi = local_g_perm(Konfederacja, st.cont, 
                      st.weights,
                      nsim = 999)
  ) %>% 
  unnest(Gi) %>% 
  select(gi, p_folded_sim) %>% 
  mutate(Spots = case_when(
    gi > 0 & p_folded_sim <= 0.05 ~ "Hot",
    gi < 0 & p_folded_sim <= 0.05 ~ "Cold",
    TRUE ~ "Insignificant"
  ),
  Spots = factor(
    Spots,
    levels = c("Hot", "Insignificant", "Cold")
  ))

locG_PiS  %>%  
  ggplot(aes(fill = Spots)) +
  geom_sf(color = "black", lwd = 0.1) +
  scale_fill_brewer(type = "div", palette = 5) +
  labs(title = "PiS 2019") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) -> plotG_PiS
locG_KO  %>%  
  ggplot(aes(fill = Spots)) +
  geom_sf(color = "black", lwd = 0.1) +
  scale_fill_brewer(type = "div", palette = 5) +
  labs(title = "KO 2019") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) -> plotG_KO
locG_Lewica  %>%  
  ggplot(aes(fill = Spots)) +
  geom_sf(color = "black", lwd = 0.1) +
  scale_fill_brewer(type = "div", palette = 5) +
  labs(title = "Lewica 2019") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) -> plotG_Lewica
locG_PSL  %>%  
  ggplot(aes(fill = Spots)) +
  geom_sf(color = "black", lwd = 0.1) +
  scale_fill_brewer(type = "div", palette = 5) +
  labs(title = "PSL 2019") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) -> plotG_PSL
locG_Konfederacja  %>%  
  ggplot(aes(fill = Spots)) +
  geom_sf(color = "black", lwd = 0.1) +
  scale_fill_brewer(type = "div", palette = 5) +
  labs(title = "Konfederacja 2019") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) -> plotG_Konfederacja

# local Gertis-Ord 2015

locG_PiS15 <- POW15 %>% 
  mutate(
    Gi = local_g_perm(PiS, st.cont, 
                      st.weights,
                      nsim = 999)
  ) %>% 
  unnest(Gi) %>% 
  select(gi, p_folded_sim) %>% 
  mutate(Spots = case_when(
    gi > 0 & p_folded_sim <= 0.05 ~ "Hot",
    gi < 0 & p_folded_sim <= 0.05 ~ "Cold",
    TRUE ~ "Insignificant"
  ),
  Spots = factor(
    Spots,
    levels = c("Hot", "Insignificant", "Cold")
  ))

locG_KO15 <- POW15 %>% 
  mutate(
    Gi = local_g_perm(KO, st.cont, 
                      st.weights, 
                      nsim = 999)
  ) %>% 
  unnest(Gi) %>% 
  select(gi, p_folded_sim) %>% 
  mutate(Spots = case_when(
    gi > 0 & p_folded_sim <= 0.05 ~ "Hot",
    gi < 0 & p_folded_sim <= 0.05 ~ "Cold",
    TRUE ~ "Insignificant"
  ),
  Spots = factor(
    Spots,
    levels = c("Hot", "Insignificant", "Cold")
  ))

locG_Lewica15 <- POW15 %>% 
  mutate(
    Gi = local_g_perm(Lewica, st.cont, 
                      st.weights, 
                      nsim = 999)
  ) %>% 
  unnest(Gi) %>% 
  select(gi, p_folded_sim) %>% 
  mutate(Spots = case_when(
    gi > 0 & p_folded_sim <= 0.05 ~ "Hot",
    gi < 0 & p_folded_sim <= 0.05 ~ "Cold",
    TRUE ~ "Insignificant"
  ),
  Spots = factor(
    Spots,
    levels = c("Hot", "Insignificant", "Cold")
  ))

locG_PSL15 <- POW15 %>% 
  mutate(
    Gi = local_g_perm(PSL, st.cont, 
                      st.weights, 
                      nsim = 999)
  ) %>% 
  unnest(Gi) %>% 
  select(gi, p_folded_sim) %>% 
  mutate(Spots = case_when(
    gi > 0 & p_folded_sim <= 0.05 ~ "Hot",
    gi < 0 & p_folded_sim <= 0.05 ~ "Cold",
    TRUE ~ "Insignificant"
  ),
  Spots = factor(
    Spots,
    levels = c("Hot", "Insignificant", "Cold")
  ))

locG_Konfederacja15 <- POW15 %>% 
  mutate(
    Gi = local_g_perm(Konfederacja, st.cont, 
                      st.weights, 
                      nsim = 999)
  ) %>% 
  unnest(Gi) %>% 
  select(gi, p_folded_sim) %>% 
  mutate(Spots = case_when(
    gi > 0 & p_folded_sim <= 0.05 ~ "Hot",
    gi < 0 & p_folded_sim <= 0.05 ~ "Cold",
    TRUE ~ "Insignificant"
  ),
  Spots = factor(
    Spots,
    levels = c("Hot", "Insignificant", "Cold")
  ))

locG_PiS15  %>%  
  ggplot(aes(fill = Spots)) +
  geom_sf(color = "black", lwd = 0.1) +
  scale_fill_brewer(type = "div", palette = 5) +
  labs(title = "PiS 2015") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) -> plotG_PiS15
locG_KO15  %>%  
  ggplot(aes(fill = Spots)) +
  geom_sf(color = "black", lwd = 0.1) +
  scale_fill_brewer(type = "div", palette = 5) +
  labs(title = "KO 2015") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) -> plotG_KO15
locG_Lewica15  %>%  
  ggplot(aes(fill = Spots)) +
  geom_sf(color = "black", lwd = 0.1) +
  scale_fill_brewer(type = "div", palette = 5) +
  labs(title = "Lewica 2015") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) -> plotG_Lewica15
locG_PSL15  %>%  
  ggplot(aes(fill = Spots)) +
  geom_sf(color = "black", lwd = 0.1) +
  scale_fill_brewer(type = "div", palette = 5) +
  labs(title = "PSL 2015") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) -> plotG_PSL15
locG_Konfederacja15  %>%  
  ggplot(aes(fill = Spots)) +
  geom_sf(color = "black", lwd = 0.1) +
  scale_fill_brewer(type = "div", palette = 5) +
  labs(title = "Konfederacja 2015") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) -> plotG_Konfederacja15

grid.arrange(plotG_PiS, plotG_PiS15,
             plotG_KO, plotG_KO15,
             plotG_Lewica, plotG_Lewica15,
             ncol = 2)
```

![](spatial_analysis_files/figure-commonmark/local%20Gertis-Ord%20plots-1.png)

``` r
grid.arrange(plotG_PSL, plotG_PSL15, 
             plotG_Konfederacja, plotG_Konfederacja15,
             ncol = 2)
```

![](spatial_analysis_files/figure-commonmark/local%20Gertis-Ord%20plots-2.png)

``` r
data$ubostwo_pc1000 <- data$ubostwo/data$ludnosc*1000
data$ubostwo_pc1000 <- data$ubostwo_pc1000/mean(data$ubostwo_pc1000)*100
data$firmy_per_tys10 <- data$firmy_per_tys10/mean(data$firmy_per_tys10)*100
data$inwestycje_per_capita <- data$inwestycje_per_capita/mean(data$inwestycje_per_capita)*100
data$PiS <- results$PiS
data$KO <- results$KO
data$Lewica <- results$Lewica
data$PSL <- results$PSL
data$Konfederacja <- results$Konfederacja
data$PiS15 <- results15$PiS
data$KO15 <- results15$KO
data$Lewica15 <- results15$Lewica
data$PSL15 <- results15$PSL
data$Konfederacja15 <- results15$Konfederacja
data$lag.Konfederacja15 <- lag.listw(cont.listw, results15$Konfederacja)
data$lag.Lewica15 <- lag.listw(cont.listw, results15$Lewica)
data$lag.PiS15 <- lag.listw(cont.listw, results15$PiS)
data$lag.KO15 <- lag.listw(cont.listw, results15$KO)
data$lag.PSL15 <- lag.listw(cont.listw, results15$PSL)

form_PiS <- PiS ~ PiS15 + wynagrodzenie + feminizacja + firmy_per_tys10 + demografia +
  log(ludnosc_per_km) + bezrobocie + ubostwo_pc1000 + inwestycje_per_capita

form_KO <- KO ~ KO15 + wynagrodzenie + feminizacja + firmy_per_tys10 + demografia +
  log(ludnosc_per_km) + bezrobocie + ubostwo_pc1000 + inwestycje_per_capita

form_Lewica <- Lewica ~ Lewica15 + wynagrodzenie + feminizacja + firmy_per_tys10 + demografia +
  log(ludnosc_per_km) + bezrobocie + ubostwo_pc1000 + inwestycje_per_capita + lag.Lewica15

form_PSL <- PSL ~ PSL15 + wynagrodzenie + feminizacja + firmy_per_tys10 + demografia +
  log(ludnosc_per_km) + bezrobocie + ubostwo_pc1000 + inwestycje_per_capita

form_Konfederacja <- Konfederacja ~ Konfederacja15 + wynagrodzenie + feminizacja + firmy_per_tys10 + demografia +
  log(ludnosc_per_km) + bezrobocie + ubostwo_pc1000 + inwestycje_per_capita + lag.Konfederacja15

estimate_models <- function (form) {
  model.lm <<- lm(form, data = data)
  GNS <<- sacsarlm(form, data = data, listw = cont.listw, type = "sacmixed", 
                   method = "LU")
  SAC <<- sacsarlm(form, data = data, listw = cont.listw)
  SDEM <<- errorsarlm(form, data = data, listw = cont.listw, etype = "emixed")
  SEM <<- errorsarlm(form, data = data, listw = cont.listw)
  SDM <<- lagsarlm(form, data = data, listw = cont.listw, type = "mixed")
  SAR <<- lagsarlm(form, data = data, listw = cont.listw)
  SLX <<- lmSLX(form, data = data, listw = cont.listw)
}
```

``` r
# change the argument for another party
estimate_models(form_PiS)
lm.morantest(model.lm, cont.listw, alternative = "two.sided")
AIC(model.lm, GNS, SDM, SDEM, SLX, SAR, SAC, SEM)
summary(GNS)
summary(SDM)
summary(SDEM)
summary(SLX)
summary(SAR)
summary(SAC)
summary(SEM)
```

``` r
SDM_PiS <- lagsarlm(form_PiS, data = data, listw = cont.listw, type = "mixed")
SDM_KO <- lagsarlm(form_KO, data = data, listw = cont.listw, type = "mixed")
SAR_Lewica <- lagsarlm(form_Lewica, data = data, listw = cont.listw)
SDM_PSL <- lagsarlm(form_PSL, data = data, listw = cont.listw, type = "mixed")
SAR_Konfederacja <- lagsarlm(form_Konfederacja, data = data, listw = cont.listw)

moran.test(resid(SDM_PiS), cont.listw, alternative = "two.sided")
```


        Moran I test under randomisation

    data:  resid(SDM_PiS)  
    weights: cont.listw    

    Moran I statistic standard deviate = 0.1601, p-value = 0.8728
    alternative hypothesis: two.sided
    sample estimates:
    Moran I statistic       Expectation          Variance 
          0.002801717      -0.002638522       0.001154701 

``` r
moran.test(resid(SDM_KO), cont.listw, alternative = "two.sided")
```


        Moran I test under randomisation

    data:  resid(SDM_KO)  
    weights: cont.listw    

    Moran I statistic standard deviate = -0.41094, p-value = 0.6811
    alternative hypothesis: two.sided
    sample estimates:
    Moran I statistic       Expectation          Variance 
         -0.016585232      -0.002638522       0.001151852 

``` r
moran.test(resid(SAR_Lewica), cont.listw, alternative = "two.sided")
```


        Moran I test under randomisation

    data:  resid(SAR_Lewica)  
    weights: cont.listw    

    Moran I statistic standard deviate = 0.021125, p-value = 0.9831
    alternative hypothesis: two.sided
    sample estimates:
    Moran I statistic       Expectation          Variance 
         -0.001921803      -0.002638522       0.001151081 

``` r
moran.test(resid(SDM_PSL), cont.listw, alternative = "two.sided")
```


        Moran I test under randomisation

    data:  resid(SDM_PSL)  
    weights: cont.listw    

    Moran I statistic standard deviate = -0.56033, p-value = 0.5753
    alternative hypothesis: two.sided
    sample estimates:
    Moran I statistic       Expectation          Variance 
         -0.021651628      -0.002638522       0.001151371 

``` r
moran.test(resid(SAR_Konfederacja), cont.listw, alternative = "two.sided")
```


        Moran I test under randomisation

    data:  resid(SAR_Konfederacja)  
    weights: cont.listw    

    Moran I statistic standard deviate = -0.53759, p-value = 0.5909
    alternative hypothesis: two.sided
    sample estimates:
    Moran I statistic       Expectation          Variance 
         -0.020522190      -0.002638522       0.001106662 

``` r
screenreg(list(SDM_PiS, SDM_KO, SAR_Lewica, SDM_PSL, SAR_Konfederacja),
          custom.coef.names = c(NA, 
                                "results",
                                "salary",
                                "feminisation",
                                "firms_per10k",
                                "demography",
                                "log(pop_per_km)",
                                "unemplyoment",
                                "poverty_per10k",
                                "investments_pc",
                                "lag.results15",
                                "lag.salary",
                                "lag.feminisation",
                                "lag.firms_per10k",
                                "lag.demography",
                                "lag.log(pop_per_km)",
                                "lag.unemployment",
                                "lag.poverty_per10k",
                                "lag.investments_pc",
                                NA,
                                rep(c("results15", "lag.results15"), times=4)))
```


    ====================================================================================
                         Model 1      Model 2      Model 3      Model 4      Model 5    
    ------------------------------------------------------------------------------------
    (Intercept)            44.58 ***    -4.26       -19.64 ***   -12.38        -0.23    
                          (10.93)      (12.89)       (4.90)      (12.14)       (2.28)   
    results                 0.84 ***                                                    
                           (0.02)                                                       
    salary                 -0.04 ***     0.02        -0.01        -0.01         0.00    
                           (0.01)       (0.02)       (0.01)       (0.01)       (0.00)   
    feminisation           -0.43 ***     0.15         0.17 **     -0.05         0.04    
                           (0.08)       (0.11)       (0.06)       (0.10)       (0.03)   
    firms_per10k           -0.05 ***     0.03 ***     0.03 ***     0.01        -0.00    
                           (0.01)       (0.01)       (0.00)       (0.01)       (0.00)   
    demography              0.15 ***    -0.06         0.00         0.00        -0.08 ***
                           (0.04)       (0.05)       (0.03)       (0.05)       (0.01)   
    log(pop_per_km)        -0.53 *       0.53         0.08        -0.51         0.07    
                           (0.23)       (0.30)       (0.14)       (0.28)       (0.06)   
    unemplyoment            0.03        -0.03        -0.06 *       0.02        -0.02    
                           (0.04)       (0.05)       (0.03)       (0.05)       (0.01)   
    poverty_per10k         -0.01 *      -0.00         0.01 **      0.01        -0.00    
                           (0.00)       (0.00)       (0.00)       (0.00)       (0.00)   
    investments_pc         -0.00         0.00         0.00        -0.00        -0.00    
                           (0.00)       (0.00)       (0.00)       (0.00)       (0.00)   
    lag.results15          -0.35 ***    -0.31 ***    -0.27 ***     0.04        -0.32 ** 
                           (0.06)       (0.07)       (0.08)       (0.07)       (0.11)   
    lag.salary             -0.04         0.04                     -0.01                 
                           (0.03)       (0.03)                    (0.03)                
    lag.feminisation        0.12        -0.23                      0.28                 
                           (0.13)       (0.17)                    (0.15)                
    lag.firms_per10k        0.01        -0.05 **                   0.02                 
                           (0.01)       (0.02)                    (0.01)                
    lag.demography          0.08         0.13                     -0.23 **              
                           (0.06)       (0.08)                    (0.08)                
    lag.log(pop_per_km)     0.35         0.49                     -0.52                 
                           (0.34)       (0.44)                    (0.43)                
    lag.unemployment        0.14 *       0.03                     -0.27 **              
                           (0.07)       (0.09)                    (0.08)                
    lag.poverty_per10k     -0.02 **      0.01 *                    0.00                 
                           (0.01)       (0.01)                    (0.01)                
    lag.investments_pc      0.00        -0.01 *                   -0.00                 
                           (0.00)       (0.00)                    (0.00)                
    rho                     0.44 ***     0.47 ***     0.49 ***     0.28 ***     0.51 ***
                           (0.06)       (0.06)       (0.05)       (0.07)       (0.05)   
    results15                            0.77 ***     0.71 ***     0.46 ***     0.61 ***
                                        (0.04)       (0.04)       (0.04)       (0.06)   
    ------------------------------------------------------------------------------------
    Num. obs.             380          380          380          380          380       
    Parameters             21           21           13           21           13       
    Log Likelihood       -809.46      -913.51      -751.81      -880.98      -445.41    
    AIC (Linear model)   1700.50      1916.52      1592.65      1817.49       974.19    
    AIC (Spatial model)  1660.93      1869.02      1529.62      1803.96       916.82    
    LR test: statistic     41.57        49.50        65.02        15.53        59.38    
    LR test: p-value        0.00         0.00         0.00         0.00         0.00    
    ====================================================================================
    *** p < 0.001; ** p < 0.01; * p < 0.05

``` r
W.c <- as(as_dgRMatrix_listw(cont.listw), "CsparseMatrix") 
trMat <- trW(W.c, type = "mult") 
SDM_PiS_imp <- impacts(SDM_PiS, tr = trMat, R = 2000)$res %>% as.data.frame()
SDM_KO_imp <- impacts(SDM_KO, tr = trMat, R = 2000)$res %>% as.data.frame()
SAR_Lewica_imp <- impacts(SAR_Lewica, tr = trMat, R = 2000)$res %>% 
  as.data.frame(
                row.names = c("Lewica15",
                              "salary",
                              "feminisation",
                              "firms_per10k",
                              "demography",
                              "log(pop_per_km)",
                              "unemplyoment",
                              "poverty_per10k",
                              "investments_pc",
                              "lag.Lewica15"))
SDM_PSL_imp <- impacts(SDM_PSL, tr = trMat, R = 2000)$res %>% as.data.frame()
SAR_Konfederacja_imp <- impacts(SAR_Konfederacja, tr = trMat, R = 2000)$res %>% 
  as.data.frame(row.names = c("Konfederacja15",
                              "salary",
                              "feminisation",
                              "firms_per10k",
                              "demography",
                              "log(pop_per_km)",
                              "unemplyoment",
                              "poverty_per10k",
                              "investments_pc",
                              "lag.Konfederacja15"))

SDM_PiS_imp %>% kbl(format = "markdown",
                    digits = 3,
                    col.names = c("Direct",
                                  "Indirect",
                                  "Total"),
                    align = "c",
                    caption = "<center>PiS</center>")
```

Table:
<center>
PiS
</center>

|                       | Direct | Indirect | Total  |
|:----------------------|:------:|:--------:|:------:|
| PiS15                 | 0.844  |  0.023   | 0.867  |
| wynagrodzenie         | -0.046 |  -0.089  | -0.135 |
| feminizacja           | -0.437 |  -0.117  | -0.554 |
| firmy_per_tys10       | -0.053 |  -0.013  | -0.067 |
| demografia            | 0.166  |  0.243   | 0.409  |
| log(ludnosc_per_km)   | -0.513 |  0.202   | -0.311 |
| bezrobocie            | 0.046  |  0.253   | 0.298  |
| ubostwo_pc1000        | -0.009 |  -0.032  | -0.042 |
| inwestycje_per_capita | -0.001 |  0.001   | 0.000  |

``` r
SDM_KO_imp %>% kbl(format = "markdown",
                    digits = 3,
                    col.names = c("Direct",
                                  "Indirect",
                                  "Total"),
                    align = "c",
                    caption = "<center>KO</center>")
```

Table:
<center>
KO
</center>

|                       | Direct | Indirect | Total  |
|:----------------------|:------:|:--------:|:------:|
| KO15                  | 0.775  |  0.092   | 0.867  |
| wynagrodzenie         | 0.026  |  0.093   | 0.118  |
| feminizacja           | 0.131  |  -0.291  | -0.159 |
| firmy_per_tys10       | 0.031  |  -0.054  | -0.023 |
| demografia            | -0.044 |  0.189   | 0.145  |
| log(ludnosc_per_km)   | 0.616  |  1.333   | 1.949  |
| bezrobocie            | -0.030 |  0.030   | 0.000  |
| ubostwo_pc1000        | 0.000  |  0.023   | 0.023  |
| inwestycje_per_capita | 0.000  |  -0.011  | -0.011 |

``` r
SAR_Lewica_imp %>% kbl(format = "markdown",
                    digits = 3,
                    col.names = c("Direct",
                                  "Indirect",
                                  "Total"),
                    align = "c",
                    caption = "<center>Lewica</center>")
```

Table:
<center>
Lewica
</center>

|                 | Direct | Indirect | Total  |
|:----------------|:------:|:--------:|:------:|
| Lewica15        | 0.747  |  0.632   | 1.379  |
| salary          | -0.007 |  -0.006  | -0.012 |
| feminisation    | 0.182  |  0.154   | 0.336  |
| firms_per10k    | 0.027  |  0.023   | 0.049  |
| demography      | 0.001  |  0.001   | 0.002  |
| log(pop_per_km) | 0.080  |  0.067   | 0.147  |
| unemplyoment    | -0.066 |  -0.055  | -0.121 |
| poverty_per10k  | 0.007  |  0.006   | 0.013  |
| investments_pc  | 0.001  |  0.001   | 0.001  |
| lag.Lewica15    | -0.285 |  -0.241  | -0.526 |

``` r
SDM_PSL_imp %>% kbl(format = "markdown",
                    digits = 3,
                    col.names = c("Direct",
                                  "Indirect",
                                  "Total"),
                    align = "c",
                    caption = "<center>PSL</center>")
```

Table:
<center>
PSL
</center>

|                       | Direct | Indirect | Total  |
|:----------------------|:------:|:--------:|:------:|
| PSL15                 | 0.472  |  0.230   | 0.701  |
| wynagrodzenie         | -0.008 |  -0.010  | -0.019 |
| feminizacja           | -0.034 |  0.358   | 0.324  |
| firmy_per_tys10       | 0.008  |  0.024   | 0.032  |
| demografia            | -0.010 |  -0.305  | -0.315 |
| log(ludnosc_per_km)   | -0.547 |  -0.895  | -1.442 |
| bezrobocie            | 0.001  |  -0.358  | -0.357 |
| ubostwo_pc1000        | 0.006  |  0.008   | 0.014  |
| inwestycje_per_capita | -0.001 |  -0.004  | -0.004 |

``` r
SAR_Konfederacja_imp %>% kbl(format = "markdown",
                    digits = 3,
                    col.names = c("Direct",
                                  "Indirect",
                                  "Total"),
                    align = "c",
                    caption = "<center>Konfederacja</center>")
```

Table:
<center>
Konfederacja
</center>

|                    | Direct | Indirect | Total  |
|:-------------------|:------:|:--------:|:------:|
| Konfederacja15     | 0.644  |  0.595   | 1.238  |
| salary             | 0.005  |  0.005   | 0.010  |
| feminisation       | 0.046  |  0.043   | 0.089  |
| firms_per10k       | -0.002 |  -0.002  | -0.004 |
| demography         | -0.083 |  -0.076  | -0.159 |
| log(pop_per_km)    | 0.071  |  0.065   | 0.136  |
| unemplyoment       | -0.022 |  -0.020  | -0.042 |
| poverty_per10k     | -0.001 |  -0.001  | -0.001 |
| investments_pc     | 0.000  |  0.000   | -0.001 |
| lag.Konfederacja15 | -0.337 |  -0.311  | -0.648 |
