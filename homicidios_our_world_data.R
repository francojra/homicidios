
# Homicídios -------------------------------------------------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 18/09/22 ---------------------------------------------------------------------------------------------------------------------------
# Referência: https://ourworldindata.org/homicides -----------------------------------------------------------------------------------------

# Sobre os dados ---------------------------------------------------------------------------------------------------------------------------

### Esse artigo trata de homicídios intecionais que são definidos como "uma
### morte ilegal infligida deliberadamente a uma pessoa por outra".

### Homicídio se refere a uma violência interpessoal. Mortes civis e 
### militares durante guerras interestaduais, guerras civis e genocídios 
### não são contados como homicídios.

### Menos que 1% das mortes no mundo são por causa de homicídio, mas em alguns
### países a porcentagem alcança 10%. Globalmente, 0.7% das mortes em 2019
### foram resultantes de homicídio.

### A porcentagem de mortes varia em todo mundo. Existem grandes diferenças
### entre países e regiões.

### Em torno dos países da Europa ocidental, por exemplo, menos que 0.1% de 
### mortes resultaram de homicídio. Já em grande parte da Europa oriental,
### norte da África, Ásia e Oceania, foi menos que 0.5%. Nos Estados Unidos
### alcançou 0.6%.

### Em alguns países a porcentagem é muito maior. Nós vemos altas taxas em
### países da América Latina. Mais que 7% de mortes em El Salvador foram
### por homicídios, mais que 6% na Guatemala e 5% na Venezuela.

# Carregar pacotes -------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(cols4all)

# Carregar dados ---------------------------------------------------------------------------------------------------------------------------

hom <- read.csv("share-of-deaths-homicides.csv")
view(hom)
names(hom)

# Mnaipular dados --------------------------------------------------------------------------------------------------------------------------

hom <- hom %>%
  rename(por_mor = Deaths...Interpersonal.violence...Sex..Both...Age..All.Ages..Percent.) %>%
  view()

hom <- hom %>%
  select(por_mor, Entity, Year) %>%
  view()

hom1 <- hom %>%
  filter(Entity %in% c("United States", "Canada", "Mexico", "Guatemala",
                       "Honduras", "Nicaragua", "Colombia", "Venezuela",
                       "Brazil", "Ecuador", "Paraguay", "Argentina",
                       "Chile", "Bolivia", "Peru", "Cuba", 
                       "Dominican Republic")) %>%
  view()

hom2 <- hom1 %>%
  group_by(Entity) %>%
  summarise(media = mean(por_mor),
            n = n(), sd = sd(por_mor),
            se = sd/sqrt(n)) %>%
  view()

# Gráficos ---------------------------------------------------------------------------------------------------------------------------------

ggplot(hom1, aes(x = Year, y = por_mor, group = Entity, col = Entity)) +
  geom_line(size = 1.5) +
  facet_wrap(. ~ Entity, scales = "free") +
  labs(x = "Tempo (anos)", y = "Porcentagem de mortes por homicídio") +
  theme_light() +
  theme(legend.position = "none") 

c4a_gui()
c4a("rainbow", 17)

ggplot(hom2, aes(x = Entity, y = media, fill = Entity)) +
  geom_col() +
  geom_errorbar(aes(ymin = media - se, ymax = media + se),
                    size = 0.8, width = 0.3) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = c("#D1BBD7", "#BA8DB4",
"#AA6F9E", "#994F88",
"#882E72", "#1965B0",
"#5289C7", "#7BAFDE",
"#4EB265", "#90C987",
"#CAE0AB", "#F7F056",
"#F6C141", "#F1932D",
"#E8601C", "#DC050C",
"#72190E")) +
  labs(x = "Países", y = "Porcentagem de mortes por homicídio") +
  theme_light() +
  theme(legend.position = "none") 


