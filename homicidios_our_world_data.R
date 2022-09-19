
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
