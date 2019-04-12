#TP1
#https://www.yan-holtz.com/PDF/Ggplot2_basicTP.html

# MOT DE PASSE POUR LA CORRECTION : ilovecheese

library(ggplot2)
library(dplyr)

# Importation du dataSet  gapminder
library(gapminder)
library(hrbrthemes)

#CORRELATION
data=gapminder



#Regarder?les 6 premieres lignes avec head 
head(data)

# Nombre de lignes
nrow(data)


# Combien d'années differentes
gapminder %>%
  select(year)%>%
  unique()%>%
  nrow()

#ou
length(unique(gapminder$year))

# Number of country available per year

gapminder %>%
 ?group_by(year) %>%
  summarize(n=n())

# Construire un scatterplot qui montre la relation entre gdp et lifeExp in 1952

gapminder %>%
  filter(year=="1952") %>%
  ggplot( aes(x=lifeExp,y=gdpPercap)) + geom_point()

# Trouver le point chelou et le retirer
g?pminder %>%
  filter(year=="1952") %>%
  filter(gdpPercap<90000) %>%
  # coloration des pays en fonction des continents
  ggplot( aes(x=gdpPercap,y=lifeExp, color=continent)) + geom_point()


# Ajouter une difference taille des cercle en fonction de la pop?lation des pays
# Il s'agit donc d'un 
gapminder %>%
  filter(year=="1952") %>%
  filter(gdpPercap<90000) %>%
  ggplot( aes(x=gdpPercap,y=lifeExp, color=continent, size=pop)) + geom_point() +theme_ipsum()


# DISTRIBUTION

# Load dataset from github
data1 ?- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/1_OneNum.csv", header=TRUE)

head(data1)
nrow(data1)

# permet d'afficher des petites stats simple
summary(data1)

# On affiche un histograme
data %>%
  # Filtre les ?rix au dessus de 1500???
  filter(price<1500)%>%
  ggplot(aes(x=price)) + geom_histogram(binwidth=10)

 #binwidth permet de modifier la largeur des barres

# Afficher un density chart

data %>%
  filter(price<1500)%>%
  ggplot(aes(x=price)) + geom_density(mapping=NULL, fill="blue?,bw=20)

# faire un help(geom_density) pour avoir de la doc
#bw c'est le binwidth de la geom_density

# NOUVEAU DATA
# Load dataset from github
data2 <- read.table("https://raw.githubusercontent.com/holtzy/Teaching/master/DATA/probability.csv", header=TRUE? sep=",")
head(data2)
nrow(data2)
summary(data2)

# Afficher les 8 types de réponses
data2 %>%
  group_by(text) %>%
  summarize(n=n())

#faire un basic boxplot

data2 %>%
  ggplot(aes(x=text,y=value, fill=text)) + geom_boxplot()


# On va apporter des amél?orations
# On va classer les text par moyenne, on effectue un trie avec le package forcats
library(forcats)

data2 %>%
  mutate(text= fct_reorder(text, value, .fun=median))%>%
  ggplot(aes(x=text,y=value, fill=text)) + geom_boxplot() + theme_ipsum()

# Ave? boxplot, le plus gros problème c'est qu'on ne connait pas la densité
# On va donc afficher les points

data2 %>%
  mutate(text= fct_reorder(text, value, .fun=median))%>%
  ggplot(aes(x=text,y=value, fill=text)) +
  geom_boxplot() + 
  geom_jitter(color="g?ey", width=.4, size=.5, alpha=.8) + # ajoute les points
  theme(legend.position="none") + # permet d'enlever la légende
  coord_flip() # permet de tourner le boxplot

# On va maintenant changer le box plot en violin

data2 %>%
  mutate(text= fct_reorder(te?t, value, .fun=median))%>%
  ggplot(aes(x=text,y=value, fill=text)) +
  geom_violin()

# 3 - RANKING

# Load dataset from github
data3 <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/7_OneCatOneNum.csv", header=TR?E, sep=",")

head(data3)
summary(data3)

# ici nous réalisons un barplot
data3 %>%
  ggplot(aes(x=Country,y=Value)) +
    geom_bar(stat="identity", fill="#69b3a2")
# La couleur est identique car nous l'avons fait dans geom_bar

data3 %>%
  ggplot(aes(x=Cou?try,y=Value, fill=Country)) +
  geom_bar(stat="identity")
# Ici la couleur est differente pour chaque pays.

# on va maintenant utiliser coord_flip() pour que ce soit plus lisible

data3 %>%
  ggplot(aes(x=Country,y=Value)) +
  geom_bar(stat="identity", fi?l="#69b3a2") +
    coord_flip()

# On va aussi classer les pays en fonction de la valeur des ventes d'qrmes

data3 %>%
  filter(!is.na(Value))%>%
  arrange(Value) %>%
  mutate(Country=factor(Country,Country))%>%
  ggplot(aes(x=Country,y=Value)) +
  geom_ba?(stat="identity", fill="#69b3a2") +
  coord_flip()


# et on supprime les pays qui n'ont pas de valeurs


# On peut également faire des lollipop

data3 %>%
  filter(!is.na(Value))%>%
  arrange(Value) %>%
  mutate(Country=factor(Country,Country))%>%
  ggplo?(aes(x=Country,y=Value)) +
  geom_segment(aes(x=Country,xend=Country,y=0, yend=Value)) + # Syntaxe speciale mais necessaire
  # Pour les arguments des points c'est pour l'apparence
  geom_point(size=2, color="red", fill=alpha("orange", 0.1),alpha=0.9, shap?=21, stroke=1) +
  coord_flip()


# 4 - EVOLUTION

data4 <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/3_TwoNumOrdered.csv", header=T)
data4$date <- as.Date(data4$date)
# Juste au dessus c'est pour changer le fo?mat de la colonne date en format date


head(data4)
# basic line chart
data4 %>%
  ggplot(aes(x=date,y=value)) +
  geom_line()

# area chart
data4 %>%
  ggplot(aes(x=date,y=value)) +
    geom_area(fill="blue", color="red")

# On va selectionner les 10 dern?eres valeurs

data4 %>%
  tail(n=10)%>%
  ggplot(aes(x=date,y=value)) +
  geom_line() + # permet d'afficher les courbres
  geom_point()+ # permet d'afficher les points de mesures
  geom_area(fill="blue", color="red")

# On va faire des diagrammes interacti?s







  






