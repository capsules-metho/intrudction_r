df <- read.csv("data/ces_1993.csv")
df <- readxl::read_excel("data/ces_1993.xlsx")
df <- readRDS("data/ces_1993.rds")
df <- haven::read_sav("data/ces_1993.sav")

# Installer les packages nécessaires
install.packages("dplyr") # Manipulation de données
install.packages("ggplot2") # Graphiques
install.packages("ces") # Données de l'étude électorale canadienne

# Charger les packages nécessaires
library(dplyr) # Manipulation de données
library(ggplot2) # Graphiques
library(ces)

# Importer les données swiss (jeu de données intégré)
df <- get_ces("2021")

# Explorer les données
View(df)
summary(df)
names(df)
head(df)
ncol(df)
nrow(df)


# Voir les metadonnées de la variable 'cps21_votechoice'
attributes(df$cps21_votechoice)

# Voir le nombre de valeurs pour chaque élément d'une variable
table(df$cps21_votechoice)

# Histogramme de la variable 'cps21_votechoice'
hist(df$cps21_votechoice)


# Sélectionner des colonnes 
# (par exemple, cps21_votechoice, Education et Agriculture)

df_selected <- df %>%
  select(cps21_votechoice, cps21_education, cps21_yob)

# Filtrer les lignes pour inclure uniquement les gens 
# avec une année de naissance supérieure à la moyenne

mean_yob <- mean(df_selected$cps21_yob, na.rm = TRUE)

df_filtered <- df_selected %>%
  filter(cps21_yob > mean_yob)

# Créer une nouvelle variable binaire "high_education" 
attributes(df$cps21_education)

df_mutated <- df_filtered %>%
  mutate(high_education = ifelse(cps21_education >= 9, 1, 0))

# Regrouper par "high_education" et calculer la moyenne de l'age

df_summarized <- df_mutated %>%
  group_by(high_education) %>%
  summarize(moyenne_age = mean(cps21_yob, na.rm = TRUE))

# Afficher le résultat final
print(df_summarized)


# Nettoyage minimal pour le graphique
df_graph <- df %>%
  select(cps21_votechoice, cps21_education, cps21_yob) %>%
  mutate(
    age = 2021 - as.numeric(cps21_yob),
    age_group = ifelse(is.na(age), NA_character_,
                  ifelse(age < 30, "18–29",
                    ifelse(age < 45, "30–44",
                      ifelse(age < 65, "45–64", "65+")))),
    educ_cat = case_when(
      cps21_education %in% 1:6  ~ "Secondaire ou moins",
      cps21_education %in% 7:8  ~ "Collège/CEGEP",
      cps21_education %in% 9:11 ~ "Université+",
      TRUE ~ NA_character_
    ),
    votechoice = case_when(
      cps21_votechoice == 1 ~ "Libéral",
      cps21_votechoice == 2 ~ "Conservateur",
      cps21_votechoice == 3 ~ "NPD",
      cps21_votechoice == 4 ~ "Bloc Québécois",
      cps21_votechoice == 5 ~ "Vert",
      cps21_votechoice == 6 ~ "Autre",
      TRUE ~ "Indécis"
    )
  ) %>%
  mutate(
    age_group = factor(age_group, levels = c("18–29","30–44","45–64","65+")),
    educ_cat  = factor(educ_cat,  levels = c("Secondaire ou moins","Collège/CEGEP","Université+")),
    votechoice= factor(votechoice,levels = c("Libéral","Conservateur","NPD","Bloc Québécois","Vert","Autre","Indécis"))
  )

ggplot(df_graph, aes(x = votechoice, fill = age_group)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ educ_cat) +
  labs(
    title = "Âge et scolarité selon l'intention de vote",
    x = "Intention de vote",
    y = "Nombre de répondants",
    fill = "Âge"
  ) +
  theme_minimal()

ggsave("grahiques/vote_age_education_dodge.png", width = 10, height = 6)
