Impact des caractéristiques éducatives et personnelles sur le salaire de départ
================
Marie-Anne Randrianarivony,N’GUESSAN WILFRIED ARMEL,Fatoumata Binta
Diallo

## Contexte
Projet réalisé dans le cadre du cours **Introduction à la science des données et à l’intelligence des affaires** (Automne 2025).

L’objectif est d’analyser les facteurs qui influencent la réussite professionnelle des jeunes diplômés, mesurée par leur Starting Salary.

## Question de recherche
Dans quelle mesure les caractéristiques éducatives et personnelles influencent-elles le salaire de départ des jeunes diplômés ?

Nous analysons :

 - 📊  Performance académique (GPA)

 - 💼 Expérience pratique (stages complétés, projets réalisés)

 - 🤝 Compétences relationnelles (Soft Skills)

 - 🎓 Certifications

 - 👩‍💼 Niveau de poste

 - 🏫 Domaine d’étude

 - ⚧ Genre

## Données
 - 400 observations
 - 19 variables
 - Variables catégorielles, numériques discrètes et continues
 - Variable réponse : Starting Salary
Les données respectent les exigences du projet (> 50 observations, diversité de types de variables).
Un aperçu des données est disponible dans le dossier /data via glimpse()



## Méthodologie
L’analyse s’est déroulée en quatre étapes :
 1. Description des données (dimensions, types de variables)
 2. Analyse exploratoire (statistiques descriptives, visualisations ciblées)
 3. Comparaisons entre groupes (genre, domaine, niveau de poste)
 4. Modélisation : régression linéaire multiple avec `Starting Salary` comme variable réponse

Objectif du modèle : estimer l’effet marginal de chaque variable explicative sur le salaire de départ.

## Résultats principaux

 - 📈 Un niveau de poste plus élevé est fortement associé à un salaire plus élevé.
 - 💻 Les domaines comme informatique et médecine présentent des salaires moyens supérieurs.
 - 🎓 Les stages complétés ont un effet positif sur le salaire.
 - 🤝 Les Soft Skills montrent une association positive avec le salaire.
 - ⚧ Un léger écart salarial est observé entre les genres dans ce jeu de données.
 - 📌 34 % des étudiants ont complété 3 stages.

## Conclusion

Les résultats suggèrent que la réussite professionnelle initiale dépend principalement :

 - Du **niveau de poste obtenu**

 - Du **domaine d’étude**

 - De **l’expérience pratique** (stages)

 - Des **compétences relationnelles**

Ce projet démontre l’importance d’une combinaison entre performance académique, expérience concrète et compétences humaines dans l’intégration au marché du travail.

