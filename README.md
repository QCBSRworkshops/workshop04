# [QCBS R Workshop series](https://wiki.qcbs.ca/r)

This series of 10 workshops walks participants through the steps required to use R for a wide array of statistical analyses relevant to research in biology and ecology. These open-access workshops were created by members of the QCBS both for members of the QCBS and the larger community.

The content of this workshop has been peer-reviewed by several QCBS members. If you would like to suggest modifications, please contact the current series coordinators, listed [here](https://wiki.qcbs.ca/r).

# [Série d'ateliers R du CSBQ](https://wiki.qcbs.ca/r)

Cette série de 10 ateliers guide les participants à travers les étapes requises afin de maîtriser le logiciel R pour une grande variété d’analyses statistiques pertinentes en recherche en biologie et en écologie. Ces ateliers en libre accès ont été créés par des membres du CSBQ à la fois pour les membres du CSBQ et pour la grande communauté d’utilisateurs de R.

Le contenu de cet atelier a été révisé par plusieurs membres du CSBQ. Si vous souhaitez y apporter des modifications, veuillez SVP contacter les coordonnateurs actuels de la série, listés [ici](https://wiki.qcbs.ca/r).

# Workshop 4: Linear models

[![Build Status](https://travis-ci.org/QCBSRworkshops/workshop04.svg?branch=dev)](https://travis-ci.org/QCBSRworkshops/workshop04)

In this workshop, you will learn how to implement in R basic linear models commonly used in ecology such as simple regression, analysis of variance (ANOVA), analysis of covariance (ANCOVA), and multiple regression. After verifying visually and statistically the assumptions of these models and transforming your data when necessary, the interpretation of model outputs and the plotting of your final model will no longer keep secrets from you!

# Atelier 4: Modéles linéaires

[![Build Status](https://travis-ci.org/QCBSRworkshops/workshop04.svg?branch=dev)](https://travis-ci.org/QCBSRworkshops/workshop04)

Dans cet atelier, vous apprendrez comment effectuer en R les modèles linéaires fréquemment utilisés en écologie tels que la régression simple, l’analyse de variance (ANOVA), l’analyse de covariance (ANCOVA) et la régression multiple. Après avoir vérifié les postulats de ces modèles (visuellement et statistiquement) et transformé vos données si nécessaire, l’interprétation des résultats et leur représentation graphique n’auront plus de secrets pour vous!



# Links

#### [English](https://qcbsrworkshops.github.io/workshop04/workshop04-en/workshop04-en.html)

#### [Français](https://qcbsrworkshops.github.io/workshop04/workshop04-fr/workshop04-fr.html)

# Developers

1. Set the working directory set to this folder.
2. then use:

``` r
install.packages("remotes")
remotes::install_github("QCBSRworkshops/qcbsRworkshops")
library("qcbsRworkshops")
build_workshops()
```
