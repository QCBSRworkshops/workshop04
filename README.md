# [QCBS R Workshop Series](https://r.qcbs.ca/)

This series of 10 workshops walks participants through the steps required to use R for a wide array of statistical analyses relevant to research in biology and ecology. These open-access workshops were created by members of the QCBS both for members of the QCBS and the larger community.

The content of this workshop has been peer-reviewed by several QCBS members. If you would like to suggest modifications, please contact the current series coordinators, listed [here](https://r.qcbs.ca/about/) or open a pull request (see contributing guidelines at <https://r.qcbs.ca/presenter-developer-protocol/developing-en.html>).

# [Série d'ateliers R du CSBQ](https://r.qcbs.ca/fr/)

Cette série de 10 ateliers guide les participants à travers les étapes requises afin de maîtriser le logiciel R pour une grande variété d'analyses statistiques pertinentes en recherche en biologie et en écologie. Ces ateliers en libre accès ont été créés par des membres du CSBQ à la fois pour les membres du CSBQ et pour la grande communauté d'utilisateurs de R.

Le contenu de cet atelier a été révisé par plusieurs membres du CSBQ. Si vous souhaitez y apporter des modifications, veuillez SVP contacter les coordonnateurs actuels de la série, listés [ici](https://r.qcbs.ca/fr/about/) ou ouvrez un pull request (voir les instructions <https://r.qcbs.ca/presenter-developer-protocol/developper-fr.html>).

# Workshop 4: Linear models

In this workshop, you will learn how to implement in R basic linear models commonly used in ecology such as simple regression, analysis of variance (ANOVA), analysis of covariance (ANCOVA), and multiple regression. After verifying visually and statistically the assumptions of these models and transforming your data when necessary, the interpretation of model outputs and the plotting of your final model will no longer keep secrets from you!

# Atelier 4: Modéles linéaires

Dans cet atelier, vous apprendrez comment effectuer en R les modèles linéaires fréquemment utilisés en écologie tels que la régression simple, l’analyse de variance (ANOVA), l’analyse de covariance (ANCOVA) et la régression multiple. Après avoir vérifié les postulats de ces modèles (visuellement et statistiquement) et transformé vos données si nécessaire, l’interprétation des résultats et leur représentation graphique n’auront plus de secrets pour vous!

# Workshop materials

Language | Slides | Bookdown | Script | GitHub 
:--------|:-------|:-----|:------ |:-------
EN | [![badge](https://img.shields.io/static/v1?style=flat-square&label=Slides&message=04&color=red&logo=html5)](https://r.qcbs.ca/workshop04/pres-en/workshop04-pres-en.html) | [![badge](https://img.shields.io/static/v1?style=flat-square&label=book&message=04&logo=github)](https://r.qcbs.ca/workshop04/book-en/index.html) | [![badge](https://img.shields.io/static/v1?style=flat-square&label=script&message=04&color=2a50b8&logo=r)](https://r.qcbs.ca/workshop04/book-en/workshop04-script-en.R) | [![badge](https://img.shields.io/static/v1?style=flat-square&label=repo&message=dev&color=6f42c1&logo=github)](https://github.com/QCBSRworkshops/workshop04) 
FR | [![badge](https://img.shields.io/static/v1?style=flat-square&label=Diapos&message=04&color=red&logo=html5)](https://r.qcbs.ca/workshop04/pres-fr/workshop04-pres-fr.html) | [![badge](https://img.shields.io/static/v1?style=flat-square&label=livre&message=04&logo=github)](https://r.qcbs.ca/workshop04/book-fr/index.html) | [![badge](https://img.shields.io/static/v1?style=flat-square&label=script&message=04&color=2a50b8&logo=r)](https://r.qcbs.ca/workshop04/book-fr/workshop04-script-fr.R) | [![badge](https://img.shields.io/static/v1?style=flat-square&label=repo&message=dev&color=6f42c1&logo=github)](https://github.com/QCBSRworkshops/workshop04) 

> *Note: The wiki for this workshop was converted to Bookdown in September 2021. <br> The wiki pages for this workshop will no longer be updated (Archive: [EN](https://wiki.qcbs.ca/r_workshop4), [FR](https://wiki.qcbs.ca/r_atelier4)).*

# License

This work is licensed under a [Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/).

[![License: CC BY-SA 4.0](https://img.shields.io/badge/License-CC%20BY--SA%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by-sa/4.0/)

# Contributors | Contributeurs et contributrices 

This workshop was originally developed by Catherine Baltazar, Bérenger Bourgeois, Zofia Taranu, and Shaun Turney. 

Since 2014, several QCBS members contributed to consistently and collaboratively develop and update this workshop, as part of the *Learning and Development Award* from the Québec Centre for Biodiversity Science. They were:

|      2022 - 2021 - 2020     |      2019 - 2018 - 2017     |      2016 - 2015 - 2014      |
|:---------------------------:|:---------------------------:|:----------------------------:|
| Victor Cameron  | Willian Vieira | Catherine Baltazar |
| Laurie Maynard  | Shaun Turney  | Bérenger Bourgeois |
| Daniel Schoenig  | Marie-Hélène Brice | Zofia Taranu |
| Pedro Henrique P. Braga | Katherine Hébert | Shaun Turney |
|   |  | Emmanuelle Chrétien |
|   |  | Maxwell Farrell |
|   |  | Vincent Fugère |

# Development status

**Template** 

[![receive-from-template-and-dispatch-to-workflows](https://github.com/QCBSRworkshops/workshop04/workflows/receive-from-template-and-dispatch-to-workflows/badge.svg)](https://github.com/QCBSRworkshops/workshop04/actions?query=workflow%3Areceive-from-template-and-dispatch-to-workflows) 

**Building workshop materials**

Language | Slides | Book
:------- | :----- | :-----
EN  | [![render-presentation-en](https://github.com/QCBSRworkshops/workshop04/workflows/render-presentation-en/badge.svg)](https://github.com/QCBSRworkshops/workshop04/actions?query=workflow%3Arender-presentation-en) | [![render-book-en](https://github.com/QCBSRworkshops/workshop04/workflows/render-book-en/badge.svg)](https://github.com/QCBSRworkshops/workshop04/actions?query=workflow%3Arender-book-en)
FR   | [![render-presentation-fr](https://github.com/QCBSRworkshops/workshop04/workflows/render-presentation-fr/badge.svg)](https://github.com/QCBSRworkshops/workshop04/actions?query=workflow%3Arender-presentation-fr) | [![render-book-fr](https://github.com/QCBSRworkshops/workshop04/workflows/render-book-fr/badge.svg)](https://github.com/QCBSRworkshops/workshop04/actions?query=workflow%3Arender-book-fr)
