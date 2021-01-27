# Nom: Catherine Baltazar, Bérenger Bourgeois, Zofia Taranu 
# Date: Septembre 2014
# Description: Modèles linéaires, test t, ANOVA et ANCOVA
# Bases de données: fichiers "birdsdiet.csv" et "dickcissel.csv"
# Notes: Le script R et les bases de données utilisées sont issus, en partie, du matériel pédagogique de Brian McGill (http://brianmcgill.org/614)
#***************************************************************************************#

#### Chargement des données####
rm(list=ls())
install.packages("e1071")
install.packages("MASS")
library(e1071)
library(MASS)
setwd("~/Dropbox/CSBQ:QCBS R Ateliers:Workshops (1)/Workshop 4-lm/English Final")
# Notez que ce répertoire de travail NE FONCTIONNERA PAS sur votre ordinateur ! Vous devez spécifier le répertoire (fichier) où les
# données sont sauvegardées
bird<-read.csv("birdsdiet.csv")

#### Exploration des données ####
names(bird)
str(bird)
head(bird)
summary(bird) 

# Explorons les relations de bases
plot(bird)  # note: la fonction pairs(bird) renvoie le méme résultat

#***************************************************************************************#

#### Régression linéaire ####

# Ce type de régression permet de modéliser une relation linéaire entre une variable réponse (Y) et une variable dépendante (X)
# Créons maintenant un jeu de données artificielles pour mieux comprendre le fonctionnement de la régression linéaire:
X <- 1:100
Y <- X + rnorm(100,sd=5) # la fonction rnorm(100) effectue un tirage aléatoire de 100 points de données à partir d'une distibution normale. Voir ?runif pour plus d'informations
lm1 <- lm(Y~X) # lm1 correspond a notre modele de régression linéaire ouù Y ~ X signifie Y "en fonction de" X
plot(X,Y) # permet de visualiser les données crées

# Traçons maintenant les graphiques diagnostics de notre régression linéaire
opar <- par(mfrow=c(2,2)) # cette fonction spécifie que les graphiques construits par la suite seront placés selon une grille de 2 lignes x 2 colonnes, la fonction plot(lm1) produisant 4 graphiques
plot(lm1)
par(opar) # ré-initialise la fenêtre graphique 

# Essayons maintenant avec la base de données "bird"
# Notre hypoyhése principale postule que l'abondance maximale (MaxAbund) est une fonction linéaire de la masse (Mass)
lm1 <- lm(bird$MaxAbund ~ bird$Mass) 

# l'option data=bird peut également être utilisée à la place de data$variable.name pour simplifier le code
lm1 <- lm(MaxAbund ~ Mass, data=bird)

# Traçons maintenant les graphiques diagnostics (à faire AVANT de regarder les résultats du modèle)
opar <- par(mfrow=c(2,2)) # graphique 2 x 2
plot(lm1)
par(opar) # rétablir les paramètres de graphiques précédents
# Que nous suggèrent les résidus ?
# Note: les résidus peuvent aussi être extraits du modèle lm1 en utilisant
hist(resid(lm1))

# Traçons le graphique Y ~ X et la droite de régression correspondante
plot(bird$MaxAbund ~ bird$Mass)
abline(lm1, lwd=2) 

# Les données sont-elles normalement distribuées ?
hist(bird$MaxAbund, main="Non-transformé")
hist(bird$Mass, main="Non-transformé")

# Vérifions la normalité des données à l'aide d'un test de Shapiro-Wilk et d'un test d'asymétrie:
shapiro.test(bird$MaxAbund) 
shapiro.test(bird$Mass)
?shapiro.test   # teste l'hypothèse nulle selon laquelle l'échantillon provient d'une population distribuée normalement 
# Si p < 0.05 --> non-normalité
# Si p > 0.05 --> normalité
skewness(bird$MaxAbund)
skewness(bird$Mass) 
?skewness # mesure de symétrie : une valeur positive indique que la distribution des données est décalée vers la gauche,
# et une valeur négative que la distribution des données est décalée vers la droite.

# Normalisons les données en appliquant une transformation log10() et ajoutons ces données transformées à notre base de données
bird$logMaxAbund <- log10(bird$MaxAbund)
bird$logMass <- log10(bird$Mass)
names(bird) # vérifie que la procédure d'ajout des données transformées à la base données a fonctionné

hist(bird$logMaxAbund,main="Transformé", xlab=expression("log"[10]*"(Maximum Abundance)"))
hist(bird$logMass,main="Transformé",xlab=expression("log"[10]*"(Mass)"))
shapiro.test(bird$logMaxAbund); skewness(bird$logMaxAbund)
shapiro.test(bird$logMass); skewness(bird$logMass)

# Ré-exécutons la régression linéaire sur les données transformées
lm2 <- lm(logMaxAbund ~ logMass, data=bird)

# Semble-t-il encore y avoir des problèmes sur les graphiques diagnostics (hétéroscédasticité, non-indépendence, données aberrantes)?
opar <- par(mfrow=c(2,2))
plot(lm2)
par(opar)

# Note : les points de données aberrantes peuvent être retrouvées dans la base de données à partir de leur numéro de colonne (indiqué par les distances de Cook)
# bird[c(32,21,50),]
# Lorsque cela est justifié, les points de données aberrantes peuvent être éliminés et le modèle ré-exéctuté 
# bird2 <- bird[-c(32,21,50),]
# lm2.rmlev <- lm(logMaxAbund ~ logMass, data=bird2)
# plot(bird2$logMaxAbund ~ bird2$logMass)
# abline(lm2.rmlev) 
# abline(lm2,col="red") 

# Traçons finalement le graphique de Y ~ X et la doite de régression
plot(logMaxAbund ~ logMass, data=bird, xlim=c(min(bird$logMass),max(bird$logMass)),ylab = expression("log"[10]*"(Maximum Abundance)"), xlab = expression("log"[10]*"(Mass)"))
abline(lm2) 

# Nous pouvons aussi signaler les points de données aberrantes
points(bird$logMass[32], bird$logMaxAbund[32], pch=19, col="red")
points(bird$logMass[21], bird$logMaxAbund[21], pch=19, col="red")
points(bird$logMass[50], bird$logMaxAbund[50], pch=19, col="red")

# Calcul des intervalles de confiance(IC)
confit<-predict(lm2,interval="confidence")
head(confit)
head(confit[,2]) # donne un aperçu des valeurs inférieures de l'intervalle de confiance
head(confit[,3]) # donne un aperçu des valeurs supérieures de l'intervalle de confiance

# Traçons les IC
points(bird$logMass,confit[,2],col="blue") 
points(bird$logMass,confit[,3],col="blue")

# Lorsque les graphiques diagnostics sont en accord avec les postulats de la régression linéaire, nous pouvons alors observer les estimés des paramètres du modèle et les valeurs de probabilité
summary(lm2)

# Nous pouvons aussi extraire uniquement les estimés des paramètres du modèle
lm2$coef

# Quoi d'autre ?
str(summary(lm2))
summary(lm2)$coefficients # Std. Error = erreurs standards des estimés des paramètres du modèle; t-value = test de la diffèrence des estimés des paramètres à zéro
summary(lm2)$r.squared # R2 (coefficient de détermination = SSreg/ SStotal)
# Note: la régression simple correspond au seul cas pour lequel le carré du coefficient de corrélation est égal au coefficient de détermination: voir (cor.test(bird$logMaxAbund,bird$logMass)$estimate)^2
summary(lm2)$adj.r.squared # R2 ajusté
summary(lm2)$sigma # erreur type des résidus (racine carrée du carré moyen de l'erreur)
# etc.

# Vous pouvez aussi vérifier, pour votre information personnelle, l'équation du R2:
# SSE = sum(resid(lm2)^2)
# SST = sum((bird$logMaxAbund - mean(bird$logMaxAbund))^2)
# R2 = 1 - ((SSE)/SST)
# R2

#***************************************************************************************#

# L'analyse des oiseaux terrestres seulement permet-elle d'améliorer le modèle de régression?

# Rappelons que vous pouvez exclure des objets en utilisant "!" 
# Ré-analysons un échantillon de la base de données en utilisant cette commande dans la fonction lm() 
?lm # pour la liste complète des arguments (cf. argument "subset")

lm3 <- lm(logMaxAbund ~ logMass, data=bird, subset =! bird$Aquatic) # élimine les oiseaux aquatiques du modèle de régression

# ou également
lm3 <- lm(logMaxAbund ~ logMass, data=bird, subset=bird$Aquatic == 0)

# Examinons les graphiques diagnostics
opar <- par(mfrow=c(2,2))
plot(lm3)
summary(lm3)
par(opar)

# Comparons les deux modèles de régression
opar <- par(mfrow=c(1,2))
plot(logMaxAbund ~ logMass, data=bird, main="Tout les oiseaux", ylab = expression("log"[10]*"(Maximum Abundance)"), 
     xlab = expression("log"[10]*"(Mass)"),col="darkgreen")
abline(lm2)

plot(logMaxAbund ~ logMass, data=bird, subset=!bird$Aquatic, main="Oiseaux terrestres",ylab = expression("log"[10]*"(Maximum Abundance)"), 
     xlab = expression("log"[10]*"(Mass)"), col="skyblue")
abline(lm3)
par(opar)
#***************************************************************************************#

#### Défi 1 ####
# Examiner la relation entre logMaxAbund et logMass chez les passereaux ("passerine birds")
# Indice: tout comme les espèces aquatiques, les espèces de passereaux sont codées 0/1, ce qui peut être vérifié à partir de la structure de la base de données:
str(bird) 
# (Réponse ci-dessous)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
#
# 
# 
# Réponse
lm4 <- lm(logMaxAbund ~ logMass, data=bird, subset=bird$Passerine == 1)

# Examinons les graphiques diagnostics
opar <- par(mfrow=c(2,2))
plot(lm4)
par(opar)
summary(lm4)

# Comparons les variances expliquées par les modèles lm2, lm3 et lm4
str(summary(lm4)) # Rappelons-nous cette comparaison doit être basée sur le R2 ajusté (adj.r.squared)
summary(lm4)$adj.r.squared # R2-adj = -0.02
summary(lm2)$adj.r.squared # R2-adj = 0.05
summary(lm3)$adj.r.squared # R2-adj = 0.25

# Comparons visuellement les 3 modèles
opar <- par(mfrow=c(1,3))
plot(logMaxAbund ~ logMass, data=bird, main="All birds", ylab = expression("log"[10]*"(Maximum Abundance)"), 
     xlab = expression("log"[10]*"(Mass)"), col="green")
abline(lm2)

plot(logMaxAbund ~ logMass, subset=Passerine == 1, data=bird, main="Passerine birds",ylab = expression("log"[10]*"(Maximum Abundance)"), 
     xlab = expression("log"[10]*"(Mass)"), col="red")
abline(lm4)

plot(logMaxAbund ~ logMass, data=bird, subset=!bird$Aquatic, main="Terrestrial birds",ylab = expression("log"[10]*"(Maximum Abundance)"), 
     xlab = expression("log"[10]*"(Mass)"), col="skyblue")
abline(lm3)
par(opar)

#**********************************************************************************************#

#### Test t ####

# Hypothèse: les oiseaux aquatiques sont plus lourds que les oiseaux terrestres
# Ici, le facteur explicatif possède seulement deux niveaux
boxplot(logMass ~ Aquatic, data=bird, ylab=expression("log"[10]*"(Bird Mass)"), names=c("Non-Aquatic", "Aquatic"), col=c("green","skyblue"))

# Testons tout d'abord l'homogénéité des variances 
# Note: la vérification de la normalité des données n'est pas requise puisque nous les avons déjà transformées 
var.test(logMass~Aquatic,data=bird)
# le rapport des variances n'est pas statistiquement différent de 1, celles-ci peuvent donc être considérées comme égales 

# Nous sommes maintenant prêt à effectuer le test t en spécifiant que l'homogénéité des variances est respectée
?t.test
ttest1 <- t.test(logMass ~ Aquatic,  var.equal=TRUE, data=bird)

# ou également
ttest1 <- t.test(x=bird$logMass[bird$Aquatic==0], y=bird$logMass[bird$Aquatic==1], var.equal=TRUE)
ttest1

# Note: au lieu de la fonction t.test(), la function lm() peut également être utilisée 
ttest.lm1 <- lm(logMass ~ Aquatic, data=bird)
anova(ttest.lm1)
# Note : le carré de la valeur du test t est égal à la valeur F lorsque que les écarts sont homogènes
ttest1$statistic^2 # 60.3845
anova(ttest.lm1)$F # 60.3845

# Test t unilatéral
uni.ttest1 <- t.test(logMass~Aquatic, var.equal=TRUE, data=bird, alternative="less")
uni.ttest1 # ce test évalue si les oiseaux aquatiques sont plus lourds que les oiseux terrestres

# Comment évaluer si les oiseaux terrestres sont plus lourds que les oiseaux aquatiques ?
uni.ttest2 <- t.test(logMass~Aquatic,  var.equal=TRUE, data=bird, alternative="greater")
uni.ttest2

#**********************************************************************************************#

#### ANOVA ####

# Utilisons maintenant l'analyse de variance pour déterminer l'effet du régime alimentaire (Diet ; variable catégorique), sur l'abondance maximale (logMaxAbund)
# Hypothèse: l'abondance maximale dépend du régime alimentaire
# Une ANOVA est requise pour répondre à cette hypothèse puisque le facteur explicatif possède plus de deux niveaux 

# Visualisons tout d'abord les données
boxplot(logMaxAbund ~ Diet, data=bird)
# Note: R ordonne par défaut les niveaux du facteur explicatif selon leur ordre alphabétique

# L'ordre des niveaux du facteur explicatif peut égalment suivre l'ordre coissant de leurs médianes respectives 
med <- sort(tapply(bird$logMaxAbund, bird$Diet, median))
boxplot(logMaxAbund ~ factor(Diet, levels=names(med)), data=bird, col=c("white","lightblue1","skyblue1","skyblue3","skyblue4"))

# La meilleure façon de visualiser graphiquement la moyenne de la variable réponse (logMaxAbund) pour chaque niveau du facteur (Diet) est d'utiliser la fonction plot.design()
plot.design(logMaxAbund ~ Diet, data=bird, ylab = expression("log"[10]*"(Maximum Abundance)"))

# Exécutons une ANOVA à l'aide de la fonction aov()
aov1 <- aov(logMaxAbund ~ Diet, data=bird)
summary(aov1) # l'effet du régime alimentaire est significatif (p<0.05; au moins un des niveaux du régime alimentaire diffère des autres)

# Ou, exécutons un modèle lm() de l'abondance maximale en fonction du régime alimentaire
anov1 <- lm(logMaxAbund ~ Diet, data=bird)
anova(anov1) # effet significatif du régime alimentaire (p<0.05; au moins un des niveaux du régime alimentaire diffère des autres)

# ANOVA (aov) et régression linéaire (lm) sont donc identiques. 
# Notez que les résultats de l'ANOVA sont identiques à ceux de la régression linéaire et observez les estimés des paramètres (effet de chaque niveau du régime alimentaire)
summary.lm(aov1)
summary(anov1)

# Traçons les graphiques diagnostics
opar <- par(mfrow=c(2,2))
plot(anov1)
par(opar)
# Le premier graphique devrait idéalement indiquer une dispersion similaire des données pour chaque niveau du régime alimentaire

# Testons la supposition de la normalité des résidus
shapiro.test(resid(anov1))
# Le test de Shapiro étant non-significatif, les résidus du modèle peuvent être considérés comme normaux 

# Testons la supposition d'homogénéité des variances
bartlett.test(logMaxAbund ~ Diet, data=bird)
# Le test de Bartlett étant non-significatif, les variances peuvent être considérées comme homogènes

# Résultats du modèle
summary(anov1) 
# Notez que R trie les niveaux du facteur (Diet) selon leur ordre alphabétique et compare chacun des niveaux au niveau de référence (ici, le niveau "Insect")
# Si un niveaux de la variable explicative correpond au controle, il peut être choisi comme niveau de référence 

# Lorque l'ANOVA détecte un effet significatif de la variable explicative, un test post-hoc doit être effectué pour déterminer quel(s) traitement(s) diffère(nt) des autres
TukeyHSD(aov(anov1),ordered=T) 
# ou également
TukeyHSD(aov1,ordered=T) # seuls les régimes alimentaires "Vertebrate" et "PlantInsect" diffèrent
                
# Au cas où les résultats du test post-hoc soit difficilement lisibles, vous pouvez essayer:
Tukey <- TukeyHSD(aov1,ordered=T) 
Tukey$Diet[which(Tukey$Diet[,4] < 0.05),] 

#***************************************************************************************#

# Illustration graphique de l'ANOVA à l'aide de la fonction barplot()

sd <- tapply(bird$logMaxAbund,list(bird$Diet),sd) 
means <- tapply(bird$logMaxAbund,list(bird$Diet),mean)
n <- length(bird$logMaxAbund)
se <- 1.96*sd/sqrt(n)

bp <- barplot(means, col=c("white","lightblue1","skyblue1","skyblue3","skyblue4"), 
       ylab = expression("log"[10]*"(Maximum Abundance)"), xlab="Diet", ylim=c(0,1.8))

# Ajouter les barres verticales des écarts types
segments(bp, means - se, bp, means + se, lwd=2)
# ainsi que les lignes horizontales
segments(bp - 0.1, means - se, bp + 0.1, means - se, lwd=2)
segments(bp - 0.1, means + se, bp + 0.1, means + se, lwd=2)

#**********************************************************************************************#

#### ANOVA à deux critéres de classification ####

#### Défi 2 ####
# Essayez de déterminer si l'abondance maximale (logMaxAbund) varie à la fois en fonction du régime alimentaire (Diet) et de l'habitat (Aquatic)
# (Réponse ci-dessous)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#

anov2 <- lm(logMaxAbund ~ Diet*Aquatic, data=bird)
opar <- par(mfrow=c(2,2))
plot(anov2) # N'oubliez pas de vérifier les graphiques diagnostics !
par(opar)

summary(anov2) 
anova(anov2)
# Existe-t-il une interaction entre les deux variables explicatives ?

# L'interaction peut également être testée en comparant deux modèles nichés à l'aide de la fonction anova()
anov3 <- lm(logMaxAbund ~ Diet + Aquatic, data=bird)
anova(anov3, anov2)
# Les modèles avec ou sans interaction ne diffèrent pas (p > 0.05)
# L'interaction est donc éliminée du modèle pour conserver le modèle le plus parcimonieux

# Un graphique d'interaction peut également être utilisé:
interaction.plot(bird$Diet, bird$Aquatic, bird$logMaxAbund, col="black", 
                 ylab = expression("log"[10]*"(Maximum Abundance)"), xlab="Diet")
# Que signifie le trou dans la courbe correspondant aux oiseaux aquatiques ? 
table(bird$Diet, bird$Aquatic)
# Le graphique suggère-t-il la présence d'une interaction ?

#### Défi 3 ####

# Testez si l'habitat (Aquatic) a un effet significatif en comparant deux modèles nichés
# (Réponse ci-dessous)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
anova(anov1,anov3) # Rappelons-nous que le modèle anov1 comprend seulement le régime alimentaire (Diet) comme facteur
# L'habitat (Aquatic) n'est donc pas significative (p > 0.05)

#**********************************************************************************************#

#### ANCOVA ####

# Combinaison de l'ANOVA et de régression linéaire

# Par exemple, en utilisant la base de données CO2, où Y=consommation (uptake), X=concentration (conc) et Z=traitement (Treatment),
# l'ANCOVA Y ~ X*Z est implémentée par:
ancova.example <- lm(uptake ~ conc*Treatment, data=CO2)
anova(ancova.example)

# Pour comparer correctement les moyennes des différents niveaux du facteur, il faut utiliser les moyennes ajustées fournies par l'équation de l'ANCOVA qui estime l'effet moyen de chaque niveau du facteur (Z) corrigé pour l'effet de la covariable (X)
install.packages("effects")
library(effects)
adj.means <- effect('Treatment', ancova.example)
plot(adj.means)


#### Défi 4 ####

# Exécutez une ANCOVA pour tester les effets du régime alimentaire (Diet) et la masse (logMass), ainsi que leur interaction, sur l'abondance maximale des oiseaux (logMaxAbund)
# (Réponse ci-dessous)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
ancov1 <- lm(logMaxAbund ~ logMass*Diet, data=bird)
summary(ancov1)
anova(ancov1)
# Etant donné que le terme d'interaction entre logMass et Diet est non-significatif, nous nous attendons à ce que les pentes de régression 
# de chaque type de régime alimentaire soient égales (ou que l'un des types de régime alimentaire n'ait pas suffisament d'observations pour que le modèle soit appliqué).
# Notez que le régime alimentaire est également non-significatif lorsque logMass est inclus dans le modèle.

# Nous pouvons tout d'abord éliminer le terme d'interaction, puis ré-évaluer le modèle contenant les effets simples de logMass et Diet
ancov2 <- lm(logMaxAbund ~ logMass + Diet, data=bird)
summary(ancov2)
anova(ancov2)

# Comparons finalement les deux modèles nichés avec et sans le terme Diet
anova(lm2, ancov2) 
# Nous pouvons finalement éliminer le terme Diet

# Aucun effet significatif de la variable Diet n'a été détecté, correspondant à une égalité des pentes et des ordonnées à l'origine des droites de régression de chaque niveau de régime alimentaire
# Il est néanmoins préfèrable de tracer les droites de régression du modèle ANCOVA (modèle ancov1 ci-sessus) pour vérifier
# si celles-ci sont réellement parallèles ou si la non-significativité du terme Diet est à une faible puissance du test statistique.

# Tracer les pentes et ordonnées à l'origine du modèle ANCOVA (modèle ancov1 ci-dessus) en utilisant les fonctions abline() et coef()
?abline

# Visualisons les coefficients
summary(ancov1)$coefficients

# ou également
coef(ancov1)
# Les termes Intercept et logMass correpondent à l'ordonnée à l'origine et à la pente de la droite de régression du niveau de référence du régime alimentaire (premier niveau de la variable régime alimentaire selon l'ordre alphabétique)
# Pour obtenir les ordonnées à l'origine et les pentes des autres niveaux de régime alimentaire, il faut ajouter à ces coefficients ceux du niveau de référence.


plot(logMaxAbund~logMass, data=bird, col=Diet, pch=19, ylab=expression("log"[10]*"(Maximum Abundance)"),xlab=expression("log"[10]*"(Mass)"))
abline(a=coef(ancov1)[1],b=coef(ancov1)[2], col="deepskyblue1")
abline(a=sum(coef(ancov1)[1]+coef(ancov1)[3]),b=sum(coef(ancov1)[2]+coef(ancov1)[7]), col="green2", lwd=2)
abline(a=sum(coef(ancov1)[1]+coef(ancov1)[4]),b=sum(coef(ancov1)[2]+coef(ancov1)[8]), col="orange1", lwd=2)
abline(a=sum(coef(ancov1)[1]+coef(ancov1)[5]),b=sum(coef(ancov1)[2]+coef(ancov1)[9]), col="lightsteelblue1", lwd=2)
abline(a=sum(coef(ancov1)[1]+coef(ancov1)[6]),b=sum(coef(ancov1)[2]+coef(ancov1)[10]), col="darkcyan", lwd=2)


#**********************************************************************************************#

#### Régression multiple ####

# La base de données "Dickcissel" vise à déterminer les variables environnementales qui influencent l'absence/présence et l'abondance de différentes espèces d'oiseaux
# Ici, nous utiliserons une seule espèce, Dickcissel 

Dickcissel <- read.csv("Dickcissel.csv")
head(Dickcissel)
str(Dickcissel)
plot(Dickcissel) 

# Comparons l'importance relative des trois variables climat, productivité et occupation du sol sur l'abondance de l'espèce Dickcissel en exécutant
# une régression multiple de l'abondance en fonction des variables clTma + NDVI + grass   
lm.mult <- lm(abund ~ clTma + NDVI + grass, data=Dickcissel)

opar <- par(mfrow=c(2,2))
plot(lm.mult)
par(opar)

summary(lm.mult)

opar <- par(mfrow=c(1,3),mar=c(4,4,1,2),mgp=c(2,.7,0))
plot(abund ~ clTma, data=Dickcissel, col="orange")
plot(abund ~ NDVI, data=Dickcissel, col="skyblue2")
plot(abund ~ grass, data=Dickcissel, col="green")
par(opar)

#### Défi 5 ####

# Est-il nécessaire d'appliquer une transformation à la variable réponse "abund"?
# (réponse ci-bas)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
#
hist(Dickcissel$abund, main="", xlab="Dickcissel abundance")
shapiro.test(Dickcissel$abund)
skewness(Dickcissel$abund)

summary(Dickcissel$abund) # beaucoup de zéros! On peut ajouter une constante avant d'effectuer une transformation log10() 

hist(log10(Dickcissel$abund+0.1), main="", xlab=expression("log"[10]*"(Dickcissel Abundance + 0.1)"))
shapiro.test(log10(Dickcissel$abund+0.1))
skewness(log10(Dickcissel$abund+0.1)) # ne suit toujours pas une distribution normale



#**********************************************************************************************#

#### Régression pas à pas ####

# Exécutons une régression multiple contenant les variables disponibles dans la base de données, sauf la variable Present
lm.full <- lm(abund ~ . - Present, data=Dickcissel) 
summary(lm.full)

# Puis sélectionnons le modèle le plus parcimonieux sur la base de l'AIC, ou critère d'Akaike
lm.step <- step(lm.full)
summary(lm.step)
?step

#**********************************************************************************************#

