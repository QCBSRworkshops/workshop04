##Section: 01-preparation-pour-l-atelier.R 




##Section: 02-introduction-fr.R 




##Section: 03-modele-lineaire.R 

# Chargez les paquets et le jeu de données bird
library(e1071)
library(MASS)
setwd("~/Desktop/...") # N'oubliez pas de spécifier votre répertoire de travail (note: le vôtre sera différent de celui-ci)
bird<-read.csv("birdsdiet.csv")

# Visualisez le tableau de données :
names(bird)
str(bird)
head(bird)
summary(bird)
plot(bird)

lm1 <- lm(bird$MaxAbund ~ bird$Mass) # où Y ~ X signifie Y "en fonction de" X>

opar <- par(mfrow=c(2,2)) # Permet de créer les graphiques dans un panneau 2 x 2
plot(lm1)
par(opar) # Remet la fenêtre graphique à un seul panneau

# Graphique Y ~ X et la ligne de régression
plot(bird$MaxAbund ~ bird$Mass, pch=19, col="coral", ylab="Maximum Abundance",
     xlab="Mass")
abline(lm1, lwd=2)
?plot # Pour obtenir plus de détails sur les arguments de la fonction plot().
# Allez voir colours() pour une liste de couleurs.

# Les données sont-elles distribuées normalement ?
hist(bird$MaxAbund,col="coral", main="Untransformed data",
     xlab="Maximum Abundance")
hist(bird$Mass, col="coral", main="Untransformed data", xlab="Mass")

# Teste l'hypothèse nulle que l'échantillon provient d'une population distribuée normalement
shapiro.test(bird$MaxAbund)
shapiro.test(bird$Mass)
# Si p < 0.05, la distribution n'est pas normale
# Si p > 0.05, la distribution est normale

skewness(bird$MaxAbund)
skewness(bird$Mass)
# Une valeur positive indique une asymétrie vers la gauche (i.e. left-skewed distribution)
# tandis qu'une valeur négative indique une asymétrie vers la droite (i.e. right skewed distribution).

# Ajoutez les variables transformées au tableau
bird$logMaxAbund <- log10(bird$MaxAbund)
bird$logMass <- log10(bird$Mass)
names(bird) # pour visualiser le tableau avec les nouvelles variables

hist(bird$logMaxAbund,col="yellowgreen", main="Log transformed",
     xlab=expression("log"[10]*"(Maximum Abundance)"))
hist(bird$logMass,col="yellowgreen", main="Log transformed",
     xlab=expression("log"[10]*"(Mass)"))
shapiro.test(bird$logMaxAbund); skewness(bird$logMaxAbund)
shapiro.test(bird$logMass); skewness(bird$logMass)

# Refaire l'analyse avec les transformations appropriées
lm2 <- lm(bird$logMaxAbund ~ bird$logMass)

# Reste-il des problèmes avec ce modèle (hétéroscédasticité, non-indépendance, forte influence)?
opar <- par(mfrow=c(2,2))
plot(lm2, pch=19, col="gray")
par(opar)

# Examinons les coefficients du modèle ainsi que les valeurs de p
summary(lm2)

# Vous pouvez faire apparaître seulement les coefficients
lm2$coef

# Quoi d'autre ?
str(summary(lm2))
summary(lm2)$coefficients # où Std. Error est l'erreur type de chaque coefficient
summary(lm2)$r.squared # Coefficient de détermination
summary(lm2)$adj.r.squared # Coefficient de détermination ajusté
summary(lm2)$sigma # Erreur type résiduelle (racine du carré moyen de l'erreur)
# etc.

# Vous pouvez également vérifier l'équation du coefficient de détermination (R<sup>2</sup>)par vous-mêmes :
SSE <- sum(resid(lm2)^2)
SST <- sum((bird$logMaxAbund - mean(bird$logMaxAbund))^2)
R2 <- 1 - ((SSE)/SST)
R2

plot(logMaxAbund ~ logMass, data=bird, pch=19, col="yellowgreen",
                   ylab = expression("log"[10]*"(Maximum Abundance)"), xlab = expression("log"[10]*"(Mass)"))
abline(lm2, lwd=2)

# On peut faire ressortir les points avec une forte influence
points(bird$logMass[32], bird$logMaxAbund[32], pch=19, col="violet")
points(bird$logMass[21], bird$logMaxAbund[21], pch=19, col="violet")
points(bird$logMass[50], bird$logMaxAbund[50], pch=19, col="violet")

# On peut également tracer les intervalles de confiance
confit<-predict(lm2,interval="confidence")
points(bird$logMass,confit[,2])
points(bird$logMass,confit[,3])

# Souvenez-vous qu'on peut exclure des valeurs avec le symbole "!"
# On peut analyser un sous-ensemble des données de "bird" en utilisant l'argument 'subset' de la fonction lm().
lm3 <- lm(logMaxAbund ~ logMass, data=bird, subset =! bird$Aquatic) # enlever les oiseaux aquatiques du modèle

# Cette commande permet également d'exclure les oiseaux aquatiques
lm3 <- lm(logMaxAbund ~ logMass, data=bird, subset=bird$Aquatic == 0)

# Examinons le modèle
opar <- par(mfrow=c(2,2))
plot(lm3)
summary(lm3)
par(opar)

# Comparons les deux analyses
opar <- par(mfrow=c(1,2))
plot(logMaxAbund ~ logMass, data=bird, main="All birds", ylab = expression("log"[10]*"(Maximum Abundance)"),
     xlab = expression("log"[10]*"(Mass)"))
abline(lm2,lwd=2)

plot(logMaxAbund ~ logMass, data=bird, subset=!bird$Aquatic, main="Terrestrial birds",
     ylab = expression("log"[10]*"(Maximum Abundance)"), xlab = expression("log"[10]*"(Mass)"),
     pch=19)
abline(lm3,lwd=2)
opar(par)

lm4 <- lm(logMaxAbund ~ logMass, data=bird, subset=bird$Passerine == 1)

# Examinez les graphiques de diagnostic
opar <- par(mfrow=c(2,2))
plot(lm4)
summary(lm4)
par(opar)

# Comparez la variance expliquée par lm2, lm3 et lm4
str(summary(lm4)) # Rappelez-vous qu'on veut le R^2 ajusté
summary(lm4)$adj.r.squared # R2-adj = -0.02
summary(lm2)$adj.r.squared # R2-adj = 0.05
summary(lm3)$adj.r.squared # R2-adj = 0.25

# Comparez visuellement les trois modèles
opar <- par(mfrow=c(1,3))
plot(logMaxAbund ~ logMass, data=bird, main="All birds",
     ylab = expression("log"[10]*"(Maximum Abundance)"),
     xlab = expression("log"[10]*"(Mass)"), pch=19, col="yellowgreen")
abline(lm2,lwd=2)

plot(logMaxAbund ~ logMass, subset=Passerine == 1, data=bird, main="Passerine birds",
     ylab = expression("log"[10]*"(Maximum Abundance)"),
     xlab = expression("log"[10]*"(Mass)"), pch=19, col="violet")
abline(lm4,lwd=2)

plot(logMaxAbund ~ logMass, data=bird, subset=!bird$Aquatic, main="Terrestrial birds",
     ylab = expression("log"[10]*"(Maximum Abundance)"),
     xlab = expression("log"[10]*"(Mass)"), pch=19, col="skyblue")
abline(lm3,lwd=2)
par(opar)


##Section: 04-test-t-anova.R 

# Test de t
boxplot(logMass ~ Aquatic, data=bird, ylab=expression("log"[10]*"(Bird Mass)"),
        names=c("Non-Aquatic","Aquatic"),
        col=c("yellowgreen","skyblue"))

# Tout d'abord, vérifions si les variances de chaque groupe sont égales
# Note : il n'est pas nécessaire de vérifier la normalité des données,
# car on utilise déjà une transformation logarithmique
tapply(bird$logMass,bird$Aquatic,var)
var.test(logMass~Aquatic,data=bird)

# Nous sommes prêts pour le test de t
ttest1 <- t.test(logMass~Aquatic, var.equal=TRUE, data=bird)

# Cette commande est équivalente :
ttest1 <- t.test(x=bird$logMass[bird$Aquatic==0], y=bird$logMass[bird$Aquatic==1], var.equal=TRUE)
ttest1

# Test de t en spécifiant l'argument "alternative"
uni.ttest1 <- t.test(logMass~Aquatic, var.equal=TRUE, data=bird, alternative="less")
uni.ttest1

ttest.lm1 <- lm(logMass ~ Aquatic, data=bird)
anova(ttest.lm1)

ttest1$statistic^2
anova(ttest.lm1)$F

# Ordre alphabétique par défaut
boxplot(logMaxAbund ~ Diet, data=bird)

# Réorganiser l'ordre des facteurs
med <- sort(tapply(bird$logMaxAbund, bird$Diet, median))
boxplot(logMaxAbund ~ factor(Diet, levels=names(med)), data=bird, col=c("white","lightblue1",
           "skyblue1","skyblue3","skyblue4"))

plot.design(logMaxAbund ~ Diet, data=bird, ylab = expression("log"[10]*"(Maximum Abundance)"))

# En utilisant aov()
aov1 <- aov(logMaxAbund ~ Diet, data=bird)
summary(aov1)

# En utilisant lm()
anov1 <- lm(logMaxAbund ~ Diet, data=bird)
anova(anov1)

# Diagrammes de diagnostic
opar <- par(mfrow=c(2,2))
plot(anov1)
par(opar)

# Test de la supposition de la normalité des résidus
shapiro.test(resid(anov1))

# Test de la supposition de l'homogénéité de la variance
bartlett.test(logMaxAbund ~ Diet, data=bird)

aov1 <- aov(logMaxAbund ~ Diet, data=bird)

summary(aov1)

anov1 <- lm(logMaxAbund ~ Diet, data=bird)

anova(anov1)

# À quel niveau se situe la différence de diète ?
TukeyHSD(aov(anov1),ordered=T)

# Cette commande est équivalente à la précédente :
TukeyHSD(aov1,ordered=T)

# Visualisation d'un modèle d'ANOVA à l'aide de la fonction barplot()

sd <- tapply(bird$logMaxAbund,list(bird$Diet),sd)
means <- tapply(bird$logMaxAbund,list(bird$Diet),mean)
n <- length(bird$logMaxAbund)
se <- 1.96*sd/sqrt(n)

bp <- barplot(means, col=c("white","lightblue1","skyblue1","skyblue3","skyblue4"),
       ylab = expression("log"[10]*"(Maximum Abundance)"), xlab="Diet", ylim=c(0,1.8))


# Ajout des lignes verticales représentant les erreurs types
segments(bp, means - se, bp, means + se, lwd=2)
# et des lignes horizontales
segments(bp - 0.1, means - se, bp + 0.1, means - se, lwd=2)
segments(bp - 0.1, means + se, bp + 0.1, means + se, lwd=2)

bird$Diet2 <- relevel(bird$Diet, ref="Plant")
anov2 <- lm(logMaxAbund ~ Diet2, data=bird)
summary(anov2)
anova(anov2)

contrasts(bird$Diet2)

contrasts(bird$Diet2) <- cbind(c(4,-1,-1,-1,-1), c(0,1,1,-1,-1), c(0,0,0,1,-1), c(0,1,-1,0,0))

sum(contrasts(bird$Diet)[,1]) # première condition pour la colonne 1
sum(contrasts(bird$Diet)[,1]*contrasts(bird$Diet)[,2]) # deuxième condition pour les colonnes 1 et 2

anov4 <- lm(logMaxAbund ~ Diet*Aquatic, data=bird)
opar <- par(mfrow=c(2,2))
plot(anova4)
par(opar)
summary(anov4)
anova(anov4)

anov5 <- lm(logMaxAbund ~ Diet + Aquatic, data=bird)
anova(anov5, anov4)

interaction.plot(bird$Diet, bird$Aquatic, bird$logMaxAbund, col="black",
                 ylab = expression("log"[10]*"(Maximum Abundance)"), xlab="Diet")

table(bird$Diet, bird$Aquatic)

anova(anov3,anov5) # Rappel que anov3 est le modèle avec le facteur "Diet" seulement.

unb_anov1 <- lm(logMaxAbund ~ Aquatic + Diet, data=bird)
unb_anov2 <- lm(logMaxAbund ~ Diet + Aquatic, data=bird)
anova(unb_anov1)
anova(unb_anov2)

Anova(unb_anov1,type="III")


##Section: 05-ancova.R 

ancova.example <- lm(uptake ~ conc*Treatment, data=CO2)
anova(ancova.example)

install.packages("effects")
library(effects)
adj.means <- effect('Treatment', ancova.example)
plot(adj.means)

adj.means <- effect('conc*Treatment', ancova.example)
plot(adj.means)

# Si vous avez complété la section avancée sur les contrastes, vous devrez réinitialiser les contrastes
# à l'aide de la fonction ''options()''
# Si vous n'avez pas complété la section avancée sur les contrastes, ignorez la première ligne du script.
options(contrasts=c("contr.treatment", "contr.poly"))
ancov1 <- lm(logMaxAbund ~ logMass*Diet, data=bird)
summary(ancov1)
anova(ancov1)

ancov2 <- lm(logMaxAbund ~ logMass + Diet, data=bird)

lm2 <- lm(logMaxAbund ~ logMass, data=bird)

coef(ancov1)


plot(logMaxAbund~logMass, data=bird, col=Diet, pch=19, ylab=expression("log"[10]*"(Maximum Abundance)"),
     xlab=expression("log"[10]*"(Mass)"))
abline(a=coef(ancov1)[1],b=coef(ancov1)[2], col="deepskyblue1")
abline(a=sum(coef(ancov1)[1]+coef(ancov1)[3]),b=sum(coef(ancov1)[2]+coef(ancov1)[7]),col="green2", lwd=2)
abline(a=sum(coef(ancov1)[1]+coef(ancov1)[4]),b=sum(coef(ancov1)[2]+coef(ancov1)[8]),col="orange1", lwd=2)
abline(a=sum(coef(ancov1)[1]+coef(ancov1)[5]),b=sum(coef(ancov1)[2]+coef(ancov1)[9]),col="lightsteelblue1",
       lwd=2)
abline(a=sum(coef(ancov1)[1]+coef(ancov1)[6]),b=sum(coef(ancov1)[2]+coef(ancov1)[10]),col="darkcyan", lwd=2)


##Section: 06-regression-multiple.R 

vif(clDD ~ clFD + clTmi + clTma + clP + grass, data=Dickcissel)

hist(Dickcissel$abund, main="", xlab="Dickcissel abundance")
shapiro.test(Dickcissel$abund)
skewness(Dickcissel$abund)
summary(Dickcissel$abund)

hist(log10(Dickcissel$abund+0.1), =main="", xlab=expression("log"[10]*"(Dickcissel Abundance + 0.1)"))
shapiro.test(log10(Dickcissel$abund+0.1))
skewness(log10(Dickcissel$abund+0.1))

lm.mult <- lm(abund ~ clTma + NDVI + grass, data=Dickcissel)
summary(lm.mult)

plot(abund ~ clTma, data=Dickcissel, pch=19, col="orange")
plot(abund ~ NDVI, data=Dickcissel, pch=19, col="skyblue")
plot(abund ~ grass, data=Dickcissel, pch=19, col="green")

lm.linear <- lm(abund ~ clDD, data=Dickcissel)
lm.quad <- lm(abund ~ clDD + I(clDD^2), data=Dickcissel)
lm.cubic <- lm(abund ~ clDD + I(clDD^2) + I(clDD^3), data=Dickcissel)

anova(lm.linear,lm.quad,lm.cubic) # fait la liste des modèles par ordre croissant de complexité.
# On accepte le modèle se trouvant sur la ligne la plus basse avec une valeur de p significative.
# i.e. le modèle lm.quad

# Examinons le résumé du modèle
summary(lm.quad)
# Les coefficients de régression
summary(lm.quad)$coefficients[,1]
# Estimation des valeurs de p
summary(lm.quad)$coefficients[,4]
# Le R carré ajusté
summary(lm.quad)$adj.r.squared

lm.full <- lm(abund ~ . - Present, data=Dickcissel)
lm.step <- step(lm.full)
summary(lm.step)

part.lm = varpart(Dickcissel$abund, Dickcissel[,c("clDD", "clFD", "clTmi", "clTma", "clP")],
                  Dickcissel[,c("broadleaf", "conif", "grass", "crop", "urban", "wetland")])
part.lm

# Variables climatiques
out.1 = rda(Dickcissel$abund, Dickcissel[,c("clDD", "clFD", "clTmi", "clTma", "clP")],
            Dickcissel[,c("broadleaf", "conif", "grass", "crop", "urban", "wetland")])
anova(out.1, step=1000, perm.max=1000)

# Variables du paysage
out.2 = rda(Dickcissel$abund, Dickcissel[,c("broadleaf", "conif", "grass", "crop", "urban", "wetland")],
        Dickcissel[,c("clDD", "clFD", "clTmi", "clTma", "clP")])
anova(out.2, step=1000, perm.max=1000)

showvarparts(2)
plot(part.lm,digits=2, bg=rgb(48,225,210,80,maxColorValue=225), col="turquoise4")


##Section: 07-considerations-finales.R 




##Section: 08-references-fr.R 




