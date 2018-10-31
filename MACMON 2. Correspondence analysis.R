##########################################################################
## 1. Impact of mgmt on local communities well-being in Madagascar      ##
##    1.2 AFC                                                           ##
##    03/08/18                                                          ##
##    Caroline BOUSQUET                                                 ##
##########################################################################

# clean the workspace
rm(lis=ls())

# load packages
library(FactoMineR)
library(factoextra)
library(gplots)
library(graphics)
library(corrplot) #visualiser les cos2

# load datas
#costs
afc1 <- as.table(rbind (c(17, 5, 3), c(5,2,4), c(3,2,3), c(1,4,1), c(26,7,11), c(0,0,1), c(1,0,0), c(0,1,1), c(1,3,5), c(1,0,0), c(0,1,1))) # Convertir les donn??es en tant que table
dimnames(afc1) <- list(categories=c("livelihoods", "economic wealth", "material wealth", "employment",
                                   "rights and access", "local institutions", "empowerment and agency", "participation",
                                   "food security", "emotional health",
                                   "social capital and cohesion"), life_satisfaction = c("Worse", "No change", "Better"))
afc1


#benef
afc2 <- as.table(rbind (c(78, 43, 73), c(13, 11, 24), c(3, 2, 9), c(1, 3, 1), c(57, 23, 24), c(1, 1, 4), c(0, 0, 1), c(16, 18, 33), c(1, 1, 2), c(1, 1, 2))) # Convertir les donn??es en tant que table
dimnames(afc2) <- list(categories=c("livelihoods", "economic wealth", "material wealth", "employment",
                                   "rights and access", "local institutions", "participation",
                                   "food security",
                                   "social capital and cohesion", "education and knowledge"), life_satisfaction = c("Worse", "No change", "Better"))

# graph FIG 12 and FIG 13
balloonplot(t (afc2), main = "Categories", xlab = "", ylab = "",
            label = F, show.margins = F)
chi.sq<-chisq.test(afc2)
chi.sq


#chisq costs*rights and access
costs_raa<-as.table(rbind(c(26, 7, 11)))
dimnames(costs_raa) <- list(cat=c("rights and access"), life_satisfaction = c("Worse", "No change", "Better"))
chisq.test(costs_raa)


res.ca <- CA (afc2, graph = FALSE)
print(res.ca)
chi.sq
# khi2 statistic
chi2 <- 27.823
# degree of freedom
df <- (nrow (afc2) - 1) * (ncol (afc2) - 1)
# p value
pval <- pchisq (chi2, df = df, lower.tail = FALSE)
pval
eig.val <- get_eigenvalue (res.ca)
eig.val # obtention valeurs propres (quantit?? d'info gard??e dans les axes)
fviz_screeplot (res.ca, addlabels = TRUE, ylim = c(0, 90)) #graphique des valeurs propres
fviz_screeplot (res.ca) + geom_hline (yintercept = 50, linetype = 2, color = "red")
fviz_ca_biplot (res.ca, repel=TRUE) #biplot rows + columns FIG 11a

## Rows
row <- get_ca_row(res.ca)
head(row$coord)
  # Cos2: graphic's quality
row$cos2
  # Colour according to the graphic's quality 
fviz_ca_row (res.ca, col.row = "cos2",
             gradient.cols = c ("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
  #cos2
corrplot(row$cos2, is.corr = FALSE)

  # Rows contributions to the dimensions 
row$contrib
corrplot(row$contrib, is.corr=FALSE) 
  # Rows contributions to dim 1
fviz_contrib(res.ca, choice = "row", axes = 1, top = 10)
  # Rows contributions to dim 2
fviz_contrib(res.ca, choice = "row", axes = 2, top = 10)
  # Rows contributions to dim 1 and 2
fviz_contrib (res.ca, choice = "row", axes = 1:2, top = 10)
  # Most important rows FIG 11b
fviz_ca_row (res.ca, col.row = "contrib",
             gradient.cols = c ("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)



## Columns
col <- get_ca_col(res.ca)
col$coord
fviz_ca_col (res.ca, col.col = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
  # Graphic's quality
col$cos2
fviz_cos2 (res.ca, choice = "col", axes = 1:2)
  # Contributions
col$contrib
fviz_contrib (res.ca, choice = "col", axes = 1:2)
fviz_ca_biplot (res.ca, map = "colgreen", arrow = c (TRUE, FALSE),
                repel = TRUE)
  #Description des dim
res.desc <- dimdesc(res.ca, axes = c(1, 2))
head(res.desc[[1]]$row, 4)

