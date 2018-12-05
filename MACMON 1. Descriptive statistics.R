##########################################################################
## 1. Impact of mgmt on local communities well-being in Madagascar      ##
##    1.1 Descriptive statistics (radar charts)                         ##
##    19/07/18                                                          ##
##    Caroline BOUSQUET  
##    updated Steph D'agata - Dec 2018
##########################################################################

rm(list=ls())

## Library
library(fmsb)
library(dplyr)
library(plyr)

# load file for cost/benefits management
rm(CB)
CB <- read.csv("data/mgmt_commu_1.csv",header=T,sep=";")
head(CB)
summary(CB)
dim(CB)


# rename factor level for domain
CB[,"mgmt_community.benefits_domain_1"] <- revalue(CB[,"mgmt_community.benefits_domain_1"], c("economic"="economy"))
                                                                                               
# rename factor level for catgeories
CB[,"mgmt_community.benefits_category_1"] <- revalue(CB[,"mgmt_community.benefits_category_1"], c("economic wealth"="economic_wealth",
                                                 "education and knowledge"="education_knowledge",
                                                 "food security" = "food_security",
                                                 "local institutions" = "local_institutions",
                                                 "material wealth" = "material_wealth",
                                                 "rights and access" = "rights_access",
                                                 "social capital and cohesion"="socialcapital_cohesion"))

CB[,"mgmt_community.costs_category_1"] <- revalue(CB[,"mgmt_community.costs_category_1"], c("economic wealth"="economic_wealth",
                                                                                                  "food security" = "food_security",
                                                                                                  "local institutions" = "local_institutions",
                                                                                                  "material wealth" = "material_wealth",
                                                                                                  "rights and access" = "rights_access",
                                                                                                  "social capital and cohesion"="socialcapital_cohesion",
                                                                                                  "empowerment and agency" = "empowerment_agency"))


## Overall communities' benefits and costs
  # summarize management benefits by categories
rm(data.CB)
data.CB=as.data.frame(matrix(data=NA , nrow=2, ncol=12), ncol=12)
colnames(data.CB)=c(levels(CB[,"mgmt_community.benefits_category_1"]),"emotional","empowerment_agency")
rownames(data.CB)=c("benefits", "costs")
head(data.CB)


 # fill benefits
  # number of respondants to compute % for each benefits
  n.resp.ben <- length(CB$mgmt_community.benefits_category_1) - which(is.na(CB$mgmt_community.benefits_category_1)) %>% length()
  n.resp.ben
  
  ben.col <- which(colnames(data.CB) %in% names(summary(CB$mgmt_community.benefits_category_1))[-11])
  data.CB[1,ben.col] <- round(100*summary(CB$mgmt_community.benefits_category_1)[-11]/n.resp.ben,1)
  head(data.CB)

  # fill costs
  # number of respondants to compute % for each costs
  n.resp.cost <- length(CB$mgmt_community.costs_category_1) - which(is.na(CB$mgmt_community.costs_category_1)) %>% length()
  n.resp.cost
  
  n.resp <- n.resp.cost + n.resp.ben
  
  cost.col <- which(colnames(data.CB) %in% names(summary(CB$mgmt_community.costs_category_1))[-12])
  data.CB[2,cost.col] <- round(100*summary(CB$mgmt_community.costs_category_1)[-12]/n.resp.cost,1)

  # file NAs with 0
  data.CB[is.na(data.CB)] <- 0
  head(data.CB)
#radar chart:

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot
data.CB=rbind(rep(100,2) , rep(0,5) , data.CB)
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4))
vlabels <- c("economic wealth","education and knowledge","employment","food security",  
             "livelihoods", "local institutions", "material wealth", "participation",         
             "rights and access", "social capital and cohesion", "emotional", "empowerment and agency")
  
# save radar chart for cost and benefits
pdf("figs/MD_Radarchart_cost_ben_total.pdf")
radarchart( data.CB  , axistype=1 , 
            #custom polygon
            pcol=colors_border, pfcol=colors_in,  plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,100,25), cglwd=0.5,
            #custom labels
            vlcex=0.65,
            # custom names
            vlabels = vlabels)
legend(x=1, y=1.3, legend = rownames(data.CB[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=0.8, pt.cex=1.5)
dev.off()

#histogram with superimpose barplot for each domain of cost/benefits

    # create data.frame to merge domain and categories
    rm(data.DC)
    data.DC <-matrix(nrow=12,ncol=8,NA)
    colnames(data.DC) <- c("eco_ben","eco_cost","govern_ben","govern_cost","health_ben","health_cost","social_ben","social_cost")
    rownames(data.DC) <- c("livelihoods" , "economic_wealth" , "material_wealth" , "employment" , "rights_access", "local_institutions", 
                          "participation", "empowerment_agency","food_security","emotional","socialcapital_cohesion", "education_knowledge")
    head(data.DC)
    tail(data.DC)
      # eco benefits
      eco.ben <- which(CB$mgmt_community.benefits_domain_1 == "economy")
      eco.ben.perc <- sort(100*round(summary(CB$mgmt_community.benefits_category_1[eco.ben])/n.resp,3),decreasing=T)
      eco.ben.perc <- eco.ben.perc[rownames(data.DC)]
      data.DC[,"eco_ben"] <- eco.ben.perc 

      # governance benefits
      gov.ben <- which(CB$mgmt_community.benefits_domain_1 == "governance")
      gov.ben.perc <- 100*round(summary(CB$mgmt_community.benefits_category_1[gov.ben])/n.resp,3)
      gov.ben.perc <- gov.ben.perc[rownames(data.DC)]
      data.DC[,"govern_ben"] <- gov.ben.perc
      
      # health benefits
      health.ben <- which(CB$mgmt_community.benefits_domain_1 == "health")
      health.ben.perc <- 100*round(summary(CB$mgmt_community.benefits_category_1[health.ben])/n.resp,3)
      health.ben.perc <- health.ben.perc[rownames(data.DC)]
      data.DC[,"health_ben"] <- health.ben.perc
      
      # social benefits
      social.ben <- which(CB$mgmt_community.benefits_domain_1 == "social")
      social.ben.perc <- 100*round(summary(CB$mgmt_community.benefits_category_1[social.ben])/n.resp,3)
      social.ben.perc <- social.ben.perc[rownames(data.DC)]
      data.DC[,"social_ben"] <- social.ben.perc
      
      # eco costs
      eco.cost <- which(CB$mgmt_community.costs_domain_1 == "economy")
      eco.cost.perc <- sort(100*round(summary(CB$mgmt_community.costs_category_1[eco.cost])/n.resp,3),decreasing=T)
      eco.cost.perc <- eco.cost.perc[rownames(data.DC)]
      data.DC[,"eco_cost"] <- eco.cost.perc 
      
      # governance costefits
      gov.cost <- which(CB$mgmt_community.costs_domain_1 == "governance")
      gov.cost.perc <- 100*round(summary(CB$mgmt_community.costs_category_1[gov.cost])/n.resp,3)
      gov.cost.perc <- gov.cost.perc[rownames(data.DC)]
      data.DC[,"govern_cost"] <- gov.cost.perc
      
      # health costefits
      health.cost <- which(CB$mgmt_community.costs_domain_1 == "health")
      health.cost.perc <- 100*round(summary(CB$mgmt_community.costs_category_1[health.cost])/n.resp,3)
      health.cost.perc <- health.cost.perc[rownames(data.DC)]
      data.DC[,"health_cost"] <- health.cost.perc
      
      # social costefits
      social.cost <- which(CB$mgmt_community.costs_domain_1 == "social")
      social.cost.perc <- 100*round(summary(CB$mgmt_community.costs_category_1[social.cost])/n.resp,3)
      social.cost.perc <- social.cost.perc[rownames(data.DC)]
      data.DC[,"social_cost"] <- social.cost.perc
      
      # fill NAs with 0
      data.DC[is.na(data.DC)] <- 0
      
  # colors for each domain and category
couleurs_eco = c("gray87", "gray70", "gray50", "gray30" )
couleurs_gov = c("lightskyblue1", "steelblue1", "steelblue","steelblue2")
couleurs_health = c("seagreen2","seagreen4")
couleurs_social =c("darkgoldenrod1", "darkgoldenrod4")

  # barplot
pdf("figs/Barplot_Domain_cat_cost_benefits.pdf",width=12)
vlabels.1 <- c("livelihoods","economic wealth", "material wealth","employment",
               "rights and access","local institutions","participation","empowerment and agency",
               "food security","emotional health","social capital and cohesion","education and knowledge") 
      

par(mar = c(3,5,3,0)) #down,left,up, right
couleurs_compil = c(couleurs_eco, couleurs_gov, couleurs_health, couleurs_social)
barplot(height = data.DC,
        space=0.25,col=couleurs_compil, ylim=c(0,60),
        names.arg = c("Economy","", "Governance","", "Health", "", "Social", ""),
        legend.text = vlabels.1,ylab="Percentage Respondents (%)"
        )
text(c(0.8,3.2,5.7,8.25),c(50,25,15,5),"Benefits")
text(c(2,4.5,7,9.5),c(12,12,5,5),"Costs")
dev.off()


### to be continued
## Communities' benefits and costs according to livelihoods categories
# benefits :
x<- c(31.9,0.9,0.2,1.1, 7.3, 1.1,0.9,1.3, 2.6,0.2,0,0, 0,0.2,0,1.1, 21.2,0.9,0.2,0.4, 0.4,0.2,0.6,0.2, 0.2,0,0,0, 2.8,4.5,2.6,1.9, 0.6,0,0,0.2, 0.9,0,0,0)
data=as.data.frame(matrix(data=x , nrow=4, ncol=10), ncol=10)
colnames(data)=c("livelihoods" , "economic wealth" , "material wealth" , "employment" , "rights and access", "local institutions", "participation", "food security","social capital 
                 and cohesion", "education and 
                 knowledge")
rownames(data)=c("Fishing", "Fishing Related", "Agriculture", "Other")
data=rbind(rep(40,2) , rep(0,5) , data)
colors_border=c( rgb(0.2,0.5,0.8,0.8), rgb(0.2,0.5,0.5,0.8), rgb(0.8,0.5,0.2,0.8), rgb(0.5,0.5,0.5,0.8) )
radarchart( data  , axistype=1 , 
            #custom polygon
            pcol=colors_border,  plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,40,10), cglwd=0.5,
            #custom labels
            vlcex=0.65
            )
legend(x=1, y=1.3, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_border , text.col = "grey", cex=0.8, pt.cex=1.5)

#costs :
x<- c(18.6,0,1.8,1.8, 5.3,1.8,1.8,0.9, 5.3,1.8,0,0, 3.5,0,0,1.8, 38.1,0.9,0.9,0.9, 0,0,0.9,0, 0.9,0,0,0.9, 0,0,1.8,1.8, 0.9,0,0,0, 1.8,0,0,0)
data=as.data.frame(matrix(data=x , nrow=4, ncol=10), ncol=10)
colnames(data)=c("livelihoods" , "economic wealth" , "material wealth" , "employment" , "rights and access", "local institutions", "participation", "food security", "emotional health", "social capital 
                 and cohesion")
rownames(data)=c("Fishing", "Fishing Related", "Agriculture", "Other")
data=rbind(rep(40,2) , rep(0,5) , data)
colors_border=c( rgb(0.2,0.5,0.8,0.8), rgb(0.2,0.5,0.5,0.8), rgb(0.8,0.5,0.2,0.8), rgb(0.5,0.5,0.5,0.8) )
radarchart( data  , axistype=1 , 
            #custom polygon
            pcol=colors_border,  plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,40,10), cglwd=0.5,
            #custom labels
            vlcex=0.65
            )
legend(x=1, y=1.3, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_border , text.col = "grey", cex=0.8, pt.cex=1.5)




## Commu benefits and costs according to gender
# benefits 
x<- c(29.8, 12.6, 6.4, 4.7, 1.9, 0.9, 0.9, 0.4, 15.4, 8.4, 1.3, 0.2, 0.2, 0, 7.9, 7.1, 0.4, 0.4, 0.9, 0)
data=as.data.frame(matrix(data=x , nrow=2, ncol=10), ncol=10)
colnames(data)=c("livelihoods" , "economic wealth" , "material wealth" , "employment" , "rights and access", "local institutions", "participation", "food security","social capital 
                 and cohesion", "education and 
                 knowledge")
rownames(data)=c("men", "women")
data=rbind(rep(100,2) , rep(0,5) , data)
colors_border=c( rgb(0.3,0.5,0.6,0.8), rgb(0.8,0.3,0.5,0.8) )
colors_in=c( rgb(0.3,0.5,0.6,0.2), rgb(0.8,0.3,0.5,0.2))
radarchart( data  , axistype=1 , 
            #custom polygon
            pcol=colors_border, pfcol=colors_in,  plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,100,25), cglwd=0.5,
            #custom labels
            vlcex=0.65
            )
legend(x=1, y=1.3, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_border , text.col = "grey", cex=0.8, pt.cex=1.5)

#costs
x<- c(14.2, 7.1, 8, 1.8, 4.4, 2.7, 2.7, 2.7, 30.1, 11.5, 0, 0.9, 0.9, 0, 1.8, 0, 2.7, 5.3, 0, 0.9, 1.8, 0 )
data=as.data.frame(matrix(data=x , nrow=2, ncol=11), ncol=11)
colnames(data)=c("livelihoods" , "economic wealth" , "material wealth" , "employment" , "rights and access", "local institutions", "empowerment
                 an agencies", "participation", "food security", "emotional health", "social capital 
                 and cohesion")
rownames(data)=c("men", "women")
data=rbind(rep(100,2) , rep(0,5) , data)
colors_border=c( rgb(0.3,0.5,0.6,0.8), rgb(0.8,0.3,0.5,0.8) )
colors_in=c( rgb(0.3,0.5,0.6,0.2), rgb(0.8,0.3,0.5,0.2))
radarchart( data  , axistype=1 , 
            #custom polygon
            pcol=colors_border, pfcol=colors_in,  plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,100,25), cglwd=0.5,
            #custom labels
            vlcex=0.65
            )
legend(x=1, y=1.3, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_border , text.col = "grey", cex=0.8, pt.cex=1.5)




## Commu benefits and costs according to type of mgmt
# benefits 
x<- c(1.1, 20.8, 20.6, 1.9, 4.3, 4.9, 0, 0.9, 2.1, 0, 0.9, 0.4, 1.1, 11.3, 11.3, 0.4, 0.4, 0.6, 0, 0.2, 0, 6, 3.9, 5.1, 0, 0.6, 0, 0, 0.6, 0.2)
data=as.data.frame(matrix(data=x , nrow=3, ncol=10), ncol=10)
colnames(data)=c("livelihoods" , "economic wealth" , "material wealth" , "employment" , "rights and access", "local institutions", "participation", "food security","social capital 
                 and cohesion", "education and 
                 knowledge")
rownames(data)=c("LMMA", "MPA", "No management")
data=rbind(rep(25,2) , rep(0,5) , data)
colors_border=c( rgb(0.2,0.5,0.5,0.8), rgb(0.8,0.5,0.2,0.8), rgb(0.1,0.5,0.8,0.8)  )
colors_in=c( rgb(0.2,0.5,0.5,0.2), rgb(0.8,0.5,0.2,0.2), rgb(0.1,0.5,0.8,0.2))
radarchart( data  , axistype=1 , 
            #custom polygon
            pcol=colors_border, plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.5,
            #custom labels
            vlcex=0.65
            )
legend(x=1, y=1.3, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_border , text.col = "grey", cex=0.8, pt.cex=1.5)

#costs
x<- c(0, 11.5, 10.6, 0.9, 5.3, 3.5, 0, 2.7, 4.4, 0, 1.8, 3.5, 1.8, 23, 16.8, 0.9, 0, 0, 0, 0.9, 0, 0.9, 0, 0.9, 7.1, 0.9, 0, 0, 0.9, 0, 0, 1.8, 0)
data=as.data.frame(matrix(data=x , nrow=3, ncol=11), ncol=11)
colnames(data)=c("livelihoods" , "economic wealth" , "material wealth" , "employment" , "rights and access", "local institutions", "empowerment 
                 and agencies", "participation", "food security", "emotional health", "social capital 
                 and cohesion")
rownames(data)=c("LMMA", "MPA", "No management")
data
data=rbind(rep(25,2) , rep(0,5) , data)
colors_border=c( rgb(0.2,0.5,0.5,0.8), rgb(0.8,0.5,0.2,0.8), rgb(0.1,0.5,0.8,0.8)  )
colors_in=c( rgb(0.2,0.5,0.5,0.2), rgb(0.8,0.5,0.2,0.2), rgb(0.1,0.5,0.8,0.2))
radarchart( data  , axistype=1 , 
            #custom polygon
            pcol=colors_border, plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.5,
            #custom labels
            vlcex=0.65
)
legend(x=1, y=1.3, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_border , text.col = "grey", cex=0.8, pt.cex=1.5)




#### Commu benefits and costs according to education
# benefits 
x<- c(27, 10.1, 0, 6, 3, 1.3, 1.5, 0.4, 0.4, 0.2, 0.9, 0, 14.3, 6, 0.9, 0.9, 0.2, 0, 0, 0, 0.2, 6.4, 6.2, 1.1, 0.6, 0.2, 0, 0.4, 0, 0.2)
data=as.data.frame(matrix(data=x , nrow=3, ncol=10), ncol=10)
colnames(data)=c("livelihoods" , "economic wealth" , "material wealth" , "employment" , "rights and access", "local institutions", "participation", "food security","social capital 
                 and cohesion", "education and 
                 knowledge")
rownames(data)=c("Primary school", "Middle school", "Secondary school and more")
data
data=rbind(rep(40,2) , rep(0,5) , data)
colors_border=c( rgb(0.2,0.5,0.5,0.8), rgb(0.8,0.5,0.2,0.8), rgb(0.1,0.5,0.8,0.8)  )
colors_in=c( rgb(0.2,0.5,0.5,0.2), rgb(0.8,0.5,0.2,0.2), rgb(0.1,0.5,0.8,0.2))
radarchart( data  , axistype=1 , 
            #custom polygon
            pcol=colors_border, plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,40,10), cglwd=0.5,
            #custom labels
            vlcex=0.65
            )
legend(x=1, y=1.3, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_border , text.col = "grey", cex=0.8, pt.cex=1.5)

#costs
x<- c(14.2, 1.8, 0, 6.2, 3.5, 0, 3.5, 1.8, 0, 4.4, 0, 0, 30.1, 8, 0.9, 0, 0.9, 0, 0.9, 0, 0, 0, 0.9, 0.9, 3.5, 3.5, 0, 1.8, 0, 0)
data=as.data.frame(matrix(data=x , nrow=3, ncol=10), ncol=10)
colnames(data)=c("livelihoods" , "economic wealth" , "material wealth" , "employment" , "rights and access", "local institutions", "empowerment 
                 and agencies", "participation", "food security", "social capital 
                 and cohesion")
rownames(data)=c("Primary school", "Middle school", "Secondary school and more")
data
data=rbind(rep(40,2) , rep(0,5) , data)
colors_border=c( rgb(0.2,0.5,0.5,0.8), rgb(0.8,0.5,0.2,0.8), rgb(0.1,0.5,0.8,0.8)  )
colors_in=c( rgb(0.2,0.5,0.5,0.2), rgb(0.8,0.5,0.2,0.2), rgb(0.1,0.5,0.8,0.2))
radarchart( data  , axistype=1 , 
            #custom polygon
            pcol=colors_border, plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,40,10), cglwd=0.5,
            #custom labels
            vlcex=0.65
            )
legend(x=1, y=1.3, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_border , text.col = "grey", cex=0.8, pt.cex=1.5)




## Commu benefits and costs according to participation
# benefits 
x<- c(0.6, 20.3, 11.3, 9.9, 0.9, 5.1, 1.9, 2.6, 0, 1.9, 0.6, 0.4, 0, 0.4, 0, 0.6,
      0.4, 9.6, 7.1, 6.2, 0, 0.2, 0.6, 0.2, 0, 0, 0.2, 0, 0.6, 4.9, 2.1, 7.3, 0, 0.2, 0.2, 0.4,
      0, 0.6, 0.2, 0)
data=as.data.frame(matrix(data=x , nrow=4, ncol=10), ncol=10)
colnames(data)=c("livelihoods" , "economic wealth" , "material wealth" , "employment" , "rights and access", "local institutions", "participation", "food security","social capital 
                 and cohesion", "education and 
                 knowledge")
rownames(data)=c("leader", "active", "passive", "not involved")
data
data=rbind(rep(20,2) , rep(0,5) , data)
colors_border=c( rgb(0.2,0.5,0.5,0.8), rgb(0.8,0.5,0.2,0.8), rgb(0.1,0.5,0.8,0.8), rgb(0.5,0.5,0.5,0.6)  )
radarchart( data  , axistype=1 , 
            #custom polygon
            pcol=colors_border, plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.5,
            #custom labels
            vlcex=0.65)
legend(x=1, y=1.3, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_border , text.col = "grey", cex=0.8, pt.cex=1.5)

#costs
x<- c(0, 8.8, 5.3, 8, 0, 4.4, 3.5, 1.8, 0, 5.3, 0, 1.8, 0, 1.8, 1.8, 1.8, 3.5, 16.8, 11.5, 9.7, 0, 0, 0, 0.9, 0, 0.9, 0, 0, 0, 0.9, 0, 0.9,
      0, 0, 2.7, 5.3, 0, 0, 0, 0.9, 0, 0.9, 0.9, 0)
data=as.data.frame(matrix(data=x , nrow=4, ncol=11), ncol=11)
colnames(data)=c("livelihoods" , "economic wealth" , "material wealth" , "employment" , "rights and access", "local institutions", "empowerment
                 and agencies", "participation", "food security","emotional health", "social capital 
                 and cohesion")
rownames(data)=c("leader", "active", "passive", "not involved")
data
data=rbind(rep(20,2) , rep(0,5) , data)
colors_border=c( rgb(0.2,0.5,0.5,0.8), rgb(0.8,0.5,0.2,0.8), rgb(0.1,0.5,0.8,0.8), rgb(0.5,0.5,0.5,0.6)  )
colors_in=c( rgb(0.2,0.5,0.5,0.2), rgb(0.8,0.5,0.2,0.2), rgb(0.1,0.5,0.8,0.2), rgb(0.5,0.5,0.5,0.2))
radarchart( data  , axistype=1 , 
            #custom polygon
            pcol=colors_border, plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.5,
            #custom labels
            vlcex=0.65
            )
legend(x=1, y=1.3, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_border , text.col = "grey", cex=0.8, pt.cex=1.5)




#### Commu benefits and costs according to the age category
# benefits 
x<- c(13.5, 10.7, 9, 7.7, 2.4, 3.6, 3, 2.1, 0.6, 0.9, 0.4, 1.1, 0.6, 0.6, 0, 0.2, 6, 6.6, 5.8, 5.4, 0.9, 0, 0.4, 0.2, 0.2, 0, 0, 0, 3.4, 5.4, 3.2, 2.8, 0, 0.2, 0.2, 0, 0, 0, 0.4, 0.4)
data=as.data.frame(matrix(data=x , nrow=4, ncol=10), ncol=10)
colnames(data)=c("livelihoods" , "economic wealth" , "material wealth" , "employment" , "rights and access", "local institutions", "participation", "food security","social capital 
                 and cohesion", "education and 
                 knowledge")
rownames(data)=c("18-28", "29-36", "37-47", "Over 47")
data=rbind(rep(20,5) , rep(0,5) , data)
colors_border=c( rgb(0.8,0.5,0.2,0.8), rgb(0.1,0.5,0.8,0.8), rgb(0.2,0.5,0.5,0.8),  rgb(0.5,0.5,0.5,0.6) )
radarchart( data  , axistype=1 , 
            #custom polygon
            pcol=colors_border, plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.5,
            #custom labels
            vlcex=0.65
)
legend(x=1, y=1.3, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_border , text.col = "grey", cex=0.8, pt.cex=1.5)

#costs
x<- c(6.2, 5.3, 3.5, 7.1, 3.5, 1.8, 0.9, 3.5, 0.9, 2.7, 1.8, 1.8, 0.9, 2.7, 0, 1.8,
      12.4, 8, 9.7, 10.6, 0.9, 0, 0, 0, 0, 0, 0.9, 0, 0.9, 0.9, 0, 0,
      2.7, 1.8, 1.8, 1.8, 0, 0, 0.9, 0,
      0.9, 0, 0.9, 0)
data=as.data.frame(matrix(data=x , nrow=4, ncol=11), ncol=11)
colnames(data)=c("livelihoods" , "economic wealth" , "material wealth" , "employment" , "rights and access", "local institutions", "empowerment
                 and agency", "participation", "food security","social capital 
                 and cohesion", "education and 
                 knowledge")
rownames(data)=c("18-28", "29-36", "37-47", "Over 47")
data=rbind(rep(20,5) , rep(0,5) , data)
colors_border=c( rgb(0.8,0.5,0.2,0.8), rgb(0.1,0.5,0.8,0.8), rgb(0.2,0.5,0.5,0.8),  rgb(0.5,0.5,0.5,0.6) )
radarchart( data  , axistype=1 , 
            #custom polygon
            pcol=colors_border, plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.5,
            #custom labels
            vlcex=0.65
)
legend(x=1, y=1.3, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_border , text.col = "grey", cex=0.8, pt.cex=1.5)


















#### Commu benefits and costs according to WCS presence
# benefits 
x<- c(13.1, 15, 14.3, 3.9, 4.1, 3.2, 1.9, 0.2, 0.9, 0.2, 0.4, 0.6, 6.9, 8.1, 8.8, 0.6, 0.2, 0.6, 0, 0, 0.2, 4.5, 4.3, 6.2, 0, 0.6, 0.2, 0.2, 0.2, 0.4)
data=as.data.frame(matrix(data=x , nrow=3, ncol=10), ncol=10)
colnames(data)=c("livelihoods" , "economic wealth" , "material wealth" , "employment" , "rights and access", "local institutions", "participation", "food security","social capital 
                 and cohesion", "education and 
                 knowledge")
rownames(data)=c("No WCS", "Far WCS", "Close WCS")
data
data=rbind(rep(20,2) , rep(0,5) , data)
colors_border=c( rgb(0.8,0.5,0.2,0.8), rgb(0.1,0.5,0.8,0.8), rgb(0.2,0.5,0.5,0.8))
radarchart( data  , axistype=1 , 
            #custom polygon
            pcol=colors_border, plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,40,10), cglwd=0.5,
            #custom labels
            vlcex=0.65
)
legend(x=1, y=1.3, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_border , text.col = "grey", cex=0.8, pt.cex=1.5)

#costs
x<- c(7.1, 8, 7.1, 0.9, 4.4, 4.4, 4.4, 0, 2.7, 2.7, 2.7, 0, 8.8, 14.2, 18.6, 0, 0, 0.9, 0, 0.9, 0, 0.9, 0, 0.9, 0.9, 4.4, 2.7, 0, 0, 0.9, 0, 0.9, 0.9)
data=as.data.frame(matrix(data=x , nrow=3, ncol=11), ncol=11)
colnames(data)=c("livelihoods" , "economic wealth" , "material wealth" , "employment" , "rights and access", "local institutions", "empowerment 
                 and agencies", "participation", "food security", "emotional health", "social capital 
                 and cohesion")
rownames(data)=c("No WCS", "Far WCS", "Close WCS")
data
data=rbind(rep(40,2) , rep(0,5) , data)
colors_border=c( rgb(0.8,0.5,0.2,0.8), rgb(0.1,0.5,0.8,0.8), rgb(0.2,0.5,0.5,0.8)  )
radarchart( data  , axistype=1 , 
            #custom polygon
            pcol=colors_border, plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,40,10), cglwd=0.5,
            #custom labels
            vlcex=0.65
)
legend(x=1, y=1.3, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_border , text.col = "grey", cex=0.8, pt.cex=1.5)

