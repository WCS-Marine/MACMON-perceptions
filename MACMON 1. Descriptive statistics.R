##########################################################################
## 1. Impact of mgmt on local communities well-being in Madagascar      ##
##    1.1 Descriptive statistics (radar charts)                         ##
##    19/07/18                                                          ##
##    Caroline BOUSQUET                                                 ##
##########################################################################

rm(lis=ls())

## Library
library(fmsb)


## Overall communities' benefits and costs 
#radar chart:
x<- c(42.4, 22.1, 11.1, 9.7, 3, 7.1, 1.3, 5.3, 23.8, 41.6, 1.5, 0.9, 
      0, 0.9, 0.2, 1.8, 15, 8, 0, 0.9, 0.9, 1.8, 0.9, 0)
data=as.data.frame(matrix(data=x , nrow=2, ncol=12), ncol=12)
colnames(data)=c("livelihoods" , "economic wealth" , "material wealth" , "employment" , "rights and access", "local institutions", "empowerment 
                 and agencies", "participation", "food security", "emotional 
                 health", "social capital 
                 and cohesion", "education and 
                 knowledge")
rownames(data)=c("benefits", "costs")
# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot
data=rbind(rep(100,2) , rep(0,5) , data)
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4))
radarchart( data  , axistype=1 , 
            #custom polygon
            pcol=colors_border, pfcol=colors_in,  plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,100,25), cglwd=0.5,
            #custom labels
            vlcex=0.65
            )
legend(x=1, y=1.3, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=0.8, pt.cex=1.5)

#histogram
economy_bc = c(34.1,9,2.4,1 , 0, 0, 0, 0, 0, 0, 0, 0)
governance_bc =c(0, 0, 0, 0, 19.1,1.2,0,0.2, 0, 0, 0, 0)
health_bc =c(0, 0, 0, 0, 0, 0, 0, 0, 12.1, 0, 0, 0)
social_bc =c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.7, 0.7)
economy_cc = c(4.3, 1.9, 1.4, 1, 0, 0, 0, 0, 0, 0, 0, 0)
governance_cc =c(0, 0, 0, 0, 8.1,0.2,0.2,0.3, 0, 0, 0, 0)
health_cc =c(0, 0, 0, 0, 0, 0, 0, 0, 1.6, 0.2, 0, 0)
social_cc =c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.3, 0)
couleurs_eco = c("gray87", "gray70", "gray50", "gray30" )
couleurs_gov = c("lightskyblue1", "steelblue1", "steelblue", "steelblue4")
couleurs_health = c("seagreen2","seagreen4")
couleurs_social =c("darkgoldenrod1", "darkgoldenrod4")
plot.new() 
par(mar = c(3,3,3,0)) #down,left,up, right
data_mc = cbind(economy_bc, economy_cc, governance_bc, governance_cc, health_bc, health_cc, social_bc, social_cc)
couleurs_compil = c(couleurs_eco, couleurs_gov, couleurs_health, couleurs_social)
barplot(height = data_mc,
        space=1,col=couleurs_compil, ylim=c(0,60),
        names.arg = c("economy","", "governance","", "health", "", "social", "")
        )




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

