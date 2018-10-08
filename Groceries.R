#-------------------------------’κσηση 1--------------------------------------------------------#

groceries<-read.csv("C:/Users/user/Desktop/GroceriesInitial.csv",header=TRUE,sep=",")
product_names<-levels(unlist(groceries[,4:35]))
c<-which(product_names == "citrus fruit")
tr <- which(product_names == "tropical fruit")
wm <- which(product_names == "whole milk")
ov <- which(product_names == "other vegetables")
rb <- which(product_names == "rolls/buns")
ch <- which(product_names == "chocolate")
bw <- which(product_names == "bottled water")
y <- which(product_names == "yogurt")
s <- which(product_names == "sausage")
rv <- which(product_names == "root vegetables")
pa <- which(product_names == "pastry")
so <- which(product_names == "soda")
cr <- which(product_names == "cream")
product_names<-product_names[c(c,tr,wm,ov,rb,ch,bw,y,s,rv,pa,so,cr)]
products<-as.data.frame(t(apply(groceries[,4:35],1,function(x)
  (product_names)%in%as.character(unlist(x)))))
names(products)<-product_names

groceries_binary<-cbind(groceries[,1:3],products)
str(groceries_binary)

groceries_discrete<-groceries_binary
cut_points<-quantile(groceries_discrete$basket_value
                     ,probs=c(0,0.33,0.66,1)
                     ,na.rm=TRUE
                     ,names=FALSE)
groceries_discrete$basket_value_bin<-cut(groceries_discrete$basket_value
  ,breaks= cut_points,labels=c("low_value_basket","medium_value_basket","high_value_basket"),include.lowest=TRUE)
table(groceries_discrete$basket_value_bin)
str(groceries_discrete)

binarize<-function(data_columns,extra_columns=NULL){
  column_names<-levels(unlist(data_columns))
  blank<-which(column_names=="")
  if(length(blank)!=0)
    column_names<-column_names[-c(blank)]
  binary_result<-as.data.frame(t(apply(data_columns,1
      ,function(x) column_names%in%as.character(unlist(x)))))
  names(binary_result)<-column_names
  if(is.null(extra_columns)==FALSE)
    binary_result<-cbind(extra_columns,binary_result)
  return(binary_result)
}

groceries_discrete<-
  binarize(as.data.frame(groceries_discrete$basket_value_bin),groceries_discrete)
groceries_discrete<-
  groceries_discrete[,-c(which(colnames(groceries_discrete)=="basket_value_bin"))]
str(groceries_discrete)
#-------------------------------’κσηση 2--------------------------------------------------------#


#install.packages("arules")
library(arules)
#-------------------------------α' σκέλος-------------------------------------------------------#
rules<-apriori(groceries_discrete[,4:ncol(groceries_discrete)]
               ,parameter=list(minlen=2,supp=0.001,conf=1)
               ,control=list(verbose=FALSE))

#inspect(rules[])      #Εμφάνιση όλων των κανόνων
inspect(rules[1:20])   #Εμφάνιση μόνο των πρώτων είκοσι κανόνων


rules<-apriori(groceries_discrete[,4:ncol(groceries_discrete)]     #----Αύξηση του κατώτατου support---#
               ,parameter=list(minlen=2,supp=0.002,conf=1)
               ,control=list(verbose=FALSE))
#inspect(rules[])      #Εμφάνιση όλων των κανόνων
inspect(rules[1:20])   #Εμφάνιση μόνο των πρώτων είκοσι κανόνων

rules<-apriori(groceries_discrete[,4:ncol(groceries_discrete)]     #----Μείωση του κατώτατου support---#
               ,parameter=list(minlen=2,supp=0.0002,conf=1)
               ,control=list(verbose=FALSE))
#inspect(rules[])      #Εμφάνιση όλων των κανόνων
inspect(rules[1:20])   #Εμφάνιση μόνο των πρώτων είκοσι κανόνων
#-------------------β' σκέλος--------------------------#
rules<-apriori(groceries_discrete[,4:16]    
               ,parameter=list(minlen=2,supp=0.0002,conf=1)
               ,control=list(verbose=FALSE))
rules <- sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:20])

#-------------------γ' σκέλος--------------------------#

rules<-apriori(groceries_discrete[,4:19]    
               ,parameter=list(minlen=2,supp=0.0002,conf=1)
               ,control=list(verbose=FALSE))

rules <- sort(rules, decreasing=TRUE,by=c("confidence","support"))
inspect(rules[1:20])

#-------------------------------’κσηση 3--------------------------------------------------------#


library(cluster)
#-------------------------------α' σκέλος-------------------------------------------------------#
#groceries_discrete <- scale(groceries_discrete, center = TRUE, scale = TRUE)   #Εάν θέλουμε μπορούμε να κανονικοποίησουμε τα δεδομένα
set.seed(1234)
grocerCluster <- kmeans(groceries_discrete[, 2:3], 5, nstart = 25)
grocerCluster

#-------------------------------β' σκέλος-------------------------------------------------------#
str(grocerCluster)
grocerCluster$centers

#-------------------------------γ' σκέλος-------------------------------------------------------#


Cluster = grocerCluster$cluster
str(Cluster)
groceries_discrete$Cluster=Cluster
str(groceries_discrete)
mC=matrix(Cluster,ncol = 1)
mC
#members <- as.data.frame(t(apply(mC,1, function(x)
 # (mC) %in% as.character(unlist(x)))))
#members
#names(members) <- cluster_members
#clusters_binary <- cbind(Cluster[],members)
#str(clusters_binary[])


#clusters_discrete<-cluster_binary
cut_points<-quantile(mC
                     ,probs=c(0,0.33,0.66,1)
                     ,na.rm=TRUE
                     ,names=FALSE)
mC<-cut(mC
                       ,breaks= cut_points,labels=c("Cluster1","Cluster2","Cluster3"),include.lowest=TRUE)
table(mC)
mC

binarize<-function(data_columns,extra_columns=NULL){
  column_names<-levels(unlist(data_columns))
  blank<-which(column_names=="")
  if(length(blank)!=0)
    column_names<-column_names[-c(blank)]
  binary_result<-as.data.frame(t(apply(data_columns,1
                                       ,function(x) column_names%in%as.character(unlist(x)))))
  names(binary_result)<-column_names
  if(is.null(extra_columns)==FALSE)
    binary_result<-cbind(extra_columns,binary_result)
  return(binary_result)
}

mC<-
  binarize(as.data.frame(mC),mC)
mC<-
   mC[,-c(which(colnames(mC)=="basket_value_bin"))]
str(mC)
mC

#-------------------------------’κσηση 4--------------------------------------------------------#

library(arules)
groc_clustered<-read.csv("C:/Users/user/Desktop/GroceriesClustered.csv",header=TRUE,sep=",")
str(groc_clustered)

selVar = groc_clustered[,c(2:14,17:21)]

cluster_rules<-apriori(selVar
               ,parameter=list(minlen=2,supp=0.1,conf=1,maxlen=30,maxtime=50)
               ,control=list(verbose=FALSE))
cluster_rules <- sort(cluster_rules, decreasing=TRUE,by="confidence")
inspect(cluster_rules[1:20])




