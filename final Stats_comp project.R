# the mode function

ModeVal1 = function(v){
  a= data.frame(table(v))
  b= a$v[a$Freq == max(a$Freq)]
  
  c= as.character(b)
  n= as.numeric(c)
  
  type = ""
  len = length(n)
  
  if(len ==1){
    type = "(unimodal)"
  }
  else if(len == 2){
    type = "(bimodal)"
  }
  else if(len == 3){
    type = "(trimodal)"
  }
  
  
  if(max(a$Freq) == 1){
    if(length(a$v)==1){
      return(paste(c,type))
    }
    else{ 
      return("no mode")
    }
  }
  else{
    return(paste(c,type))
  }
}

# data set work:

chickwts
chickwts.copy = chickwts

# reg=lm(chickwts.copy$weight~chickwts.copy$feed)
# summary(reg)
# # p value<alpha => regression model exists
# # model = y=323.583 -163.383x1-104.833x2-46.674x3-77.155x4+5.33x5
# # x1=horsebean , x2 = linseed , x3 = meatmeal , x4=soybean, x5 = sunflower
# # sunflower's and meatmeal's p values indicate they may not affect the model, as well as
# # the adjusted R^2 being too low indicating the model is weak
# chk2 = chickwts.copy[chickwts.copy$feed!="sunflower",]
# reg=lm(chk2$weight~chk2$feed)
# summary(reg)
# # Rsq=0.4937
# chk3 = chk2[chk2$feed!="meatmeal",]
# reg=lm(chk3$weight~chk3$feed)
# summary(reg)
# # Rsq=0.5468
# chk4 = chk3[chk3$feed!="soybean",]
# reg=lm(chk4$weight~chk4$feed)
# summary(reg)
# # Rsq=0.6317
# chk5 = chk4[chk4$feed!="linseed",]
# reg=lm(chk5$weight~chk5$feed)
# summary(reg)
# # Rsq=0.7113 : may no longer remove data, may indicate that the effect of the other feeds on weight is not significant compared to this type
# # new model: weight = 323.58-163.38x1      

anova1 = aov(chickwts.copy$weight~chickwts.copy$feed , data = chickwts.copy)
summary(anova1)
# p value = 5.94e-10 which is less than alpha therefore reject H0 and accept Ha
# conclusion: the types of feeds affect the weight of the chick
# model: ANOVA, since: there are more than 2 samples, and since the regression model is very weak
x=resid(anova1)
y=fitted(anova1)
plot(x,y)

horsebean = chickwts.copy$weight [chickwts.copy$feed == "horsebean"]
linseed = chickwts.copy$weight [chickwts.copy$feed == "linseed"]
soybean = chickwts.copy$weight [chickwts.copy$feed == "soybean"]
sunflower = chickwts.copy$weight [chickwts.copy$feed == "sunflower"]
meatmeal = chickwts.copy$weight [chickwts.copy$feed == "meatmeal"]
casein = chickwts.copy$weight [chickwts.copy$feed == "casein"]

# A single t test:

var.test(casein,horsebean)
# p val > alpha : accept H0 => variances are equal
t.test(casein,horsebean)
# p val < alpha : reject H0 => means are not equal,
# the types of feeds affect the weight of the chick

# to test all possible combinations of feed groups:

WeightsMatrix=matrix(c(horsebean,rep(NA,4),linseed,rep(NA,2),soybean,sunflower,rep(NA,2),meatmeal,rep(NA,3),casein,rep(NA,2)),nrow = 6,byrow = T)
rownames(WeightsMatrix) = c("horsebean","linseed","soybean","sunflower","meatmeal","casein")


results <- list()
for (i in 1:5) { 
  l = i+1
  for (j in l:6) {
    results[[paste0(i,"_",j)]] <- list(var.test(WeightsMatrix[i,],WeightsMatrix[j,]),t.test(WeightsMatrix[i,],WeightsMatrix[j,]))
  }
}
print(results)

GetPVal = function(){
  pvals = c()
  for (i in 1:5) { 
    l = i+1
    for (j in l:6) {
      var.test(WeightsMatrix[i,],WeightsMatrix[j,])
      pvals = c(pvals,t.test(WeightsMatrix[i,],WeightsMatrix[j,])$p.value)
      }
  }; return(pvals)
  }

namedPvals <- list()

for (i in 1:5) {
  l = i+1
  for (j in l:6) {
    var.test(WeightsMatrix[i,],WeightsMatrix[j,])
    namedPvals[[paste(rownames(WeightsMatrix)[i],rownames(WeightsMatrix)[j])]] <- c(t.test(WeightsMatrix[i,],WeightsMatrix[j,])$p.value,p.adjust(t.test(WeightsMatrix[i,],WeightsMatrix[j,])$p.value))
  }
}

p.val=GetPVal()
pval.adj=p.adjust(GetPVal())
grpa = c()
 grpb = c()
 for (i in 1:5) {
  l=i+1;
  for(j in l:6){
    grpa = c(grpa,rownames(WeightsMatrix)[i]);grpb = c(grpb,rownames(WeightsMatrix)[j])}
 }

pvals.data = data.frame(feed1 = grpa,feed2=grpb,p_value=p.val,adjusted_p_value = pval.adj)
pvals.data  

Ha = pvals.data[pvals.data$p_value<0.05,]
Ha_adj = pvals.data[pvals.data$adjusted_p_value<0.05,]
# according to the output there is a single pair whose adjust p value indicates that their means are equal or that the feed types have similar effects on weights 

horsebean.len = length(horsebean)
linseed.len = length(linseed)
soybean.len = length(soybean)
sunflower.len = length(sunflower)
meatmeal.len = length(meatmeal)
casein.len = length(casein)

barplot(c(horsebean.len,soybean.len,sunflower.len,meatmeal.len,casein.len,linseed.len))

summary(chickwts.copy)

h=summary(horsebean)
l=summary(linseed)
sb=summary(soybean)
c=summary(casein)
m=summary(meatmeal)
sf=summary(sunflower)

boxplot(h)
boxplot(l)
boxplot(sb)
boxplot(c)
boxplot(m)
boxplot(sf)


