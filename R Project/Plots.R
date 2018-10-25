library('ggplot2')
library('gridExtra')
library('ggrepel')
library('reshape2')
library('tidyverse')  # data manipulation
library('cluster')    # clustering algorithms
library('factoextra')
library('ggrepel')
source("hw2.R")
marry = read.csv("Finaldata.csv", header = T, sep =",", stringsAsFactors = F, dec=".")

#######################################################
#lets see if there is obvious trend between marrying down and major
ggplot(marry, aes(x = MenMarDown, y = WomMarDown)) + 
  geom_point(aes(MenMarDown, WomMarDown, fill = marry$Category, color = marry$Category, shape = marry$Degree), size = 4, stroke = 1.2) +
  labs(title = "Men Marrying Down vs. Women Marrying Down", x = "Men", y = "Women") +
  hw2
#No but we notice that women may more than men

#########################################################

forbox2 = melt(marry, id.vars = "Degree", measure.vars = c("MenMarDown", "WomMarDown") )
forbox2 = subset(forbox2, Degree != "Either")
#Lets make a boxplot to see this


ggplot(forbox2, aes(x = variable, y = value, fill = Degree)) + 
  geom_boxplot( outlier.shape = NA) + 
  geom_point(aes(fill = variable), size = 2.5, shape = 19, alpha = .8, position = position_jitterdodge(), show.legend = F) +
  labs(title = "Distributions of Men and Women", x = "Gender", y = "Percent that Married Down") +
  hw2 + 
  coord_flip() +
  scale_fill_manual(name = "Degree", values = c("lightblue","maroon",NA,NA), limits = c("BA","BS")) +
  scale_x_discrete(labels=c("Men", "Women")) +
  scale_y_continuous(limits = c(0.15,0.48)) 
  
#######################################################################

g = melt(table)
ggplot(g, aes(Var1, Var2)) + 
  geom_tile(aes( fill = value )) + 
  geom_text(aes(label = round(value, 1))) +
  labs(title = "Heatmap of Inter-Major", x = "Women", y = "Men") +
  scale_fill_gradient2(low = "white", high = "red4", mid = "firebrick1", 
                       midpoint = 15, limit = c(0,35), 
                       name="Values") + hw2

#######################################################################

table2 = cbind(marry$Category, table)
colnames(table2)[1] = "Category"
table2 = data.frame(table2)
bycat = aggregate(. ~ Category, data  = table2, sum)
colnames(bycat)[2:27] = marry$Category
try = t(bycat)
try = try[-1,]
try = cbind(marry$Category, try)
colnames(try) = c("Category","Art","Engineering","Human","Other","Science")
try = try[,-7]
#Because there were so many Humanities we arrange the data
try = data.frame(try)
try2 = aggregate( . ~ Category, data = try, sum)
try2 = try2[,-1]
try2 = cbind(c("Art","Engineering","Human","Other","Science"), try2)
colnames(try2)[1] = "Category"
try3 = melt(try2, id.vars = "Category")


ggplot(try3, aes(x = Category, y = value)) + 
  geom_bar(aes(fill = try4$Category), stat = "identity") +
  labs(title = "Mariages to each Category by Each Category", x = "Category", y = "Marriages") +
  hw2 + facet_grid(~ variable) +   guides(fill = F, colour = "none") 


############################################################3


marrydowns = marry[-6]
marrydowns = marrydowns[-4]
marrydowns = marrydowns[-1]
marrydowns = marrydowns[-5:-32]
marrydowns = marrydowns[-1:-2]
row.names(marrydowns) = marry$Major

res = hcut(marrydowns[1:2], k = 4, stand = T) 
fviz_dend(res, rect = F, cex = 0.7, k_colors = c("red","green4", "blue", "brown"), horiz = T) + hw2 + ggtitle("Marrying Down by Major")

########

fviz_nbclust(marrydowns[1:2], kmeans, method = "silhouette") + ggtitle("Marrying Down by Major") + hw2

big2 = kmeans(marrydowns[1:2], centers = 2, nstart = 25)
fviz_cluster(big2, data = marrydowns, geom = c("point", "text"), repel = T, ellipse.type = "convex", ellipse.alpha = 0.2) +  ggtitle("k = 2") +
  hw2

#lets see how three clusters looks
big3 = kmeans(marrydowns[1:2], centers = 3, nstart = 25)
fviz_cluster(big3, data = marrydowns, geom = c("point", "text"), repel = T, ellipse.type = "convex", ellipse.alpha = 0.2) +  ggtitle("k = 3") +
  hw2


##############################################33


ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point(col = "purple4", size = 2.5) +
    stat_smooth(method = "lm", col = "red4", size = 2) +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)), x = "Men Married Down", y = "Women Married Down")
}
linear = lm(marry$WomMarDown~marry$MenMarDown)
summary(linear)
ggplotRegression(linear)

