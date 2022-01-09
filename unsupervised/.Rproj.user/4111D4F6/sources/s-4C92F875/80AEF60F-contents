install.packages("caret")
install.packages("corrplot")
install.packages("ggplot2")
install.packages("devtools")
install.packages("factoextra")

remotes::install_github("vqv/ggbiplot")

library(devtools)
library(ggplot2)
library(corrplot)
library(caret)
library(ggbiplot)
library(ggfortify)
library(factoextra)

# Load dataframe
players = read.csv("./data/player_dataset.csv")
names(players)

#Remove columns X and ID
players = subset(players, players$Preferred.Positions != "GK ")
#feature_columns = c("Overall", "CAM", "CM", "CDM", "LAM", "RAM", "RM", "LM", "LDM", "RDM", "CF", "RF", "LF", "LS", "RS", "LW", "RW", "ST", "CB", "RCB", "LCB", "RB", "LB")
feature_columns = c("Overall", "Vision","Crossing", "Ball.control","Heading.accuracy", "Finishing", "Free.kick.accuracy", "Curve", "Dribbling","Interceptions","Long.passing","Long.shots","Marking","Penalties","Positioning", "Short.passing","Shot.power","Sliding.tackle","Standing.tackle","Volleys")
players = players[, feature_columns]
players = subset(players[, feature_columns])
dim(players)
str(players)


# Set all columns as numeric and drop all rows with NA values
players = sapply(players, as.numeric)
summary(players)
players = na.omit(players)
players = as.data.frame(players)


players = subset(players, Overall > 75)
players = within(players, rm(Overall))

# Shuffle data
random_rows = sample(nrow(players))
players = players[random_rows, ]

# Take only a sample of data instead of ~17k rows of data
sample_size = sample(500)
players = players[sample_size, ]

# -- Data analysis --

# Descriptive stats
colMeans(players)
apply(players, 2, var)

cor.players = round(cor(players), 3)
cor.players
corrplot(cor.players, tl.cex=0.5)

players = scale(players, center = TRUE, scale = TRUE)

pca.out = prcomp(players)
dim(pca.out$x)
plot(pca.out$x)
summary(pca.out)
plot(pca.out)
pca.out$center
pca.out$sdev
pca.out$rotation

screeplot(pca.out, type="l", main="Screeplot for player data")

autoplot(pca.out, loadings=TRUE, loadings.label=TRUE, loadings.label.size=5, x=2, y=3)

pca.var = pca.out$sdev^2
pca.var

par(mfrow=c(1,2))
pve=pca.var/sum(pca.var)
plot(pve, ylim=c(0,1), type="b", ylab="Proportion of variance explained", xlab="Principal component")
plot(cumsum(pve), ylim=c(0,1), type="b", ylab="Cumulative proportion variance explained", xlab="Principal component")
pve

data_pca_cor = round(cor(players, pca.out$x), 3)

corrplot(data_pca_cor, tl.cex=0.4)

plot(pca.out$x, type="n")
points(pca.out$x, cex=0.5)
summary(pca.out)

# K-means clustering

fviz_nbclust(players, kmeans, method="wss")
km = kmeans(players, centers=3, nstart=10)
names(km)

autoplot(km, players, frame=TRUE)


