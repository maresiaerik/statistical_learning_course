install.packages("caret")
install.packages("corrgram")
library(tree)
library(randomForest)
library(corrgram)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(caret)
source("./utils.R")

# Load player dataset
players = read.csv("./data/CompleteDataset.csv")
dim(players)
colnames(players)

# -- Data cleaning --

# Remove rows that are unnecessary

column_names_to_drop = c("ID", "Age", "Value", "Photo", "Name", "Nationality", "X", "Club", "Flag", "Wage", "Potential", "Club.Logo", "Special", "GK.kicking", "GK.positioning","GK.reflexes", "CAM", "CB", "CDM", "CF", "CM", "LAM", "LB", "LCM", "LCB", "LDM", "LF", "LM", "LS", "LW", "LWB", "RAM", "RB", "RCB", "RCM", "RDM", "RF", "RM", "RS", "RW", "RWB", "ST", "GK.handling", "GK.diving")
players = within(players, rm(list = column_names_to_drop))

# Remove goalkeepers
players = subset(players, Preferred.Positions != "GK")

ggplot(players, aes(x=Overall)) + ggtitle("Observations of each position") + geom_bar()


# Only take players with overall 70 into account
player_overall_treshold = 70
players = subset(players, Overall > 70)

dim(players)
colnames(players)

summary(players)

#Clean Preferred position column

# Remove all but the first position in list
players$Preferred.Positions = lapply(players$Preferred.Positions, extract_first_player_position)
players$Preferred.Positions = unlist(players$Preferred.Positions)

# Remove marginal positions
margin_positions = c("CDM", "LDM", "RDM", "CAM", "RAM", "LAM", "RWB", "LWB")
players = subset(players, !(Preferred.Positions %in% margin_positions))

#Add generic position
players$Position = with(players, specific_to_generic_postion_mapper[Preferred.Positions])
players$Position = as.character(players$Position)
players = subset(players, Position != "NULL")
players$Position = as.factor(unlist(players$Position))
players = na.omit(players)


# Inspect means
feature_columns = c("Acceleration", "Aggression", "Agility", "Balance", "Crossing", "Ball.control", "Composure", "Finishing", "Free.kick.accuracy", "Curve", "Dribbling","Heading.accuracy","Interceptions","Jumping","Long.passing","Long.shots","Marking","Penalties","Positioning","Reactions","Short.passing","Shot.power","Sliding.tackle","Sprint.speed","Stamina","Standing.tackle","Strength","Vision","Volleys")
def = subset(players, Position == "DEF")
mid = subset(players, Position == "MID")
att = subset(players, Position == "ATT")
summary(def)
def = def[, feature_columns]
mid = mid[, feature_columns]
att = att[, feature_columns]
def = sapply(def, as.numeric)
mid = sapply(mid, as.numeric)
att = sapply(att, as.numeric)
def = na.omit(def)
mid = na.omit(mid)
att = na.omit(att)
def_mean = data.frame(colMeans(def))
att_mean = data.frame(colMeans(att))
mid_mean = data.frame(colMeans(mid))
summary(def_mean)

ggplot(def_mean, aes(x=def_mean[1:nrow(def_mean),], y=feature_columns)) + geom_bar(stat="identity") + labs(y="Attributes", x="Attribute mean", title="Mean attributes for position DEF")
ggplot(mid_mean, aes(x=mid_mean[1:nrow(mid_mean),], y=feature_columns)) + geom_bar(stat="identity") + labs(y="Attributes", x="Attribute mean", title="Mean attributes for position MID")
ggplot(att_mean, aes(x=att_mean[1:nrow(att_mean),], y=feature_columns)) + geom_bar(stat="identity") + labs(y="Attributes", x="Attribute mean", title="Mean attributes for position ATT")



# Only keep feature columns
feature_columns = c("Acceleration", "Aggression", "Agility", "Balance", "Crossing", "Ball.control", "Composure", "Finishing", "Free.kick.accuracy", "Curve", "Dribbling","Heading.accuracy","Interceptions","Jumping","Long.passing","Long.shots","Marking","Penalties","Positioning","Reactions","Short.passing","Shot.power","Sliding.tackle","Sprint.speed","Stamina","Standing.tackle","Strength","Vision","Volleys")
y_column = c("Position")
columns_to_keep = c(feature_columns, y_column)
players = players[, columns_to_keep]

# Set feature columns as numeric
players[feature_columns] = sapply(players[feature_columns], as.numeric)
players = na.omit(players)

summary(players)



# Randomise data
random_rows = sample(nrow(players))
players = players[random_rows, ]

# Histogram off classes
ggplot(players, aes(x=Position)) + ggtitle("Observations of each position") + geom_bar()


# -- Decision tree model --

# Compute the model
tree.model =  tree(Position~., players)
summary(tree.model)

# Plot tree diagram
plot(tree.model)
text(tree.model, pretty=0, cex=1, main="Decision tree classifier on FIFA player data")
title("Decision tree classifier")

# Split train/test data
set.seed(123)
train = sample(1:nrow(players), nrow(players) - 1000)
tree.x.test = players[-train, ]
tree.Position.test = players$Position[-train]

# Decision tree with train data
tree.positions = tree(Position~., players, subset = train)
summary(tree.positions)
plot(tree.positions)
text(tree.positions, pretty=0)

# Test decision tree
tree.pred = predict(tree.positions, tree.x.test, type="class")
conf_mat = confusionMatrix(tree.pred, tree.x.test$Position)
conf_mat

conf_mat = conf_mat$table
conf_mat

cv.positions = cv.tree(tree.positions, FUN=prune.misclass)
names(cv.positions)
cv.positions$k

plot(cv.positions$size, cv.positions$dev, type="b", xlab="Size", ylab = "Error")
title("Error rate of pruning")

plot(cv.positions$k, cv.positions$dev, type="b")

prune.positions = prune.misclass(tree.positions, best=5)


plot(prune.positions)
text(prune.positions, pretty=0, cex=0.8)
title("Decsion tree after pruning")

prune.tree.positions.pred = predict(prune.positions, tree.x.test, type="class")
conf_mat = confusionMatrix(prune.tree.positions.pred, tree.x.test$Position)
conf_mat

# Random forest
str(players$Position)
oob.values = vector(length=20)
for (i in 1:29) {
  temp.model = randomForest(Position~.,players, importance=TRUE, subset=train, mtry=i, proximity=TRUE, verbose=TRUE)
  oob.values[i] = temp.model$err.rate[nrow(temp.model$err.rate), 1]
}


plot(oob.values, ylab="Error", xlab="m")
title("Out-of-box errors")


random.model = randomForest(Position~., players, importance=TRUE, subset=train, mtry=8)
random.model

random.predict = predict(random.model, newdata = tree.x.test)
conf_mat = confusionMatrix(random.predict, tree.x.test$Position)
conf_mat

conf_mat = conf_mat$table
varImpPlot(random.model, cex=1,  n.var=20, main="Random forest classifier variable importance")

# Boosting
set.seed(321)

fitControl = trainControl(
  method="repeatedcv",
  number=5,
  repeats=5
)

gbmGrid = expand.grid(
  interaction.depth = c(1, 2, 3), 
  n.trees = (1:15)*100, #100-1500
  shrinkage = c(0.01, 0.05, 0.1),
  n.minobsinnode = 10 #default
)

boost.model = train(Position~.,
              data=players[train,],
              method="gbm",
              trControl=fitControl,
              tuneGrid=gbmGrid,
              verbose=TRUE)

# Or subsequently load the model to save time on training the model (trained with the above)
#boost.model = readRDS("../Projects/fifa_rating/boost_model.rds")

plot(boost.model, metric="Accuracy", xlab="Number of trees")

player.boost.predict = predict(boost.model, newdata = tree.x.test)

player.boost.predict

conf_mat = confusionMatrix(player.boost.predict, tree.x.test$Position)
conf_mat

importance(boost.model)
varImp(player.boost.predict)

summary(boost.model, cBars=15, las=1)
