packages <- c('tidyverse', 'recipes', 'cluster', 'rattle', 'tidyclust', 'dplyr') #'tidymodels', 'tidyquant', 'plotly')
lapply(packages, library, character.only=TRUE)

setwd("/Users/davidingram/Desktop/R_Final") # change this to the directory that has the csv file

# === Load Data and Check ===

airline.raw <- read.csv("airline.review.cleaned.csv")
head(airline.raw)

contains_any_na = sapply(airline.raw, function(x) any(is.na(x)))
names(airline.raw)[contains_any_na]   # checking which rows have NA values

summary(airline.raw)

# === Prep & Drop NAs === (drops to 536, but over that 500 mark)

to.drop <- c('aircraft_type', 'layover', 'arriving_dest', 'departing_dest', 'country',
             'date_flown', 'X', 'wifi_connectivity')
to.keep <- c('type_of_travellers', 'seat_types','seat_comfort','cabin_staff_service', 
             'ground_service','food_beverages','inflight_entertainment', 'value_for_money','recommended')

airline.raw.drop <- airline.raw %>% select(-one_of(to.drop))

airline.kmeans <- drop_na(airline.raw.drop)

contains_any_na = sapply(airline.kmeans, function(x) any(is.na(x)))
names(airline.kmeans)[contains_any_na]   # checking if all NAs are gone 

# === Create Dummy Variables ===
 
str(airline.kmeans)   # check which variables are not numeric
summary(airline.kmeans)

recipe.kmeans <- recipe(~ ., data = airline.kmeans) %>%   # create dummy variables
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_scale(all_nominal_predictors()) %>%
  prep()

kmeans.baked <- bake(recipe.kmeans, new_data = airline.kmeans)

head(kmeans.baked)

# === Determine Cluster K ===

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="No. of Clusters",
       ylab="WSS")
  wss
}

wssplot(kmeans.baked) # doesn't give us a perfect picture, try another

pct_var <- data.frame(pct_var = 0, num_clusters=2:14)
totalss <- kmeans(kmeans.baked, centers = 14, nstart = 50, iter.max = 100)$totss
for(i in 2:14) {
  pct_var[i-1, 'pct_var'] <- kmeans(kmeans.baked, centers=i, nstart = 50, iter.max = 100)$betweenss/totalss
}

graph_elbow <- ggplot(pct_var, aes(x=num_clusters, y=pct_var)) +
  geom_line() +
  geom_point() +
  labs(y='% Variance Explained', x='No. of Clusters') +
  scale_x_continuous(breaks=seq(2, 14, by=2))   +
  theme_bw()
graph_elbow # still no definite cluster size, 6 stands out most


# === Model K-means ===

set.seed(43168)
kmeans.model <- kmeans(kmeans.baked, centers = 6)

head(kmeans.model$cluster)

# === Visualizing Clusters ===

clusplot(kmeans.baked, kmeans.model$cluster, color = TRUE, shade = TRUE, labels = 5, lines = 0, main = "K Means Cluster Plot")

kmeans.model$size  # clusters slightly imbalanced but enough could be explained by segment size 


# === Interpreting the Clusters ===

centers <- as.data.frame(t(kmeans.model$centers))
names(centers) <- paste('Cluster', 1:6)
centers$Symbol <- row.names(centers)
centers <- gather(centers, 'Cluster', 'Mean', -Symbol)

centers$Color = centers$Mean > 0
graph_clust <- ggplot(centers, aes(x=Symbol, y=Mean, fill=Color)) +
  geom_bar(stat='identity', position='identity', width=.75) + 
  facet_grid(Cluster ~ ., scales='free_y') +
  guides(fill='none')  +
  ylab('Component Loading') +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_text(angle=90, vjust=0.5))
graph_clust

