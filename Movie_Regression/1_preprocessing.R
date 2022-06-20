library (MASS)
library (corrplot)
library(psych)


### import dataset originario
movie <- read.csv(file='Movie_regression.csv', sep=',')
attach(movie)

### controllo se sono presenti duplicati
movie_no_dup <- movie [!duplicated(movie), ] #0 duplicati

### rimuovo colonne indesiderate [Multiplex coverage, Movie_length, 3D_available, Genre, Num_multiplex, Collection]
movie_drop = movie_no_dup[,-c(3, 5, 12, 15, 17, 18)]

### controlliamo che non ci siano valori mancanti
sum(is.na(movie_drop)) # 12 valori mancanti

### cerchiamo la colonna dei valori mancanti
which(is.na(movie_drop$Marketing.expense))
which(is.na(movie_drop$Production.expense))
which(is.na(movie_drop$Budget))
which(is.na(movie_drop$Lead_.Actor_Rating))
which(is.na(movie_drop$Lead_Actress_rating))
which(is.na(movie_drop$Director_rating))
which(is.na(movie_drop$Producer_rating))
which(is.na(movie_drop$Critic_rating))
which(is.na(movie_drop$Trailer_views))
which(is.na(movie_drop$Time_taken))    # 21  59  61 105 106 216 261 360 404 417 441 498
which(is.na(movie_drop$Twitter_hastags))
which(is.na(movie_drop$Avg_age_actors))

### vettore degli indici dei valori mancanti
delete <- which(is.na(movie_drop$Time_taken))
nrow(movie_drop)

### rimozione righe valori NA
movie_final <- movie_drop[-c(delete),]
nrow(movie_final) # 494



### boxplot per vedere gli outliers
dev.new()
oldpar = par(mfrow = c(2,2))
for ( i in 1:4 ) {
  boxplot(movie_final[[i]])
  mtext(names(movie_final)[i], cex = 0.8, side = 1, line = 2)
}

dev.new()
oldpar = par(mfrow = c(2,2))
for ( i in 5:8 ) {
  boxplot(movie_final[[i]])
  mtext(names(movie_final)[i], cex = 0.8, side = 1, line = 2)
}

dev.new()
oldpar = par(mfrow = c(2,2))
for ( i in 9:12 ) {
  boxplot(movie_final[[i]])
  mtext(names(movie_final)[i], cex = 0.8, side = 1, line = 2)
}
# tutte le variabili indipendenti tranne Production.expense, Critic_rating e Avg_age_actors contengono outliers



### usiamo gli istogrammi per vedere come sono distribuiti i dati
dev.new()
par(mfrow = c(3,4))
for ( i in 1:12 ) {
  truehist(movie_final[[i]], xlab = names(movie_final)[i], col = 'lightgreen', main = paste("Original, mean =", signif(mean(movie_final[[i]]),3)))
  }


### rimuoviamo gli outliers sopra il 98-esimo percentile da Marketing.expense, Budget, Lead_.Actor_Rating
# Lead_Actress_rating, Director_rating, Producer_rating, Trailer_views, Time_taken e Twitter_hastags

# Marketing.expense
q98 = quantile(movie_final$Marketing.expense, .98)
outliers1 = movie_final$Marketing.expense[movie_final$Marketing.expense > q98]   # length(outliers) = 10
movie_cleaned = movie_final[-which(movie_final$Marketing.expense %in% outliers1), ]
# plot della distribuzioni trasformata
dev.new()
par(mfrow = c(1,2))
truehist(movie_final[[1]], xlab = names(movie_final)[1], col = 'lightgreen', main = paste("Original, mean =", signif(mean(movie_final[[1]]),1)))
truehist(movie_cleaned[[1]], xlab = names(movie_cleaned)[1], col = 'lightgreen', main = paste("Trasformed, mean =", signif(mean(movie_cleaned[[1]]),1)))


# Budget
q98 = quantile(movie_final$Budget, .98)
outliers3 = movie_final$Budget[movie_final$Budget > q98]   # length(outliers) = 10
movie_cleaned = movie_final[-which(movie_final$Budget %in% outliers3), ]
# plot della distribuzioni trasformata
dev.new()
par(mfrow = c(1,2))
truehist(movie_final[[3]], xlab = names(movie_final)[3], col = 'lightgreen', main = paste("Original, mean =", signif(mean(movie_final[[3]]),3)))
truehist(movie_cleaned[[3]], xlab = names(movie_cleaned)[3], col = 'lightgreen', main = paste("Trasformed, mean =", signif(mean(movie_cleaned[[3]]),3)))


# Lead_.Actor_Rating
q98 = quantile(movie_final$Lead_.Actor_Rating, .98)
outliers4 = movie_final$Lead_.Actor_Rating[movie_final$Lead_.Actor_Rating > q98]   # length(outliers) = 9 
movie_cleaned = movie_final[-which(movie_final$Lead_.Actor_Rating %in% outliers4), ]
# plot della distribuzioni trasformata
dev.new()
par(mfrow = c(1,2))
truehist(movie_final[[4]], xlab = names(movie_final)[4], col = 'lightgreen', main = paste("Original, mean =", signif(mean(movie_final[[4]]),4)))
truehist(movie_cleaned[[4]], xlab = names(movie_cleaned)[4], col = 'lightgreen', main = paste("Trasformed, mean =", signif(mean(movie_cleaned[[4]]),4)))


# Lead_Actress_rating
q98 = quantile(movie_final$Lead_Actress_rating, .98)
outliers5 = movie_final$Lead_Actress_rating[movie_final$Lead_Actress_rating > q98]   # length(outliers) = 10
movie_cleaned = movie_final[-which(movie_final$Lead_Actress_rating %in% outliers5), ]
# plot della distribuzioni trasformata
dev.new()
par(mfrow = c(1,2))
truehist(movie_final[[5]], xlab = names(movie_final)[5], col = 'lightgreen', main = paste("Original, mean =", signif(mean(movie_final[[5]]),5)))
truehist(movie_cleaned[[5]], xlab = names(movie_cleaned)[5], col = 'lightgreen', main = paste("Trasformed, mean =", signif(mean(movie_cleaned[[5]]),5)))


# Director_rating
q98 = quantile(movie_final$Director_rating, .98)
outliers6 = movie_final$Director_rating[movie_final$Director_rating > q98]   # length(outliers) = 9
movie_cleaned = movie_final[-which(movie_final$Director_rating %in% outliers6), ]
# plot della distribuzioni trasformata
dev.new()
par(mfrow = c(1,2))
truehist(movie_final[[6]], xlab = names(movie_final)[6], col = 'lightgreen', main = paste("Original, mean =", signif(mean(movie_final[[6]]),6)))
truehist(movie_cleaned[[6]], xlab = names(movie_cleaned)[6], col = 'lightgreen', main = paste("Trasformed, mean =", signif(mean(movie_cleaned[[6]]),6)))


# Producer_rating
q98 = quantile(movie_final$Producer_rating, .98)
outliers7 = movie_final$Producer_rating[movie_final$Producer_rating > q98]   # length(outliers) = 9
movie_cleaned = movie_final[-which(movie_final$Producer_rating %in% outliers7), ]
# plot della distribuzioni trasformata
dev.new()
par(mfrow = c(1,2))
truehist(movie_final[[7]], xlab = names(movie_final)[7], col = 'lightgreen', main = paste("Original, mean =", signif(mean(movie_final[[7]]),7)))
truehist(movie_cleaned[[7]], xlab = names(movie_cleaned)[7], col = 'lightgreen', main = paste("Trasformed, mean =", signif(mean(movie_cleaned[[7]]),7)))


# Trailer_views
q98 = quantile(movie_final$Trailer_views, .98)
outliers9 = movie_final$Trailer_views[movie_final$Trailer_views > q98]   # length(outliers) = 10
movie_cleaned = movie_final[-which(movie_final$Trailer_views %in% outliers9), ]
# plot della distribuzioni trasformata
dev.new()
par(mfrow = c(1,2))
truehist(movie_final[[9]], xlab = names(movie_final)[9], col = 'lightgreen', main = paste("Original, mean =", signif(mean(movie_final[[9]]),9)))
truehist(movie_cleaned[[9]], xlab = names(movie_cleaned)[9], col = 'lightgreen', main = paste("Trasformed, mean =", signif(mean(movie_cleaned[[9]]),9)))


# Time_taken
q98 = quantile(movie_final$Time_taken, .98)
outliers10 = movie_final$Time_taken[movie_final$Time_taken > q98]   # length(outliers) = 10
movie_cleaned = movie_final[-which(movie_final$Time_taken %in% outliers10), ]
# plot della distribuzioni trasformata
dev.new()
par(mfrow = c(1,2))
truehist(movie_final[[10]], xlab = names(movie_final)[10], col = 'lightgreen', main = paste("Original, mean =", signif(mean(movie_final[[10]]),10)))
truehist(movie_cleaned[[10]], xlab = names(movie_cleaned)[10], col = 'lightgreen', main = paste("Trasformed, mean =", signif(mean(movie_cleaned[[10]]),10)))


# Twitter_hastags
q98 = quantile(movie_final$Twitter_hastags, .98)
outliers11 = movie_final$Twitter_hastags[movie_final$Twitter_hastags > q98]   # length(outliers) = 10
movie_cleaned = movie_final[-which(movie_final$Twitter_hastags %in% outliers11), ]
# plot della distribuzioni trasformata
dev.new()
par(mfrow = c(1,2))
truehist(movie_final[[11]], xlab = names(movie_final)[11], col = 'lightgreen', main = paste("Original, mean =", signif(mean(movie_final[[11]]),11)))
truehist(movie_cleaned[[11]], xlab = names(movie_cleaned)[11], col = 'lightgreen', main = paste("Trasformed, mean =", signif(mean(movie_cleaned[[11]]),11)))

nrow(movie_cleaned) # 484

### istogramma per studiare la distribuzione dopo la rimozione degli outliers
dev.new()
par(mfrow = c(3,4))
for ( i in 1:12 ) {
  truehist(movie_cleaned[[i]], xlab = names(movie_cleaned)[i], col = 'lightgreen', main = paste("Trasformed, mean =", signif(mean(movie_cleaned[[i]]),3)))
}



### valutiamo la correlazione tra le variabili
M = cor(movie_cleaned)

dev.new()
corrplot(M, method = "color", cl.ratio = 0.5,
         type = "lower", number.cex = .6,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 45, # Text label color and rotation
         diag = FALSE)

#### costruiamo scatterplots per valutare la linearità della relazione tra la risposta e ogni singolo predittore
dev.new()
par(mfrow=c(2,2))
scatter.smooth(x=movie_cleaned$Marketing.expense, y=movie_cleaned$budget, xlab="Marketing expense", ylab="budget")
scatter.smooth(x=movie_cleaned$Production.expense, y=movie_cleaned$budget, xlab="Production expense", ylab="budget")
scatter.smooth(x=movie_cleaned$Lead_.Actor_Rating, y=movie_cleaned$budget, xlab="Lead Actor Rating", ylab="budget")
scatter.smooth(x=movie_cleaned$Lead_Actress_rating, y=movie_cleaned$budget, xlab="Lead_Actress_rating", ylab="budget")

dev.new()
par(mfrow=c(2,2))
scatter.smooth(x=movie_cleaned$Director_rating, y=movie_cleaned$budget, xlab="Director_rating", ylab="budget")
scatter.smooth(x=movie_cleaned$Producer_rating, y=movie_cleaned$budget, xlab="Producer_rating", ylab="budget")
scatter.smooth(x=movie_cleaned$Critic_rating, y=movie_cleaned$budget, xlab="Critic_rating", ylab="budget")
scatter.smooth(x=movie_cleaned$Trailer_views, y=movie_cleaned$budget, xlab="Trailer_views", ylab="budget")

dev.new()
par(mfrow=c(2,2))
scatter.smooth(x=movie_cleaned$Time_taken, y=movie_cleaned$budget, xlab="Time_taken", ylab="budget")
scatter.smooth(x=movie_cleaned$Twitter_hastags, y=movie_cleaned$budget, xlab="Twitter_hastags", ylab="budget")
scatter.smooth(x=movie_cleaned$Avg_age_actors, y=movie_cleaned$budget, xlab="Avg_age_actors", ylab="budget")

### salviamo dataset per importarlo in altri files
write.csv(movie_cleaned, "movie_final.csv", row.names = FALSE)
