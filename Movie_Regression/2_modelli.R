##### definizione dei modelli usati nel progetto

mov <- read.csv(file='movie_final.csv', sep=',')
attach(mov)

linear_model <- (Budget~.)

linear_model_b <- (Budget~.-Lead_.Actor_Rating-Lead_Actress_rating-Producer_rating)

poly2_model <- (Budget~poly(Marketing.expense,2)+poly(Production.expense,2)+poly(Lead_.Actor_Rating,2)
                +poly(Lead_Actress_rating,2)+poly(Director_rating,2)+poly(Producer_rating,2)+poly(Critic_rating,2)
                +poly(Trailer_views,2)+poly(Time_taken,2)+poly(Twitter_hastags,2)+poly(Avg_age_actors,2))

poly2_model_b <- (Budget~poly(Marketing.expense,2)+poly(Production.expense,2)+poly(Director_rating,2)
                  +poly(Critic_rating,2)+poly(Trailer_views,2)+poly(Time_taken,2)+poly(Twitter_hastags,2)+poly(Avg_age_actors,2))

poly3_model <- (Budget~poly(Marketing.expense,3)+poly(Production.expense,3)+poly(Lead_.Actor_Rating,3)
                +poly(Lead_Actress_rating,3)+poly(Director_rating,3)+poly(Producer_rating,3)+poly(Critic_rating,3)
                +poly(Trailer_views,3)+poly(Time_taken,3)+poly(Twitter_hastags,3)+poly(Avg_age_actors,3))

poly3_model_b <- (Budget~poly(Marketing.expense,3)+poly(Production.expense,3)+poly(Director_rating,3)
                  +poly(Critic_rating,3)+poly(Trailer_views,3)+poly(Time_taken,3)+poly(Twitter_hastags,3)+poly(Avg_age_actors,3))

poly4_model <- (Budget~poly(Marketing.expense,4)+poly(Production.expense,4)+poly(Lead_.Actor_Rating,4)
                   +poly(Lead_Actress_rating,4)+poly(Director_rating,4)+poly(Producer_rating,4)+poly(Critic_rating,4)
                   +poly(Trailer_views,4)+poly(Time_taken,4)+poly(Twitter_hastags,4)+poly(Avg_age_actors,4))

poly4_model_b <- (Budget~poly(Marketing.expense,4)+poly(Production.expense,4)+poly(Director_rating,4)
                  +poly(Critic_rating,4)+poly(Trailer_views,4)+poly(Time_taken,4)+poly(Twitter_hastags,4)+poly(Avg_age_actors,4))

models <- c(linear_model, linear_model_b, poly2_model, poly2_model_b, poly3_model, poly3_model_b, poly4_model, poly4_model_b)
