setwd("C:/Users/chaudhary1/Downloads/Harrisburg/699/airbnb-amsterdam")
listings = read.csv('listings.csv', header = TRUE, sep = ',')
listing_details = read.csv('listings_details.csv', header = TRUE, sep = ',')
review_details = read.csv('reviews_details.csv', header = TRUE, sep = ',')
price_details = read.csv('House_prices.csv', header = TRUE, sep = ',')
price_details = price_details[c(1:6,8:14),]

#install.packages("cld2")
library(cld2)


#install.packages("syuzhet")
library(syuzhet)

listings = listings[listings$availability_365 >100,]
listing_details = listing_details[listing_details$id %in% listings$id,]
review_details = review_details[review_details$listing_id %in% listings$id,]


All_Reviews = as.vector(review_details$comments)
All_Reviews = substr(All_Reviews,0,20)
Review_Language = detect_language(All_Reviews)
review_details$Review_Language = Review_Language


review_details = review_details[review_details$Review_Language == "en",]

All_Reviews = as.vector(review_details$comments)


sentiment_score = get_sentiment(All_Reviews)
sentiment_value = sentiment_score
sentiment_value[sentiment_score>=0] = 1
sentiment_value[sentiment_score<0] = -1

review_details$sentiment_score = sentiment_value

Calculated_Ratings = cbind(aggregate(x = review_details$sentiment_score, by = list(review_details$listing_id), FUN = sum),as.data.frame(table(review_details$listing_id))[,2])
Calculated_Ratings$Positive_Ratio = Calculated_Ratings$x/Calculated_Ratings$`as.data.frame(table(review_details$listing_id))[, 2]`*100

Rating_Comparison = merge(Calculated_Ratings[,c(1,4)], listing_details [,c(1,80)], by.x = "Group.1", by.y = "id", all = FALSE)
plot(Rating_Comparison$Positive_Ratio, Rating_Comparison$review_scores_rating)
cor.test(Rating_Comparison$Positive_Ratio, Rating_Comparison$review_scores_rating)

Rating_Comparison$Positive_Ratio = (Rating_Comparison$Positive_Ratio + 100)/2
Rating_Comparison$Average_Rating = (Rating_Comparison$Positive_Ratio + Rating_Comparison$review_scores_rating)/2


#Calculating average price per person charged
listing_details$price = as.numeric(listing_details$price)
listing_details$guests_included = as.numeric(listing_details$guests_included)
listing_details$listing_price_pp = (listing_details$price / listing_details$guests_included)

Cost_Price_data = merge(listing_details,price_details, by.x = "city", by.y = "City", all = FALSE)

library(ggplot2)
ggplot(Cost_Price_data, aes(x=listing_price_pp, y=guests_included, color = city)) + geom_point()



#Predicting price per person

smp_size <- floor(0.75 * nrow(listing_details))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(listing_details)), size = smp_size)

train <- listing_details[train_ind, ]
test <- listing_details[-train_ind, ]

Linear_model = lm(listing_price_pp ~ host_is_superhost + neighbourhood_cleansed + 
                    property_type + room_type + bed_type + review_scores_rating, 
                  data = train)
summary(Linear_model)

test = test[test$property_type!="Aparthotel",]
test$Predicted_value = predict(Linear_model,newdata = test)

install.packages("neuralnet")
library(neuralnet)


train_mm = model.matrix(~listing_price_pp + host_is_superhost + room_type + bed_type + review_scores_rating, data = train)

test_mm = model.matrix(~listing_price_pp + host_is_superhost + room_type + bed_type + review_scores_rating, data = test)

colnames(train_mm) = gsub(" ", "", colnames(train_mm))
colnames(test_mm) = gsub(" ", "", colnames(test_mm))

nn_model = neuralnet(listing_price_pp ~ host_is_superhostt  + bed_typeFuton + bed_typeCouch + bed_typeRealBed + room_typePrivateroom +  review_scores_rating, data = train_mm, hidden=2,act.fct = "logistic",
                     linear.output = FALSE)

summary(nn_model)
plot(nn_model)
test_mm$Predicted_value = predict(nn_model,newdata = test_mm)





