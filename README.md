# Collaborative-Filtering to predict movie rating
Dataset:
	The MovieLens datasets (http://movielens.org, a movie recommendation service) are curated by a research group at the University of Minnesota (https://grouplens.org/datasets/movielens/). I have used the small dataset that consists of 100004 ratings and 1296 tag applications across 9125 movies. These data were created by 671 users between January 09, 1995 and October 16, 2016. This dataset was generated on October 17, 2016.

Description:
I have developed a small collaborative filtering-based recommendation system. Here I have developed a recommender engine that will predict the rating that a user with ID 191 will give to a movie.

User IDs     Jaccard Similarity
User ID 513     0.4358974
User ID 317     0.4033613
User ID 415     0.3255814
User ID 375     0.3049645
User ID 64      0.2753623
User ID 556     0.2727273
User ID 82      0.2527473
User ID 225     0.2420382
User ID 657     0.2262774
User ID 266     0.2216216
User ID 568     0.2105263
User ID 50      0.2009804


We will randomly pick 5 users from the above table. Using these 5 users and User ID 191, we will create a utility matrix, U, where the rows are the users and the columns are the movies. U will be filled with the ratings that a particular user gave a movie, if the user watched that movie. If the user did not watch the movie, the cell will contain an NA. Each User ID in the above table may have watched
more movies than User ID 191 has, or less movies, or equal amount of movies.

(a) Prediction using user-user similarity: 
  The similarity we will use between users is the Jaccard Similarity shown in the above table.

(b) Prediction using item-item similarity: 
  The similarity we will use between users is the Pearson correlation similarity.
