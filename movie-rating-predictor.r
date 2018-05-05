#A20391757 HW4
```{r}
#title: "HW4"
#output: html_document
#Author: "Goutham Muguluvalli-Niranjan"
#CWID: "A20391757"
```

#Question 2.3
```{r}
rm(list=ls())

rating <- read.csv("ratings.csv")
#ratingdata <- subset(rating, rating[,1]==67)

user191.d <- read.csv("user191.csv", stringsAsFactors=FALSE)

# randomly pick five users
users <- as.data.frame(user191.d[sample(nrow(user191.d), 5),], stringsAsFactors=FALSE)
colnames(users) <- c("userID","Jaccard_similarity")

#-------------------------------------------------------------------
#(a) Prediction using user-user similarity:

U.Matrix  <- matrix(NA, nrow=6,ncol=(29))
colnames(U.Matrix) <-c("10","34","47","110","150","153","161","165","185","208","231","292","296","300","318","339","344","349","356","380","434","454","457","480","588","590","592","593","595")

rownames(U.Matrix) <-c("191",users$userID[1],users$userID[2],users$userID[3],users$userID[4],users$userID[5])

u <- rating[rating$userId==191,2:3]

for(j in 1:ncol(U.Matrix)) 
  {
 k = as.numeric(colnames(U.Matrix)[j]) 
  if(nrow(u[u$movieId==k,]) != 0)
    {
      if(k != 150 & k != 296 & k != 380 & k != 590)
        U.Matrix[1,j] <- u[u$movieId==k,2]
  }
}

# Loops through user ids
for(i in 2:nrow(U.Matrix)) 
{
  u <- rating[rating$userId==users$userID[i-1],2:3]
  
# Loops through Movie ids 
for(j in 1:ncol(U.Matrix)) 
  {
    k = as.numeric(colnames(U.Matrix)[j]) 
    if(nrow(u[u$movieId==k,]) != 0)
      {
        U.Matrix[i,j] <- u[u$movieId==k,2]
      }
    }
}  

# User Utility Matrix
#U.Matrix

N <- users[with(users, order(Jaccard_similarity, decreasing = TRUE)), ]

# Top 3 Neighbours
u1 <- as.character(N[1,1])
u2 <- as.character(N[2,1])
u3 <- as.character(N[3,1])

r150 <- ((N[1,2]*U.Matrix[u1,5])+(N[2,2]*U.Matrix[u2,5])+(N[3,2]*U.Matrix[u3,5]))/(N[1,2]+N[2,2]+N[3,2])

r296 <- ((N[1,2]*U.Matrix[u1,13])+(N[2,2]*U.Matrix[u2,13])+(N[3,2]*U.Matrix[u3,13]))/(N[1,2]+N[2,2]+N[3,2])

r380 <- ((N[1,2]*U.Matrix[u1,20])+(N[2,2]*U.Matrix[u2,20])+(N[3,2]*U.Matrix[u3,20]))/(N[1,2]+N[2,2]+N[3,2])

r590 <- ((N[1,2]*U.Matrix[u1,26])+(N[2,2]*U.Matrix[u2,26])+(N[3,2]*U.Matrix[u3,26]))/(N[1,2]+N[2,2]+N[3,2])

# Function that returns Root Mean Squared Error
rmse <- function(error)
{
    sqrt(mean(error^2))
}

 
# Example data
actual <- c(4,5,3,4)
predicted <- c(r150,r296,r380,r590)
 
# Calculate error
error <- actual - predicted
 
# Example of invocation of functions
r <- rmse(error)

cat("User ID 191, 5 random user IDs:",users$userID,"\n\n")
cat("Using user-user similarity, User ID 191 will rate the movies as follows:\n\n")
cat("150:",r150,"\n")
cat("296:",r296,"\n")
cat("380:",r380,"\n")
cat("590:",r590,"\n")
cat("RMSE:",r,"\n\n")

#--------------------------------------------------------------

#(b) Prediction using item-item similarity:


ItemU.Matrix  <- matrix(NA, nrow=29,ncol=(6))
rownames(ItemU.Matrix)<-c("10","34","47","110","150","153","161","165","185","208","231","292","296","300","318","339","344","349","356","380","434","454","457","480","588","590","592","593","595")

colnames(ItemU.Matrix)<-c("191",users$userID[1],users$userID[2],users$userID[3],users$userID[4],users$userID[5])

u <- rating[rating$userId==191,2:3]

for(j in 1:nrow(ItemU.Matrix)) 
  {
 k = as.numeric(rownames(ItemU.Matrix)[j]) 
  if(nrow(u[u$movieId==k,]) != 0)
    {
      if(k != 150 & k != 296 & k != 380 & k != 590)
        ItemU.Matrix[j,1] <- u[u$movieId==k,2]
  }
}

# Loops through user ids
for(i in 2:ncol(ItemU.Matrix)) 
{
  u <- rating[rating$userId==users$userID[i-1],2:3]
  
# Loops through Movie ids 
for(j in 1:nrow(ItemU.Matrix)) 
  {
    k = as.numeric(rownames(ItemU.Matrix)[j]) 
    if(nrow(u[u$movieId==k,]) != 0)
      {
        ItemU.Matrix[j,i] <- u[u$movieId==k,2]
      }
    }
}  

# Item Utility Matrix
ItemU.Matrix2 <- ItemU.Matrix

p <- apply(ItemU.Matrix, 1, function(x) mean(x,na.rm=T))

data <- ItemU.Matrix


for(i in 1:nrow(data)) 
{
  for(j in 1:ncol(data)) 
  {
    if(!is.na(data[i,j]))
    {
      data[i,j] <- data[i,j] - p[[i]]
    } 
    else
    {
      ItemU.Matrix2[i,j] <- 0
      data[i,j] <- 0 
    }
  }
}

pearson.sim150 <- as.data.frame(matrix(NA,nrow=29,ncol=1))
pearson.sim296 <- as.data.frame(matrix(NA,nrow=29,ncol=1))
pearson.sim380 <- as.data.frame(matrix(NA,nrow=29,ncol=1))
pearson.sim590 <- as.data.frame(matrix(NA,nrow=29,ncol=1))

cosine <- function(x, y) {
  # Need to do error checking:
  # 1) Ensure x and y are vectors.
  
  sum(x*y)/(norm(x, type="2") * norm(y, type="2"))
}

for(j in 1:nrow(data)){
pearson.sim150[j,1]<-cosine(as.numeric(data["150",]),as.numeric(data[j,]))
pearson.sim296[j,1]<-cosine(as.numeric(data["296",]),as.numeric(data[j,]))
pearson.sim380[j,1]<-cosine(as.numeric(data["380",]),as.numeric(data[j,]))
pearson.sim590[j,1]<-cosine(as.numeric(data["590",]),as.numeric(data[j,]))
}

rownames(pearson.sim150) <- rownames(ItemU.Matrix)
rownames(pearson.sim296) <- rownames(ItemU.Matrix)
rownames(pearson.sim380) <- rownames(ItemU.Matrix)
rownames(pearson.sim590) <- rownames(ItemU.Matrix)

# Pick the top 3 neighbours
N150<- pearson.sim150[with(pearson.sim150, order(V1, decreasing = TRUE)),,drop=FALSE][2:4,,drop=FALSE]
N296<- pearson.sim296[with(pearson.sim296, order(V1, decreasing = TRUE)),,drop=FALSE][2:4,,drop=FALSE]
N380<- pearson.sim380[with(pearson.sim380, order(V1, decreasing = TRUE)),,drop=FALSE][2:4,,drop=FALSE]
N590<- pearson.sim590[with(pearson.sim590, order(V1, decreasing = TRUE)),,drop=FALSE][2:4,,drop=FALSE]

R.150 <- ((N150[1,1]*ItemU.Matrix2[rownames(N150)[1],1])+(N150[2,1]*ItemU.Matrix2[rownames(N150)[2],1])+(N150[3,1]*ItemU.Matrix2[rownames(N150)[3],1]))/(N150[1,1]+N150[2,1]+N150[3,1])

R.296 <- ((N296[1,1]*ItemU.Matrix2[rownames(N296)[1],1])+(N296[2,1]*ItemU.Matrix2[rownames(N296)[2],1])+(N296[3,1]*ItemU.Matrix2[rownames(N296)[3],1]))/(N296[1,1]+N296[2,1]+N296[3,1])

R.380 <- ((N380[1,1]*ItemU.Matrix2[rownames(N380)[1],1])+(N380[2,1]*ItemU.Matrix2[rownames(N380)[2],1])+(N380[3,1]*ItemU.Matrix2[rownames(N380)[3],1]))/(N380[1,1]+N380[2,1]+N380[3,1])

R.590 <- ((N590[1,1]*ItemU.Matrix2[rownames(N590)[1],1])+(N590[2,1]*ItemU.Matrix2[rownames(N590)[2],1])+(N590[3,1]*ItemU.Matrix2[rownames(N590)[3],1]))/(N590[1,1]+N590[2,1]+N590[3,1])

# Example data
Iactual <- c(4,5,3,4)
Ipredicted <- c(R.150,R.296,R.380,R.590)
 
# Calculate error
error <- Iactual - Ipredicted
 
# Example of invocation of functions
R <- rmse(error)

cat("User ID 191, 5 random user IDs:",users$userID,"\n\n")
cat("Using Item-Item similarity, User ID 191 will rate the movies as follows:\n\n")
cat("150:",R.150,"\n")
cat("296:",R.296,"\n")
cat("380:",R.380,"\n")
cat("590:",R.590,"\n")
cat("RMSE:",R)


```