#  importing needful library
library(MASS)
library(cluster)
library(factoextra)
library(rayshader)
library(ggplot2)
library(dplyr)

#  importing data 
customer=read.csv("Mall_Customers.csv")

# show First five Row
head(customer)
# show last five row
tail(customer)
# show dimension of our data
dim(customer)
# show data type in python
str(customer)
# check null value  and duplicate value
sum(is.na(customer))
duplicated(customer)

colnames(customer)

summary(customer)

### Exploratory Data Analysis ###
# Rename some column names
customer <- rename(customer, annual_income = Annual.Income..k..,spending_score = Spending.Score..1.100.)
## barplot for the variable "Gender" using ggplot
a=customer$Gender
barplot(table(customer$Gender),main="Total no of Male and Females",xlab = "Gender",col=rainbow(5))
count=0
count2=0
for(i in 1:200){
  if(customer$Gender[i]=="Male"){
    count=count+1
  }
  else{
    count2=count2+1
  }
 
}
cat("male=",count,"\n","Female=",count2)


## Histogram for the variable "Age"
ggplot(customer, aes(x = Age)) +
  geom_vline(aes(xintercept = mean(Age)), color = "blue",
             linetype = "dashed", size = 1) +
  geom_histogram(binwidth = 3, aes(y = ..density..), 
                 color = "black", fill = "white") +
  geom_density(alpha = 0.4, fill = "blue") +
  labs(title = "Histogram to Show Density of Age Class")

## Density for the variable "annual_income"
ggplot(customer,aes(x=annual_income))+
  geom_density()+
  labs(title="Density Plot for the annual income variable")
#approximately normally distributed

## Boxplot for the variable "spending_score"
ggplot(customer, aes(x = spending_score, y= Gender)) +
  geom_boxplot() +
  labs(title = "Boxplot for the Spending Score Variable")
x=customer[,4:5]
x
plot(x)




### Clustering using k-means Algorithm: Hartigan-Wong K-Means ###
## Set seed 
set.seed(124)
#Get the optimal number of clusters using Elbow Method
##Elbow Method
fviz_nbclust(x,kmeans,method="wss")


## Create the customer clusters with KMeans
k6=kmeans(x,centers=6)
#Print the results
k6
segment = k6$cluster
data1=cbind(customer,segment)
data1
library(cluster)
##Plot the six KMeans clusters
clusplot(customer, k6$cluster, color=TRUE, shade=TRUE, labels=0, lines=0)


# Create a plot of the customers segments
ggplot(customer, aes(x = annual_income, y = spending_score)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name = " ", 
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", 
                                "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", 
          subtitle = "Using K-means Clustering")
## We can group the clusters as
#Cluster 1 : Low income earners with low spending score. I can assume that this is so because people with low income will tend to purchase less item at the store
#Cluster 2 and cluster 4 : This group of customers have a higher income but they do not spend more at the store. One of the assumption could be that they are not satisfied with the services rendered at the store. They are another ideal group to be targeted by the marketing team because they have the potential to bring in increased profit for the store.
#Cluster 3 : These are low income earning customers with high spending scores. I can assume that why this group of customers spend more at the retail store despite earning less is because they enjoy and are satisfied with the services rendered at the retail store.
#Cluster 5 : The customers in this group are high income earners and with high spending scores. They bring in profit. Discounts and other offers targeted at this group will increase their spending score and maximize profit.
#Cluster 6 : These are average income earners with average spending scores. They are cautious with their spending at the store.
























