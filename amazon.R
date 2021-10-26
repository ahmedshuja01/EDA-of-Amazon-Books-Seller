# facet_wrap() to have multiple graphs

library(readr)
bs<- read_csv("bestsellers with categories.csv")
View(bs)

head(bs)
any(is.na((bs)))             # data is clean
summary(bs)

library(dplyr)
library(ggplot2)

# Counting number of user rating
ggplot(data=bs,aes(x=`User Rating`))+
geom_histogram(aes(y=(..count..)/sum(..count..)),fill="red",col="green")+
  scale_y_continuous(labels = scales::percent)+ylab("percent")+xlim(3,5.5)

#normal distribution of user ratings
ggplot(data=bs,aes(x=`User Rating`))+
geom_histogram(aes(y=..density..),fill="red",col="green",binwidth =0.1)+xlim(1,8)+
stat_function(fun=dnorm,
args=list(mean=mean(bs$`User Rating`,sd=sd(bs$`User Rating`))))


ggplot(data=bs,aes(x=`User Rating`))+geom_density(fill="red",alpha=0.7)+
geom_vline(aes(xintercept=mean(`User Rating`)),color="green",size=1)+
xlim(3,5.5)
  
x=quantile(bs$Price,probs = c(0.25,1))
x[2]
# total number of unique books publish in a year 
t_book=bs[!duplicated(bs$Name),]
ggplot(data=t_book,aes(x=Year))+
geom_histogram(aes(y=..density..),fill="red",col="green",binwidth = 0.2)+xlim(2007,2019)+
stat_function(fun=dnorm,args=list(mean=mean(t_book$Year,sd=sd(t_book$Year))))


#genre count
table(bs$Genre)

# rating  in the year
samp=bs%>%rename(User_rating=`User Rating`)
rating_year=bs%>%count(bs$Year,samp$User_rating)
View(rating_year)
rating_year=rating_year%>%
rename(Year=`bs$Year`,User_rating=`samp$User_rating`,Total=n)
View(rating_year)

# Top 10 author that have published the books most
amazon_publish=bs%>%count(bs$Author)%>%top_n(10)%>%head(10)
amazon_publish=amazon_publish%>%rename(Total=n,Author=`bs$Author`)
amazon_publish=amazon_publish%>%arrange(Total)
View(amazon_publish)
class(amazon_publish$Total)
amazon_publish=data.frame(amazon_publish)
ggplot(data=amazon_publish,aes(x=Author,y=Total,fill=Total))+
geom_bar(stat = "identity")+theme(axis.text=element_text(size = 7))

# top 10 most expensive books by a author
exp_books=bs[!duplicated(bs$Author),]               # deleting duplicating values
exp_books=top_n(exp_books,10,Price)
View(exp_books)
exp_books=exp_books%>%select(Name,Author,Price,`User Rating`)%>%
  arrange(-Price)%>%head(10)%>%
  mutate(Author=recode(Author,"Gary Chapman"="G. Chapman"))


View(exp_books)

ggplot(data=exp_books,aes(x=Author,y=Price,fill=Price))+
  geom_bar(stat = "identity")+theme(axis.text=element_text(size = 7))  


#
exp_books1=samp%>%count(samp$Name)%>%top_n(10)
exp_books1
View(exp_books1)

#relation between Price and user rating
samp$User_rating = as.character(samp$User_rating) # changing data type
View(samp)
print(class(samp$User_rating))
ggplot(data=samp,aes(x=User_rating,y=Price,fill=User_rating))+
  geom_boxplot()

#highest review for a book
samp$User_rating = as.numeric(samp$User_rating)
h_review=samp%>%filter(Reviews==max(samp$Reviews))
View(h_review)

#lowest review for a book
l_review=samp%>%filter(Reviews==min(samp$Reviews))
View(l_review)

# prices distribution
ggplot(data=samp,aes(x=Price))+ geom_histogram(aes(y =..density..),
                                               colour = "red", 
                                               fill = "green",binwidth=2) +
  stat_function(fun=dnorm,args=list(mean=mean(samp$Price),sd=sd(samp$Price)))+
  xlim(-30,100)

# reviews distribution
ggplot(data=samp,aes(x=Reviews))+geom_histogram(aes(y=..density..),fill="red",
                                  col="green",binwidth=1500)+xlim(-30000,100000)+
  stat_function(fun=dnorm,args=list(mean=mean(samp$Reviews),sd=sd(samp$Reviews)))

#relation between genre and price
ggplot(data=samp,aes(x=Genre,y=Price,fill=Genre))+geom_boxplot()+
  facet_grid(~Year)

#relation between genre and User rating
ggplot(data=samp,aes(x=Genre,y=User_rating))+geom_point()

#relation between genre and Review
ggplot(data=samp,aes(y=Reviews,fill=Genre))+geom_boxplot()+facet_grid(~Genre)


#relation between genre and Review
ggplot(data=samp,aes(y=Reviews))+geom_boxplot(fill="red",col="blue")+
theme(axis.text=element_text(size=15),axis.title=element_text(size=18))





