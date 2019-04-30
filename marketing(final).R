#install.packages("tidyverse")
library(tidyr)
library(ggplot2)
##setwd("C:/Users/joe/OneDrive - University of Florida/JOE - bioinformationist/marketing stuff/hscl_sm")
setwd("D:/OneDrive - University of Florida/JOE - bioinformationist/marketing stuff/hscl_sm")
post_data_column_definitions <- read.csv("post_data_column_definitions.csv")
post_data_column_definitions <- data.frame(t(post_data_column_definitions))
post_data_82016_122016 <- read.csv("post_data_82016_122016.csv")
post_data_12017_42017 <- read.csv("post_data_12017_42017.csv")
post_data_52017_82017 <- read.csv("post_data_52017_82017.csv")
post_data_92017_122017 <- read.csv("post_data_92017_122017.csv")
post_data_12018_42018 <- read.csv("post_data_12018_42018.csv")
twitter <- read.csv("TwitterMetrics_2016_2018.csv")

##Facebook statistics
post_data_82016_122016[,1:2]<-NULL
post_data_12017_42017[,1:2]<-NULL
post_data_52017_82017[,1:2]<-NULL
post_data_92017_122017[,1:2]<-NULL
post_data_12018_42018[,1:2]<-NULL

fb_num_post <- matrix(c(nrow(post_data_82016_122016), nrow(post_data_12017_42017), nrow(post_data_52017_82017), nrow(post_data_92017_122017), nrow(post_data_12018_42018)), nrow=5, ncol=1)
fb_num_post <- as.data.frame(fb_num_post)
colnames(fb_num_post) <- "number of posts"
fb_num_post$'posting period' <- c("8/2016-12/2016", "1/2017-4/2017", "5/2017-8/2017", "9/2017-12/2017", "1/2018-4/2018")
fb_num_post$`posting period`<-factor(fb_num_post$`posting period`, levels=c("8/2016-12/2016", "1/2017-4/2017", "5/2017-8/2017", "9/2017-12/2017", "1/2018-4/2018"))
fb_num_post_plot <- ggplot(fb_num_post, aes(x=fb_num_post$`posting period`, y=fb_num_post$`number of posts`))+geom_bar(stat="identity")+labs(x="Posting period", y="# Facbook posts")+scale_y_continuous(expand = c(0,0), limits = c(0,100))+theme_bw()+theme(panel.border=element_blank(), axis.line=element_line(colour="black"), panel.grid = element_blank(),text = element_text(size=14))
plot(fb_num_post_plot)
write.csv(fb_num_post, file="fb_num_post.csv")

post_total_impressions_82016_122016 <- sum(as.numeric(post_data_82016_122016$Lifetime.Post.Total.Impressions))
post_total_impressions_12017_42017 <- sum(as.numeric(post_data_12017_42017$Lifetime.Post.Total.Impressions))
post_total_impressions_52017_82017 <- sum(as.numeric(post_data_52017_82017$Lifetime.Post.Total.Impressions))
post_total_impressions_92017_122017 <- sum(as.numeric(post_data_92017_122017$Lifetime.Post.Total.Impressions))
post_total_impressions_12018_42018 <- sum(as.numeric(post_data_12018_42018$Lifetime.Post.Total.Impressions))
fb_total_post_impressions <- post_total_impressions_82016_122016+
                            post_total_impressions_12017_42017+
                            post_total_impressions_52017_82017+
                            post_total_impressions_92017_122017+
                            post_total_impressions_12018_42018
fb_post_impressions <- matrix(c(post_total_impressions_82016_122016,post_total_impressions_12017_42017,post_total_impressions_52017_82017,post_total_impressions_92017_122017,post_total_impressions_12018_42018),nrow=5,ncol=1)
fb_post_impressions <- as.data.frame(fb_post_impressions)
colnames(fb_post_impressions) <- "post impressions"
fb_post_impressions$'posting period' <- c("8/2016-12/2016", "1/2017-4/2017", "5/2017-8/2017", "9/2017-12/2017", "1/2018-4/2018")
fb_post_impressions$`posting period` <- factor(fb_post_impressions$`posting period`, levels=c("8/2016-12/2016", "1/2017-4/2017", "5/2017-8/2017", "9/2017-12/2017", "1/2018-4/2018"))
fb_post_impressions_plot <- ggplot(fb_post_impressions, aes(x=fb_post_impressions$`posting period`, y=fb_post_impressions$`post impressions`))+geom_bar(stat="identity")+scale_y_continuous(expand = c(0,0), limits = c(0,80000))+theme_bw()+theme(panel.border=element_blank(), axis.line=element_line(colour="black"), panel.grid = element_blank(),text = element_text(size=14))+labs(x="Post period", y="# Facebook impressions")
plot(fb_post_impressions_plot)
write.csv(fb_post_impressions, file="fb_post_impressions.csv")

post_total_reach_82016_122016 <- sum(as.numeric(post_data_82016_122016$Lifetime.Post.Total.Reach))
post_total_reach_12017_42017 <- sum(as.numeric(post_data_12017_42017$Lifetime.Post.Total.Reach))
post_total_reach_52017_82017 <- sum(as.numeric(post_data_52017_82017$Lifetime.Post.Total.Reach))
post_total_reach_92017_122017 <- sum(as.numeric(post_data_92017_122017$Lifetime.Post.Total.Reach))
post_total_reach_12018_42018 <- sum(as.numeric(post_data_12018_42018$Lifetime.Post.Total.Reach))
fb_total_post_reach <- post_total_reach_82016_122016+
  post_total_reach_12017_42017+
  post_total_reach_52017_82017+
  post_total_reach_92017_122017+
  post_total_reach_12018_42018
fb_post_reach <- matrix(c(post_total_reach_82016_122016,post_total_reach_12017_42017,post_total_reach_52017_82017,post_total_reach_92017_122017,post_total_reach_12018_42018),nrow=5,ncol=1)
fb_post_reach <- as.data.frame(fb_post_reach)
colnames(fb_post_reach) <- "post reach"
fb_post_reach$'posting period' <- c("8/2016-12/2016", "1/2017-4/2017", "5/2017-8/2017", "9/2017-12/2017", "1/2018-4/2018")
fb_post_reach$`posting period` <- factor(fb_post_reach$`posting period`, levels=c("8/2016-12/2016", "1/2017-4/2017", "5/2017-8/2017", "9/2017-12/2017", "1/2018-4/2018"))
fb_post_reach_plot <- ggplot(fb_post_reach, aes(x=fb_post_reach$`posting period`, y=fb_post_reach$'post reach'))+geom_bar(stat="identity")+scale_y_continuous(expand = c(0,0), limits = c(0,50000))+theme_bw()+theme(panel.border=element_blank(), axis.line=element_line(colour="black"), panel.grid = element_blank(),text = element_text(size=14))+labs(x="Post period", y="# Facebook reach")
plot(fb_post_reach_plot)
write.csv(fb_post_reach, file="fb_post_reach.csv")

post_total_engagement_82016_122016 <- sum(as.numeric(post_data_82016_122016$Lifetime.Engaged.Users))
post_total_engagement_12017_42017 <- sum(as.numeric(post_data_12017_42017$Lifetime.Engaged.Users))
post_total_engagement_52017_82017 <- sum(as.numeric(post_data_52017_82017$Lifetime.Engaged.Users))
post_total_engagement_92017_122017 <- sum(as.numeric(post_data_92017_122017$Lifetime.Engaged.Users))
post_total_engagement_12018_42018 <- sum(as.numeric(post_data_12018_42018$Lifetime.Engaged.Users))
fb_total_post_engagement <- post_total_engagement_82016_122016+
  post_total_engagement_12017_42017+
  post_total_engagement_52017_82017+
  post_total_engagement_92017_122017+
  post_total_engagement_12018_42018
fb_post_engagement <- matrix(c(post_total_engagement_82016_122016,post_total_engagement_12017_42017,post_total_engagement_52017_82017,post_total_engagement_92017_122017,post_total_engagement_12018_42018),nrow=5,ncol=1)
fb_post_engagement <- as.data.frame(fb_post_engagement)
colnames(fb_post_engagement) <- "post engagement"
fb_post_engagement$'posting period' <- c("8/2016-12/2016", "1/2017-4/2017", "5/2017-8/2017", "9/2017-12/2017", "1/2018-4/2018")
fb_post_engagement$`posting period` <- factor(fb_post_engagement$`posting period`, levels=c("8/2016-12/2016", "1/2017-4/2017", "5/2017-8/2017", "9/2017-12/2017", "1/2018-4/2018"))
fb_post_engagement_plot <- ggplot(fb_post_engagement, aes(x=fb_post_engagement$`posting period`, y=fb_post_engagement$`post engagement`))+geom_bar(stat="identity")+scale_y_continuous(expand = c(0,0), limits = c(0,2000))+theme_bw()+theme(panel.border=element_blank(), axis.line=element_line(colour="black"), panel.grid = element_blank(),text = element_text(size=14))+labs(x="Post period", y="# Facebook engagements")
plot(fb_post_engagement_plot)
write.csv(fb_post_engagement, file="fb_post_engagement.csv")

##Twitter statistics
twitter_82016_122016<-twitter[1:9,]
twitter_12017_42017<-twitter[10:23,]
twitter_52017_82017<-twitter[24:53,]
twitter_92017_122017<-twitter[54:98,]
twitter_12018_42018<-twitter[99:148,]
twitter_post_82016_122016<-nrow(twitter_82016_122016)
twitter_post_12017_42017<-nrow(twitter_12017_42017)
twitter_post_52017_82017<-nrow(twitter_52017_82017)
twitter_post_92017_122017<-nrow(twitter_92017_122017)
twitter_post_12018_42018<-nrow(twitter_12018_42018)
num_twitter_posts<-matrix(c(twitter_post_82016_122016,twitter_post_12017_42017,
                            twitter_post_52017_82017,twitter_post_92017_122017,
                            twitter_post_12018_42018), nrow=5, ncol=1)
num_twitter_posts<-as.data.frame(num_twitter_posts)
colnames(num_twitter_posts)<-"number of posts"
num_twitter_posts$'posting period'<-c("8/2016-12/2016", "1/2017-4/2017", "5/2017-8/2017", "9/2017-12/2017", "1/2018-4/2018")
num_twitter_posts$`posting period` <- factor(num_twitter_posts$`posting period`, levels=c("8/2016-12/2016", "1/2017-4/2017", "5/2017-8/2017", "9/2017-12/2017", "1/2018-4/2018"))
num_twitter_posts_plot <- ggplot(num_twitter_posts, aes(x=num_twitter_posts$`posting period`, y=num_twitter_posts$`number of posts`))+geom_bar(stat="identity")+labs(x="Posting period", y="Number of posts")+scale_y_continuous(expand = c(0,0), limits = c(0,60))+theme_bw()+theme(panel.border=element_blank(), axis.line=element_line(colour="black"), panel.grid = element_blank(),text = element_text(size=14))+labs(x="Posting period", y="# Twitter posts")
plot(num_twitter_posts_plot)
write.csv(num_twitter_posts, file="num_twitter_posts.csv")

twitter_impressions_82016_122016 <- sum(as.numeric(twitter_82016_122016$impressions))
twitter_impressions_12017_42017 <- sum(as.numeric(twitter_12017_42017$impressions))
twitter_impressions_52017_82017 <- sum(as.numeric(twitter_52017_82017$impressions))
twitter_impressions_92017_122017 <- sum(as.numeric(twitter_92017_122017$impressions))
twitter_impressions_12018_42018 <- sum(as.numeric(twitter_12018_42018$impressions))
twitter_total_impressions <- twitter_impressions_82016_122016+twitter_impressions_12017_42017+twitter_impressions_52017_82017+twitter_impressions_92017_122017+twitter_impressions_12018_42018
twitter_post_impressions <- matrix(c(twitter_impressions_82016_122016,twitter_impressions_12017_42017,twitter_impressions_52017_82017,twitter_impressions_92017_122017,twitter_impressions_12018_42018),nrow=5,ncol=1)
twitter_post_impressions <- as.data.frame(twitter_post_impressions)
colnames(twitter_post_impressions) <- "post impressions"
twitter_post_impressions$'posting period' <- c("8/2016-12/2016", "1/2017-4/2017", "5/2017-8/2017", "9/2017-12/2017", "1/2018-4/2018")
twitter_post_impressions$`posting period` <- factor(twitter_post_impressions$`posting period`, levels=c("8/2016-12/2016", "1/2017-4/2017", "5/2017-8/2017", "9/2017-12/2017", "1/2018-4/2018"))
twitter_post_impressions_plot <- ggplot(twitter_post_impressions, aes(x=twitter_post_impressions$`posting period`, y=twitter_post_impressions$`post impressions`))+geom_bar(stat="identity")+scale_y_continuous(expand = c(0,0), limits = c(0,40000))+theme_bw()+theme(panel.border=element_blank(), axis.line=element_line(colour="black"), panel.grid = element_blank(),text = element_text(size=14))+labs(x="Post period", y="# Twitter impressions")
plot(twitter_post_impressions_plot)
write.csv(twitter_post_impressions, file="twitter_post_impressions.csv")

twitter_engagements_82016_122016 <- sum(as.numeric(twitter_82016_122016$engagements))
twitter_engagements_12017_42017 <- sum(as.numeric(twitter_12017_42017$engagements))
twitter_engagements_52017_82017 <- sum(as.numeric(twitter_52017_82017$engagements))
twitter_engagements_92017_122017 <- sum(as.numeric(twitter_92017_122017$engagements))
twitter_engagements_12018_42018 <- sum(as.numeric(twitter_12018_42018$engagements))
twitter_total_engagements <- twitter_engagements_82016_122016+twitter_engagements_12017_42017+twitter_engagements_52017_82017+twitter_engagements_92017_122017+twitter_engagements_12018_42018
twitter_post_engagements <- matrix(c(twitter_engagements_82016_122016,twitter_engagements_12017_42017,twitter_engagements_52017_82017,twitter_engagements_92017_122017,twitter_engagements_12018_42018),nrow=5,ncol=1)
twitter_post_engagements <- as.data.frame(twitter_post_engagements)
colnames(twitter_post_engagements) <- "post engagement"
twitter_post_engagements$'posting period' <- c("8/2016-12/2016", "1/2017-4/2017", "5/2017-8/2017", "9/2017-12/2017", "1/2018-4/2018")
twitter_post_engagements$`posting period` <- factor(twitter_post_engagements$`posting period`, levels=c("8/2016-12/2016", "1/2017-4/2017", "5/2017-8/2017", "9/2017-12/2017", "1/2018-4/2018"))
twitter_post_engagements_plot <- ggplot(twitter_post_engagements, aes(x=twitter_post_engagements$`posting period`, y=twitter_post_engagements$`post engagement`))+geom_bar(stat="identity")+scale_y_continuous(expand = c(0,0), limits = c(0,500))+theme_bw()+theme(panel.border=element_blank(), axis.line=element_line(colour="black"), panel.grid = element_blank(),text = element_text(size=14))+labs(x="Post period", y="# Twitter engagements")
plot(twitter_post_engagements_plot)
write.csv(twitter_post_engagements, file="twitter_post_engagements.csv")

sm_num_post <- rbind(fb_num_post, num_twitter_posts)
sm_num_post$media <- c(rep("Facebook",5), rep("Twitter",5))
sm_num_post_plot <- ggplot(sm_num_post, aes(x=sm_num_post$`posting period`, y=sm_num_post$`number of posts`, fill=sm_num_post$media))+geom_bar(stat="identity", position="dodge")+scale_y_continuous(expand=c(0,0), limits=c(0,100))+theme_bw()+theme(panel.border=element_blank(), axis.line=element_line(colour="black"), panel.grid=element_blank(), text=element_text(size=14))+labs(x="Posting period", y="Number of posts", fill="media")+scale_fill_manual(values=c("gray", "darkorange2"))
plot(sm_num_post_plot)
write.csv(sm_num_post, file="sm_num_post.csv")

sm_impressions <- rbind(fb_post_impressions, twitter_post_impressions)
sm_impressions$media <- c(rep("Facebook",5), rep("Twitter",5))
sm_impressions_plot <- ggplot(sm_impressions, aes(x=sm_impressions$`posting period`, y=sm_impressions$`post impressions`, fill=sm_impressions$media))+geom_bar(stat="identity", position="dodge")+scale_y_continuous(expand=c(0,0), limits=c(0,80000))+theme_bw()+theme(panel.border=element_blank(), axis.line=element_line(colour="black"), panel.grid=element_blank(), text=element_text(size=14))+labs(x="Posting period", y="Number of impressions", fill="media")+scale_fill_manual(values=c("gray", "darkorange2"))
plot(sm_impressions_plot)
write.csv(sm_impressions, file="sm_impressions.csv")

sm_engagement <- rbind(fb_post_engagement, twitter_post_engagements)
sm_engagement$media <- c(rep("Facebook",5), rep("Twitter",5))
sm_engagement_plot <- ggplot(sm_engagement, aes(x=sm_engagement$`posting period`, y=sm_engagement$`post engagement`, fill=sm_engagement$media))+geom_bar(stat="identity", position="dodge")+scale_y_continuous(expand=c(0,0), limits=c(0,2000))+theme_bw()+theme(panel.border=element_blank(), axis.line=element_line(colour="black"), panel.grid=element_blank(), text=element_text(size=14))+labs(x="Posting period", y="Number of engagements", fill="media")+scale_fill_manual(values=c("gray", "darkorange2"))
plot(sm_engagement_plot)
write.csv(sm_engagement, file="sm_engagements.csv")
