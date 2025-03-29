######## Mit diesem Skript werden die Super Spreader analysiert

K <- readRDS("Zuschnitt_Comments.rds")
  
### Umgang mit den "Super-Spreadern"
account_comment_counts <- K %>%
  group_by(authorDisplayName) %>%
  summarise(comment_count = n()) %>%
  arrange(desc(comment_count)) 


### Quantile und Boxplot zeigen das es eine extrem right-skewed Verteilung ist.
account_comment_counts%>%
  ggplot(., aes(y = comment_count))+
  geom_boxplot()

quantile(account_comment_counts$comment_count, probs = seq(0, 1, 0.1))


top_1_percent_threshold <- quantile(account_comment_counts$comment_count, probs = 0.99)

# Wieviel kommentieren die Top 1% der Kommentatoren?
top_1_percent_accounts <- account_comment_counts %>%
  filter(comment_count >= top_1_percent_threshold)
total_comments_top_1_percent <- sum(top_1_percent_accounts$comment_count)
total_comments <- sum(account_comment_counts$comment_count)
percentage_top_1_percent <- (total_comments_top_1_percent / total_comments) * 100
print(percentage_top_1_percent)

# 1% der Accounts postet ~26,1% aller Kommentare


# Wieviel kommentieren die Top 0.1% der Kommentatoren?
top_0.1_percent_threshold <- quantile(account_comment_counts$comment_count, probs = 0.999)
top_0.1_percent_accounts <- account_comment_counts %>%
  filter(comment_count >= top_0.1_percent_threshold)
total_comments_top_0.1_percent <- sum(top_0.1_percent_accounts$comment_count)
total_comments <- sum(account_comment_counts$comment_count)

# Calculate the percentage
percentage_top_0.1_percent <- (total_comments_top_0.1_percent / total_comments) * 100
print(percentage_top_0.1_percent)

# 0.1% der Accounts verfassen ~10.7% aller Kommentare
