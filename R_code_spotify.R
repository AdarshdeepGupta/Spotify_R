spotify<-read.csv("E:\\charts.csv")
head(spotify)
shape<-dim(spotify)
shape
counts<-table(spotify$artist)
sort_tb<-sort(counts)
head(sort_tb)
tail(sort_tb)

#group data by region
# Compute total streams by region
streams <- aggregate(streams ~ region, data = spotify, sum)

# Compute percent stream
streams$percent_streams <- streams$streams / sum(streams$streams)

# Rename regions with very little streams (< 0.01%) as 'Other'
streams$region <- ifelse(streams$percent_streams >= 0.01, streams$region, 'Other')

streams <- aggregate(percent_streams ~ region, data = streams, sum)
streams$percent_streams <- round(streams$percent_streams, digits = 3)
streams <- streams[order(streams$percent_streams), ]

print(streams)

#for pi chart based on streams by region
library(ggplot2)

# Create a pie chart
pie_chart <- ggplot(streams, aes(x = 1, y = percent_streams, fill = region)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Streams by region") +
  theme_minimal()

# Display the pie chart
print(pie_chart)

#creating probability distribution
histogram <- hist(spotify$rank, breaks = 10, plot = TRUE)

# Calculate the probability distribution by normalizing the histogram
prob_distribution <- data.frame(
  Rank = histogram$mids,
  Probability = histogram$counts / sum(histogram$counts)
)

# Print the probability distribution
print(prob_distribution)

#calculating condition probability for Chantaje (feat. Maluma)
song_of_interest <- "Chantaje (feat. Maluma)"
# Filter the data for December 2018
december_data <- subset(spotify, format(as.Date(date, format = "%Y-%m-%d"), "%Y-%m") == "2018-12")

total_songs <- nrow(december_data)
top_10_songs <- sum(december_data$rank <= 10)
conditional_probability <- top_10_songs / total_songs

# Print the conditional probability
cat("Conditional Probability of", song_of_interest, "coming in top 10 in December 2018: ", conditional_probability, "\n")

#To get bayesian probability 
# Specify the song and region of interest
song_of_interest <- "Toosie Slide"
region_of_interest <- "India"

# Calculate the Bayesian probability
# Probability of "Genda Phool (feat. Payal Dev)" trending in India given historical data
probability_trending <- sum(spotify$title == song_of_interest & spotify$region == region_of_interest ) / sum(spotify$title == song_of_interest & spotify$region == region_of_interest)

# Print the Bayesian probability
cat("Bayesian Probability of", song_of_interest, "trending in", region_of_interest, "is:", probability_trending, "\n")


# Filter the data for two specific regions (e.g., Argentina and India)
selected_regions <- c("Argentina", "India")
filtered_data <- subset(spotify, region %in% selected_regions)

# Specify the numerical variable you want to test (e.g., "streams")
numeric_variable <- "streams"

# Perform a two-sample t-test
t_test_result <- t.test(get(numeric_variable) ~ region, data = filtered_data)

# Print the results
cat("Two-Sample T-Test Results:\n")
cat("Test Statistic:", t_test_result$statistic, "\n")
cat("P-value:", t_test_result$p.value, "\n")

# Determine if the null hypothesis is rejected (typically, if p-value < 0.05)
if (t_test_result$p.value < 0.05) {
  cat("Reject the null hypothesis. There is a significant difference between the selected regions.\n")
} else {
  cat("Fail to reject the null hypothesis. There is no significant difference between the selected regions.\n")
}
