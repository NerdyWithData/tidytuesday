library(readr)
library(tidyverse)
library(ggwordcloud)
library(tidytext)y
library(SnowballC)
library(udpipe)
library(ggridges)
library(multipanelfigure)
library(ggrepel)
library(extrafont)
  font_import("C:\\Users\\rober\\AppData\\Local\\Microsoft\\Windows\\Fonts")
  #font used is FinkHeavy - use a search to find the ttl online, download and change the directory above to the location on your machine
  
user_reviews <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/user_reviews.tsv')

#Download English Model (udpipe package)
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)

#Map Model to the unnested word values from th euser reviews
#This is how we find the adjectives
user_tokens <- user_reviews %>% unnest_tokens(word,text)
user_model <- as_tibble(udpipe_annotate(ud_model, x = critic_tokens$word))

#Code to remove stop words, obtain word sentiment, and filter for adjectives
user_reviews_augmented <- user_tokens %>% 
  filter(!word %in% stop_words$word) %>%
  inner_join(get_sentiments("bing")) %>%
  inner_join(user_model,by=c("word"="token")) %>%
  filter(upos %in% c("ADJ"))

#Simple code to provide the number of unique reviews mentioning the word along with avg rating and sentiment 
user_review_summary <- user_reviews_augmented %>%
  group_by(word) %>%
  summarize(mean_rating=mean(grade),
            sentiment=first(sentiment),
            mentions=n_distinct(user_name))

#Code I use to identify Negative Words and Positive words associated with each rating
words_of_interest <- user_review_words %>%
  filter((mentions>=20 & mean_rating>7 & sentiment=="positive") | (mentions>=20 & mean_rating<5 & sentiment=="negative")) %>%
  filter(word!="wrong") %>%
  select(word:sentiment)

#Code to get dataset for ridgeline charts
#Needed the data in this format for the ridgeline to work properly
density_data <- user_reviews_augmented %>%
  inner_join(words_of_interest) %>%
  group_by(word) %>%
  mutate(mentions=n_distinct(user_name))%>%
  distinct(user_name, word, sentiment, mean_rating, grade, mentions)

#Code for top chart
wordcloud <- ggplot() +
  geom_text_repel(aes(y=mentions,
                      x=mean_rating,
                      label=word,
                      color=sentiment,
                      size=log(mentions)),
                  data=user_review_summary,
                  position = position_jitter(),
                  family="FinkHeavy",
                  segment.color = 'transparent')+
  labs(title="Animal Crossing - What Do Gamers Think?",
       subtitle = "Analyzing Top Adjectives in User Reviews (X-Axis = Rating, Y-Axis = Mentions)")+
  scale_x_continuous(name="Average Rating", limits=c(1.5, 8.5))+
  scale_y_continuous(name="Mentions")+
  theme(text=element_text(family = "FinkHeavy"),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5,
                                  family = "FinkHeavy",
                                  size=24,
                                  color="grey25"),
        plot.subtitle = element_text(hjust=0.5,
                                     family="Calibri",
                                     size=16,
                                     color="grey40"),
        legend.position="none",
        panel.grid.major=element_line(color="grey90"),
        axis.ticks=element_blank(),
        axis.text=element_text(hjust = 0.5,
                               family = "FinkHeavy",
                               size=14,
                               color="grey25"),
        axis.title=element_blank())

#Code for bottom left chart
#Interesting part is the fill=..xx.. which allows to define the color gradient in the scale_fill_gradient2 function 
density_chart_neg <- ggplot() +
  geom_density_ridges_gradient(data=density_data %>% filter(sentiment=="negative"),
                               aes(x = grade,
                                   y = reorder(word, -mentions),
                                   fill = ..x..),
                               scale = 1.5,
                               rel_min_height = 0.001,
                               gradient_lwd=0.5) +
  scale_fill_gradient2(low = "#F8766D",
                       high = "#00BFC4",
                       midpoint=5)+
  theme_ridges(font_family = "FinkHeavy",
               font_size = 18)+
  theme(legend.position="none",
        axis.title = element_blank(),
        plot.title = element_text(hjust=0.5,
                                  family = "FinkHeavy",
                                  size=16,
                                  color="grey25"),
        axis.text=element_text(hjust = 0.5,
                               family = "FinkHeavy",
                               size=14,
                               color="grey25"),
        axis.text.y=element_text(color="#F8766D"))+
  labs(title="Negative Words Associated With Lower Ratings")

#code for bottom right chart - almost identical to the negative chart
density_chart_pos <- ggplot() +
  geom_density_ridges_gradient(data=density_data %>% filter(sentiment=="positive"),
                               aes(x = grade,
                                   y = reorder(word, -mentions),
                                   fill = ..x..),
                               scale = 1.5,
                               rel_min_height = 0.001,
                               gradient_lwd=0.5) +
  scale_fill_gradient2(low = "#F8766D",
                       high = "#00BFC4",
                       midpoint=5)+
  theme_ridges(font_family = "FinkHeavy",
               font_size = 18)+
  theme(legend.position="none",
        axis.title = element_blank(),
        plot.title = element_text(hjust=0.5,
                                  family = "FinkHeavy",
                                  size=16,
                                  color="grey25"),
        axis.text=element_text(hjust = 0.5,
                               family = "FinkHeavy",
                               size=14,
                               color="grey25"),
        axis.text.y=element_text(color="#00BFC4"))+
  labs(title="Positive Words Associated With Higher Ratings")

#Code used from multipanelfigure package to layout a panel and place plots within it
figure= multi_panel_figure(columns=2,rows=2,panel_label_type="none")
figure %<>%
  fill_panel(wordcloud,column=1:2,row=1) %<>%
  fill_panel(density_chart_neg,column=1,row=2) %<>%
  fill_panel(density_chart_pos,column=2,row=2)

figure

#Code to Save Plot
#ggsave(figure,file = "<YOUR DIRECTORY>\\myplot.png", dpi = "retina") # adjust dpi accordingly