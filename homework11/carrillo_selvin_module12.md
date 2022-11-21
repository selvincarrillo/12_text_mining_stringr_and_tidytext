---
title: 'Homework 11: Mining hurricane Harvey Tweets'
author: "Selvin Carrillo"
date: "created: November 17, 2022; updated: November 17, 2022"
output: 
  html_document: 
    keep_md: yes
---





**Background:**    
Hurricane Harvey made landfall as a category 4 storm in Texas on August 26, 2017. This storm resulted in widepsread and devastating flooding. Here, we analyze the tweets posted from August 17-29, 2017. 

**Objective:**  
- Apply text mining techniques to analyze Twitter data tagged with “Hurricane Harvey”.  
- Evaluate tweets posted before, during, and after the landfall of Hurricane Harvey in the U.S. 

**Motivating questions:**  
- When did Harvey-related tweets peak in relation to when the hurricane made landfall?    
- What are the 20 most commonly used words in the Hurricane Harvey tweets?  
- What are common words used in tweets that reference refineries?  
- How did the average sentiment of tweets change from August 17-29, 2017?


## Analysis  

**Part 1:** When did Harvey-related tweets peak in relation to when the hurricane made landfall?  

```r
# Each row in the dataset corresponds to one tweet
harvey_tweets <- read_csv("hurricane_harvey_tweets.csv") # homework11/hurricane_harvey_tweets.csv"

harvey_tweets %>% 
     #group_by(datetime) %>% 
     count(datetime, sort = TRUE) %>% 
     #ungroup() %>% 
     ggplot(aes(datetime, n)) + 
     geom_point(alpha = 0.5) + 
     theme_bw() + 
     geom_vline(xintercept = ymd_hms("2017-08-26 03:00:00"), 
                color = "red", 
                linewidth = 1.5) + 
     labs(x = NULL, 
          y = "Number of tweets")
```

![](carrillo_selvin_module12_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

**Part 2:** What are the 20 most commonly used words in the Hurricane Harvey tweets?  

```r
# custom stop words 
custom_stop_words <- 
     tibble(word = c("hurricane", "harvey", "hurricaneharvey", "http", "https", 
                     "html", "ift.tt", "pic.twitter.com", "twitter.com", 
                     "fb.me", "bit.ly", "dlvr.it", "youtube", "youtu.be"), 
            lexicon = c("custom"))

# bind custom and stop words
custom_stop_words <-  
     bind_rows(custom_stop_words, stop_words)

# summarize and create frequency words plot 
harvey_tweets %>% 
     mutate(linenumber = row_number(id)) %>% 
     unnest_tokens(word, tweet) %>% 
     anti_join(custom_stop_words) %>% 
     count(word, sort = TRUE) %>% 
     slice_max(n, n=20) %>% 
     ungroup() %>% 
     mutate(word = reorder(word, n)) %>% 
     
     # plot the top 20 words 
     ggplot(aes(n, word)) + 
     geom_col() +
     theme_bw() +
     labs(x = "n", 
          y = NULL)
```

```
## Joining, by = "word"
```

![](carrillo_selvin_module12_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

**Part 3:**  What are common words used in tweets that reference refineries?  

```r
harvey_tweets %>%
     filter(str_detect(tweet, pattern = "refinery|refineries")) %>% 
     
     # tokenize and remove stop words
     unnest_tokens(word, tweet) %>% 
     anti_join(custom_stop_words) %>% 
     
     # create the wordcloud
     count(word, sort = TRUE) %>% 
     with(wordcloud(word, n, max.words = 100))
```

```
## Joining, by = "word"
```

![](carrillo_selvin_module12_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

It seems that the tweets that reference "refinery" or "refineries" also include potential economic impacts. For example, the high frequency of words such as gas, oil, gasoline, business, rise, and price imply that people are more worried about economic issues than environmental impacts. 


**Part 4:** How did the average sentiment of tweets change from August 17-29, 2017?

```r
harvey_tweets %>% 
     mutate(linenumber = row_number(id)) %>% 
     unnest_tokens(word, tweet) %>% 
     anti_join(custom_stop_words) %>% 
     # join tokenized words with afinn lexicon 
     inner_join(get_sentiments("afinn")) %>% 
     # summarize average sentiment
     group_by(date) %>% 
     summarise(value = mean(value)) %>% 
     ungroup() %>% 
     # visualize average sentiment
     ggplot(aes(x = date, y = value)) +
     geom_col() + 
     theme_bw() +
     scale_x_date(date_breaks = "day", 
                  date_labels = "%d") +
     labs(x = "Day in August 2017",
          y = "Average sentiment")
```

```
## Joining, by = "word"
## Joining, by = "word"
```

![](carrillo_selvin_module12_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

Overall, the sentiment expressed on tweets was negative, except on days 17, 18, and 23, about hurricane Harvey. 
