#EMOJI ANALIZI
maybellinemoji <- stringi::stri_extract_all_regex(maybellinecomment$textDisplay, "\\p{So}|\\p{Sk}")
maybellinemoji <- maybellinemoji[!is.na(maybellinemoji)]
kac_farkli_emoji_kullanilmis1 <- length(unique(maybellinemoji))
showtext_auto()
tummaybellinemoji <- unlist(maybellinemoji)
maybelemoji_frekans <- table(tummaybellinemoji)

maybellineemoji_frekans_df <- data.frame(emoji = names(maybelemoji_frekans), frequency = as.numeric(maybelemoji_frekans))
str(maybellineemoji_frekans_df)


temiz_emoji_maybel <- maybellinemoji[sapply(maybellinemoji, is_valid_emoji)] 
maybel10emoji <- maybellineemoji_frekans_df %>%
  arrange(desc(frequency)) %>%
  head(10)

emoji_labelmaybel <- maybel10emoji$emoji
emoji_countsbaybel <- maybel10emoji$frequency
maybel10emoji$emoji

ggplot(data = maybel10emoji, aes(x = factor(emoji, levels = emoji), y = frequency)) +
  geom_bar(fill="blue" ,stat = "identity") +
  labs(title = "En ??ok Kullan??lan 10 Emoji", x = "Emoji", y = "Kullan??m S??kl??????") +
  scale_x_discrete(labels = maybel10emoji$emoji) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "grey"),
        axis.text.x = element_text(family = "EmojiOne", size = 40, color = "red"),
        axis.text.y = element_text(family = "EmojiOne", size = 40, color = "red"),
        plot.title = element_text(color = "purple", size = 25),
        axis.title.x = element_text(color = "purple", size = 20),
        axis.title.y = element_text(color = "purple", size = 20)) +
  coord_cartesian(ylim = c(0, max(maybel10emoji$frequency) * 1.1))

emoji_datamaybel <- data.frame(word = emoji_labelmaybel, freq = emoji_countsbaybel)
wordcloud2 (data = maybellineemoji_frekans_df, size= 0.7)