# 20 Oct

# Skittles
skittles <- data_frame(colors = c("green", "purple", "yellow", "orange", "red"), 
                       count = c(18, 23, 13, 24, 20),
                       proportion = count/sum(count), 
                       total = sum(count)) 

skittles_proptest <- skittles %>%
  group_by(colors) %>%
  summarize(p_val = prop.test(count, total, 1/5)$p.val)

#ggplot(skittles) + geom_col(aes(x = colors, y = count, fill = colors))

#ggplot(skittles) + geom_col(aes(x = colors, y = count, fill = colors)) + scale_fill_manual(values=c("green", "orange", "purple", "red", "yellow"))

ggplot(skittles) + geom_col(aes(x = colors, y = proportion, fill = colors)) + scale_fill_manual(values=c("green", "orange", "purple", "red", "yellow"))

# M&Ms 

mm <- data_frame(colors = c("yellow", "blue", "red", "green", "orange", "brown"),
                 count = c(21, 14, 21, 29, 29, 14),
                 proportion = count/sum(count),
                 total = sum(count))

mm.prop.test <- mm %>% 
  group_by(colors) %>%
  summarize(p_val = prop.test(count, total, 1/6)$p.val)

ggplot(mm) + geom_col(aes(x = colors, y = proportion, fill = colors)) + scale_fill_manual(values=c("blue", "brown", "green", "orange", "red", "yellow"))
