library(tidyverse)
library(readxl)

generate_image <- function(item, path) {
  
  id <- item$id
  
  item %>% 
    select(-id) %>% 
    gather(key, value, -c(size, position)) %>% 
    mutate(text_size = case_when((size == "kongruent" & key == "n1") ~ 24,
                                 (size == "kongruent" & key == "n2") ~ 48,
                                 (size == "inkongruent" & key == "n1") ~ 48,
                                 (size == "inkongruent" & key == "n2") ~ 24),
           text_position = case_when((position == "links" & key == "n1") ~ -0.5,
                                     (position == "links" & key == "n2") ~ 0.5,
                                     (position == "rechts" & key == "n1") ~ 0.5,
                                     (position == "rechts" & key == "n2") ~ -0.5)
    ) %>% 
    ggplot(aes(x = text_position, y = 1)) +
    geom_text(aes(label = value, size = text_size)) +
    theme_void() +
    theme(legend.position = "none") +
    expand_limits(x = c(-1.5, 1.5), y = c(0.9, 1.1)) +
    scale_size_continuous(limits = c(0, 50)) -> plot
  
  ggsave(paste0(path, "/item", id, ".svg"), plot, width = 2, height = 1.5)
}


# Bilder für Experiment ---------------------------------------------------

items <- read_excel("Zahlenpaare.xlsx") %>%
  mutate(id = row_number()) %>% 
  split(.$id)

walk(items, ~ generate_image(.x, "images"))


# Testbilder --------------------------------------------------------------

test_items <- read_excel("Zahlenpaare.xlsx") %>%
  mutate(id = row_number()) %>% 
  split(.$id)

walk(test_items, ~ generate_image(.x, "images"))


         