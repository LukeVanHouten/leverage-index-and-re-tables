library(tidyverse)

leverage_index_df <- read.csv("specific_games.csv")

tex_sea_df <- read.csv("tex_sea_09_29_22.csv") %>%  # 661128  93  75
    filter(!(PA %in% c(16, 78, 83, 90, 97))) %>%
    mutate(half_inning = paste0(topbot, "_", gsub("\\? ", "", Inn))) %>%
    select(-Inn, -topbot, -Score, -PA)
cle_sea_df <- read.csv("cle_vs_sea_08_26_22.csv") %>%  # 662143  79  63
    filter(!(row_number() %in% c(7, 69, 76, 77, 79))) %>%
    mutate(half_inning = paste0(topbot, "_", gsub("\\? ", "", Inn))) %>%
    select(-Inn, -topbot)
atl_sea_df <- read.csv("atl_vs_sea_09_10_22.csv")  # 662088 63 NA
sea_hou_df <- read.csv("sea_vs_hou_06_08_22.csv") %>%  # 662760 79 NA
    filter(row_number() != 49)

compare_li_df <- cle_sea_df %>%
    mutate(my_li = filter(leverage_index_df, game_pk == 662143)$li) %>%
    select(half_inning, my_li, LI) %>%
    `colnames<-`(c("half_inning", "my_li", "fangraphs_li")) %>%
    mutate(my_li = round(my_li, 2), li_diff = my_li/ fangraphs_li, 
           n = row_number())
trend_compare_li_df <- compare_li_df %>%
    select(-my_li, -fangraphs_li, -n) %>%
    mutate(half_inning = factor(half_inning, 
                                levels = unique(compare_li_df$half_inning))) %>%
    group_by(half_inning) %>%
    summarize(li_diff = mean(li_diff), .groups = "drop") %>%
    mutate(n = row_number())
plot_compare_li_df <- compare_li_df %>%
    select(-half_inning) %>%
    melt(id = "n")

# tex_sea_model <- lm(li_diff ~ n, data = trend_compare_li_df)
# summary(tex_sea_model)
cle_sea_model <- lm(li_diff ~ n, data = trend_compare_li_df)
summary(cle_sea_model)
# atl_sea_model <- lm(li_diff ~ n, data = trend_compare_li_df)
# summary(atl_sea_model)
# sea_hou_model <- lm(li_diff ~ n, data = trend_compare_li_df)
# summary(sea_hou_model)

x_1 <- 0:17
x_2 <- 18:37
y_1 <- 1.5 - (0.05 * x_1)
y_2 <- ((y_1[length(y_1)] - 0.05) * log(x_2[1])) / log(x_2)
y <- c(y_1, y_2)

innings_factor <- factor(unique(linear_weights_df$half_inning))

scale_pred_df <- left_join(compare_li_df, data.frame(
    half_inning = trend_compare_li_df$half_inning, 
    scale = y[1:nrow(trend_compare_li_df)]), by = "half_inning") %>%
    mutate(my_li_better = round(my_li / scale, 2), 
           li_diff_better = round((my_li_better + 1) / (fangraphs_li + 1), 
                                  2)) %>%
    select(-half_inning, -my_li, -li_diff, -scale) %>%
    melt(id = "n")

ggplot(data = scale_pred_df, 
       mapping = aes(x = n, y = value, color = variable)) + 
    geom_point() + 
    geom_path() +
    geom_hline(yintercept = 1, colour = "black") +
    # geom_vline(xintercept = 63, colour = "red") +
    scale_x_continuous(breaks = 1:nrow(compare_li_df)) +
    scale_y_continuous(breaks = seq(0, 7.75, length.out = 32))

ggplot(data = trend_compare_li_df, mapping = aes(x = n, y = li_diff)) +
    geom_point() +
    geom_path() +
    geom_smooth(formula = y ~ x, method = "lm", se = FALSE, col = "red") +
    geom_hline(yintercept = 1, color = "black") +
    geom_vline(xintercept = mean(1:nrow(trend_compare_li_df)), 
               color = "black")
