

rm(list = ls())


library(data.table)
library(stringr)
library(ggplot2)
library(scales)
library(ggsci)



data = fread(
    "nikopech_code/biobservations-covid19Info.tsv",
    encoding = "UTF-8"
)


# explore workplace closing ------------------------

data = data[order(data$stateProvince, data$date.cl, data$workplace_closing), ]

data$`year-month` = paste0(data$year, "-", data$month)
plot_data = data[, .N, by = date.cl]


workplace = data[, .(level = min(date.cl, na.rm = TRUE)), 
                         by = workplace_closing]


workplace = workplace[!is.na(workplace$workplace_closing), ]

workplace$workplace_closing = as.character(workplace$workplace_closing)


ggplot(data = plot_data, aes(x = date.cl, y = N)) +
    
    geom_col(alpha = 0.6) +
    
    geom_vline(data = workplace,
               aes(xintercept = level,
                   color = workplace_closing),
               linetype = "dotted",
               size = 1.3) +
    
    scale_color_npg() +

    scale_x_date(date_breaks = "1 month", 
                 labels = date_format("%b-%Y")) +
    
    scale_y_continuous(expand = c(0, 0),
                       trans = "log10") +
    

    
    theme_minimal() +
    
    theme(axis.text.x = element_text(angle = 90),
          axis.title.x = element_blank(),
          
          panel.grid = element_blank(),
          
          legend.position = "bottom",
          
          axis.line = element_line(),
          axis.ticks = element_line()) +
    
    labs(y = "Number of observations")
    
    



