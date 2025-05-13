# ------------------------------
# Title: Gayon (2009) tables. 
# Author: Jacob Hamel-Mottiez
# Date: sys.Date()
# Description: Visualizations of the tables in Gayon (2009) paper. 
# Contact : For any issue please write to jacob.hamel-mottiez.1@ulaval.ca. 
# ------------------------------

# PACKAGES ----------------------------------------------------------------
library(tidyverse)
library(plotly)
library(readxl)
library(ggplot2)
library(ggrepel)
library(directlabels)
library(highcharter)
library(ggstream)
library(viridis)
library(patchwork)

# DATA --------------------------------------------------------------------
library(tibble)

tbl_1 <- tribble(
  ~Year, ~Studies_Biological_Theories, ~Philosophical_Questions, ~Historical_Articles, ~Total_Papers,
  1986, 5, 11, 3, 19,
  1987, 22, 3, 2, 28,
  1988, 10, 20, 2, 32,
  1989, 23, 8, 2, 33,
  1990, 8, 10, 3, 21,
  1991, 7, 10, 5, 22,
  1992, 13, 7, 2, 22,
  1993, 8, 9, 4, 20,
  1994, 9, 8, 2, 19,
  1995, 10, 9, 1, 20,
  1996, 9, 9, 2, 20,
  1997, 9, 9, 0, 18,
  1998, 11, 7, 6, 24,
  1999, 14, 6, 6, 25,
  2000, 10, 7, 4, 26,
  2001, 13, 7, 6, 26,
  2002, 17, 6, 3, 26
)
print(tbl_1)

long_tbl_1 <- tbl_1 %>% 
  pivot_longer(cols = -Year,  # Pivot all columns except 'Year'
               names_to = "Category", 
               values_to = "Count")




tbl_2 <- tribble(
  ~Year, ~Evolution, ~Taxonomy, ~Species, ~Ecology, ~Genetics, ~Other,
  1986, 3, 0, 0, 1, 0, 1,
  1987, 8, 0, 13, 0, 0, 1,
  1988, 3, 0, 6, 0, 0, 1,
  1989, 17, 0, 4, 1, 0, 1,
  1990, 3, 0, 1, 0, 0, 1,
  1991, 4, 0, 0, 0, 1, 1,
  1992, 7, 1, 0, 2, 1, 2,
  1993, 1, 0, 0, 2, 0, 1,
  1994, 0, 0, 0, 0, 0, 2,
  1995, 0, 0, 0, 0, 0, 1,
  1996, 6, 1, 2, 1, 0, 1,
  1997, 5, 0, 0, 1, 0, 2,
  1998, 3, 0, 0, 1, 1, 2,
  1999, 3, 0, 1, 0, 0, 1,
  2000, 7, 0, 0, 2, 1, 1,
  2001, 3, 0, 0, 0, 0, 2,
  2002, 9, 0, 2, 0, 1, 4
)
print(tbl_2)


tbl_2 <- tbl_2 |> rowwise() %>% 
  mutate(Total_Papers = sum(c_across(-Year), na.rm = TRUE)) %>% 
  ungroup()

long_tbl_2 <- tbl_2 %>% 
  pivot_longer(cols = -Year,  # Pivot all columns except 'Year'
               names_to = "Category", 
               values_to = "Count")


tbl_3 <- tribble(
  ~Year, ~Evolutionary_Epistemology, ~Ethics_Biology, ~Nature_Culture, ~Function_Design, ~Reflexions_Biology, ~Other,
  1986, 2, 7, 1, 1, 0, 0,
  1987, 19, 1, 0, 0, 1, 0,
  1988, 1, 3, 2, 0, 0, 1,
  1989, 17, 0, 2, 1, 0, 0,
  1990, 3, 0, 1, 1, 0, 0,
  1991, 0, 2, 1, 0, 1, 0,
  1992, 2, 3, 3, 1, 0, 1,
  1993, 3, 3, 0, 1, 1, 0,
  1994, 2, 3, 1, 0, 0, 0,
  1995, 2, 3, 0, 0, 1, 0,
  1996, 5, 1, 0, 3, 1, 2,
  1997, 1, 5, 0, 1, 0, 0,
  1998, 2, 2, 0, 0, 2, 1,
  1999, 0, 3, 0, 2, 1, 1,
  2000, 2, 1, 0, 2, 2, 1,
  2001, 1, 0, 0, 2, 1, 3,
  2002, 0, 0, 0, 1, 2, 3
)
print(tbl_3)

tbl_3 <- tbl_3 |> rowwise() %>% 
  mutate(Total_Papers = sum(c_across(-Year), na.rm = TRUE)) %>% 
  ungroup()

long_tbl_3 <- tbl_3 %>% 
  pivot_longer(cols = -Year,  # Pivot all columns except 'Year'
               names_to = "Category", 
               values_to = "Count")

 

label_plot1<- long_tbl_1 |> select(Category, Year, Count) |> filter(Category != "Total_Papers", Year == max(Year-1)) |> distinct()


# VISUALIZATION -----------------------------------------------------------
ggplot(long_tbl_1 |> filter(Category != "Total_Papers"), aes(x=Year, y = Count, group = Category, color = Category)) +
  #geom_point()+
  geom_smooth(se = FALSE, span = 1, aes(fill = Category), alpha = 0.2) +
  theme(legend.position = "none")+
  scale_x_discrete(expand=c(0, 1)) +
  geom_dl(aes(label = "Category"), method = "first.points", cex = 0.8)

  geom_label_repel(data = long_tbl_1 %>% filter(Year == max(Year)), aes(label=Category), show.legend = FALSE)

  #facet_grid(~Category)


label_plot2<- long_tbl_2 |> select(Category, Year, Count) |> filter(Category != "Total_Papers", Year == max(Year)) |> distinct()
ggplot(long_tbl_2 |> filter(Category != "Total_Papers"), aes(x=Year, y = Count, group = Category, color = Category)) +
  geom_smooth(se = FALSE, span = 1,  aes(fill = Category), linewidth = 1.5, alpha = 0.2)+ 
  geom_point()+
  geom_label_repel(data = label_plot2, aes(label=Category), show.legend = FALSE)
  #facet_grid(~Category)


p3 <- ggplot(long_tbl_3 |> filter(Category != "Total_Papers"), aes(x=Year, y = Count, group = Category, color = Category)) +
  #geom_point()+
  geom_smooth(se = FALSE, span = 1,  aes(fill = Category), alpha = 0.2) +
   #facet_grid(~Category) 
theme(legend.key.size = unit(1, 'cm'), 
  legend.text = element_text(size=20))

  
ggplot(long_tbl_3 |> filter(Category != "Total_Papers"), aes(x=Year, y = Count, group = Category, color = Category, fill = Category)) + 
  geom_stream()
  


x <- ggplot(long_tbl_1 |> filter(Category != "Total_Papers"), aes(x=Year, y = Count, group = Category, color = Category, fill = Category)) + 
  geom_stream() +
  scale_fill_viridis(discrete = TRUE, option = "D", direction = -1) + 
  scale_color_viridis(discrete = TRUE, option = "D", direction = -1) + 
  theme(legend.key.size = unit(2, 'cm'), 
        legend.text = element_text(size=30),
        axis.title.x =element_text(size=30),
        axis.text.x = element_text(size = 20),
        legend.position = "top",
        legend.title=element_blank(),
        axis.text.y = element_blank(),  # Remove y-axis ticks
        axis.ticks.y = element_blank(), # Remove y-axis tick marks
        axis.title.y = element_blank()  # Remove y-axis title
        ) 

y <- ggplot(long_tbl_2 |> filter(Category != "Total_Papers"), aes(x=Year, y = Count, group = Category, color = Category, fill = Category)) + 
  geom_stream()  +
  scale_fill_viridis(discrete = TRUE, option = "C") + 
  scale_color_viridis(discrete = TRUE, option = "C")+ 
  theme(legend.key.size = unit(1, 'cm'), 
        legend.text = element_text(size=20),
              axis.title.x =element_text(size=20),
              axis.text.x = element_text(size = 20),
              legend.position = "top",
              legend.title=element_blank(),
              axis.text.y = element_blank(),  # Remove y-axis ticks
              axis.ticks.y = element_blank(), # Remove y-axis tick marks
              axis.title.y = element_blank()  # Remove y-axis title
        ) 

z <- ggplot(long_tbl_3 |> filter(Category != "Total_Papers"), aes(x=Year, y = Count, group = Category, color = Category, fill = Category)) + 
  geom_stream() + 
  scale_fill_viridis(discrete = TRUE, option = "F") + 
  scale_color_viridis(discrete = TRUE, option = "F")+ 
  theme(legend.key.size = unit(1, 'cm'), 
        legend.text = element_text(size=20),
              axis.title.x =element_text(size=20),
              axis.text.x = element_text(size = 20),
              legend.position = "top",
              legend.title=element_blank(),
              axis.text.y = element_blank(),  # Remove y-axis ticks
              axis.ticks.y = element_blank(), # Remove y-axis tick marks
              axis.title.y = element_blank()  # Remove y-axis title
        ) 
  

x
y+z

# Custom plot 

highlighted <- ggplot(long_tbl_1 |> filter(Category != "Total_Papers"), aes(x=Year, y = Count, group = Category, color = Category, fill = Category)) + 
  geom_stream() + 
  scale_fill_manual(values = c("gray", "gray", "#400059")) + 
  scale_color_manual(values = c("darkgray", "darkgray", "#400059")) + 
  theme(legend.key.size = unit(1, 'cm'), 
        legend.text = element_text(size=15),
        legend.position = "top",
        legend.title=element_blank()) 


highlighted + y

highlighted + z








# With Highcharter --------------------------------------------------------
highchart() %>%
  hc_chart(type = "streamgraph") %>%
  hc_title(text = "Streamgraph ") %>%
  hc_xAxis(categories = tbl_1$Year, crosshair = TRUE) %>%
  hc_yAxis(visible = FALSE) %>%
  hc_plotOptions(series = list(marker = list(enabled = FALSE))) |> 
hc_add_series_list(
  lapply(names(tbl_1)[-1], function(cat) {
    list(
      name = cat,
      data = tbl_1[[cat]],
      stack = "stream"
    )
  })
)




