library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(colorspace)
library(ggtext)

urlList <- c('https://www.data.qld.gov.au/dataset/2785b0e3-7926-4f23-bd13-7c81b232b744/resource/2b3b4000-c3de-476d-9f27-c6ae2e4d7267/download/top-100-baby-names-2023.csv',
'https://www.data.qld.gov.au/dataset/2785b0e3-7926-4f23-bd13-7c81b232b744/resource/9368a6bb-b6ae-4e47-a4f5-48299333047d/download/top-100-baby-names-2022.csv',
'https://www.data.qld.gov.au/dataset/2785b0e3-7926-4f23-bd13-7c81b232b744/resource/7a48d06c-d19d-42e2-8800-1686814c6f04/download/top-100-baby-names-2021.csv',
'https://www.data.qld.gov.au/dataset/2785b0e3-7926-4f23-bd13-7c81b232b744/resource/0166ada0-9477-45cd-8f39-777dc09dea91/download/top-100-baby-names-2020.csv',
'https://www.data.qld.gov.au/dataset/2785b0e3-7926-4f23-bd13-7c81b232b744/resource/8bad9e6f-1941-4890-8bda-143fef4156cb/download/bdm-top-100-baby-names-2019.csv',
'https://www.data.qld.gov.au/dataset/2785b0e3-7926-4f23-bd13-7c81b232b744/resource/9bd4e10a-c2f6-4206-8011-d6830eb070a1/download/20190129_bdm_top-100-baby-names-2018.csv',
'https://www.data.qld.gov.au/dataset/2785b0e3-7926-4f23-bd13-7c81b232b744/resource/060259c8-1cea-4b89-b897-7dd584ab9469/download/20180123_bdm_top-100-baby-names-2017.csv',
'https://www.data.qld.gov.au/dataset/2785b0e3-7926-4f23-bd13-7c81b232b744/resource/3180a908-f18a-4597-a153-d0cd10c70dc6/download/20170119_bdm_top-100-baby-names-2016.csv',
'https://www.data.qld.gov.au/dataset/2785b0e3-7926-4f23-bd13-7c81b232b744/resource/8e2e0723-c9b5-438b-b385-050bcd0a3f6e/download/20160201_bdm_top-100-baby-names-2015.csv',
'https://www.data.qld.gov.au/dataset/2785b0e3-7926-4f23-bd13-7c81b232b744/resource/12c0176d-0433-437d-a63c-ed4f3d133268/download/20150309_bdm_top-100-baby-names-2014.csv',
'https://www.data.qld.gov.au/dataset/2785b0e3-7926-4f23-bd13-7c81b232b744/resource/4e9d1f33-6b1b-4e1d-bfde-85066e7c9b44/download/20150309_bdm_top-100-baby-names-2013.csv',
'https://www.data.qld.gov.au/dataset/2785b0e3-7926-4f23-bd13-7c81b232b744/resource/97dc30b8-9a6a-4bf4-bf83-d7102816888e/download/20150309_bdm_top-100-baby-names-2012.csv',
'https://www.data.qld.gov.au/dataset/2785b0e3-7926-4f23-bd13-7c81b232b744/resource/6822d8ab-a556-4bf2-b047-d1ba341c1649/download/20150309_bdm_top-100-baby-names-2011.csv',
'https://www.data.qld.gov.au/dataset/2785b0e3-7926-4f23-bd13-7c81b232b744/resource/f2ac63e0-939d-4394-bd76-3cd6fe07f9ea/download/20150309_bdm_top-100-baby-names-2010.csv',
'https://www.data.qld.gov.au/dataset/2785b0e3-7926-4f23-bd13-7c81b232b744/resource/4f52b297-facb-4e51-bac4-0fca47680fbd/download/20160308_bdm_top-100-baby-names-2009.csv',
'https://www.data.qld.gov.au/dataset/2785b0e3-7926-4f23-bd13-7c81b232b744/resource/9dc10826-e47d-4157-835b-bb733e491a14/download/20160308_bdm_top-100-baby-names-2008.csv',
'https://www.data.qld.gov.au/dataset/2785b0e3-7926-4f23-bd13-7c81b232b744/resource/ce3e7a0b-535b-4dc2-a750-b2390e63d873/download/20160308_bdm_top-100-baby-names-2007.csv',
'https://www.data.qld.gov.au/dataset/2785b0e3-7926-4f23-bd13-7c81b232b744/resource/f9fb4660-c4a5-455b-81bb-64cc8ef9b8c3/download/20160308_bdm_top-100-baby-names-2006.csv',
'https://www.data.qld.gov.au/dataset/2785b0e3-7926-4f23-bd13-7c81b232b744/resource/3ae0300d-4835-4829-b3d7-69ed0792bc59/download/20180718_bdm_top-100-baby-names-1960-to-2005.csv')


names <- list()

for (i in 1:(length(urlList) - 1)) {
  names[[i]] <- read_csv(urlList[i]) |> 
    mutate(year = 2024 - i)
}

compiledNames <- bind_rows(names) 

names(compiledNames) <- c('name', 'n', 'name', 'n', 'year')

girls <- compiledNames[, c(1, 2, 5)] |> 
  mutate(sex = 'Female') |> 
  relocate(name, sex, year, n)

boys <- compiledNames[, c(3, 4, 5)] |> 
  mutate(sex = 'Male') |> 
  relocate(name, sex, year, n)

historical <- read_csv(urlList[length(urlList)]) |> 
  mutate(Name = tools::toTitleCase(tolower(Name)))

names(historical) <- names(girls)


namesData <- rbind(girls, boys, historical)

plotData <- namesData |> 
  filter(name %in% c('Luke', 'Harry', 'Bella', 'Leah'))

labData <- plotData |> 
  group_by(name) |> 
  arrange(year) |> 
  slice_tail(n = 1)
 
# Plot the data

myCols <- c("#CC5A71", "#96ADC8", "#F0F757", "#78FECF")
backgroundCol <- '#1B1725'
textCol <- '#F1ECCE'

ggplot() +
  geom_line(
    data = plotData,
    aes(x=year, y=n, group=name, color = name),
    linewidth = 1.2) +
  geom_point(
    data = labData,
    aes(x=year, y=n, group=name, color = name, fill = name),
    shape = 21, size = 2) +
  geom_text(
    data = labData,
    aes(x=year, y=n, group=name, color = name, label = name),
    hjust = -0.2,
    family = 'Comic Sans MS') +
  scale_color_manual(values = myCols) +
  scale_fill_manual(values = lighten(myCols, 0.90)) +
  scale_x_continuous(NULL, breaks = seq(1965, 2020, 5),
                     limits = c(1963, 2023)) +
  scale_y_continuous(NULL, 
                     labels = c('0', '100', '200', '300', '400', '500 birth registrations')) +
  labs(
    title = "Top 100 Queensland baby names",
    subtitle = "_Select names, 1966-2023_",
    caption = "**Source** data.qld.gov.au/dataset/top-100-baby-names") +
  coord_cartesian(clip = "off") +
  theme(
    plot.background = element_rect(color = 'transparent', fill = backgroundCol),
    panel.background = element_rect(fill = backgroundCol),
    legend.position = 'none',
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.1, color = textCol),
    axis.ticks = element_blank(),
    axis.text = element_text(color = textCol, size = 8),
    axis.text.y = element_text(vjust = -1, hjust = 0,
                               margin = margin(r=-80)),
    plot.title = element_text(color = textCol, size = 16),
    plot.subtitle = element_markdown(
      color = lighten(textCol, 0.9),
      margin = margin(b=20),
      size = 8),
    plot.caption = element_markdown(size = 6, 
                                    color = lighten(textCol, 0.8),
                                    margin = margin(t=10)),
    plot.title.position = "plot",
    plot.margin = margin(10, 30, 10, 10)
  )

ggsave(here::here('qld-baby-names.png'),
                  height = 5,
                  width = 7,
                  units = 'in')
  

# Save the data

write.csv(namesData,
          file = here::here('qld-baby-names-1960-2023.csv'))


namesData |> 
  filter(name %in% c('Luna', 'Archie', 'Oscar', 'Charlie')) |> 
  mutate(name = case_when(
    name == 'Charlie' & sex == 'Male' ~ 'Charlie (boy)',
    name == 'Charlie' & sex == 'Female' ~ 'Charlie (girl)',
    .default = name
  )) |> 
  bind_rows(data.frame(name = 'Charlie (girl)', sex = 'Female', year = 2023, n = 45)) |> 
  ggplot(aes(x = year, y = n, color = name)) +
  geom_line() +
  labs(title = "Perfectly good names appropriated by COVID puppies?") +
  coord_cartesian(ylim = c(50, 300))





