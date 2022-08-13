library(tidyverse)
library(openxlsx)
library(readr)
library(broom)
library(plotly)
library(corrplot)
library(ggpubr)
library(Racmacs)


names_df_a<-read.xlsx("ACMAP_nature_microbe.xlsx",sheet = "Sheet1")%>%
  select(Serum.panel,ID)
unique(names_df_a$Serum.panel)


a_df<-read.xlsx("ACMAP_nature_microbe.xlsx",sheet = "Sheet1")%>%
  select(-Serum.panel)%>%
  pivot_longer(-ID,names_to = "Virus",values_to = "NT50") %>% 
  mutate(NT50=round(NT50))%>%
  pivot_wider(names_from=ID, values_from=NT50) %>%
  mutate(Virus=gsub("\\.", " ", Virus))%>%
  mutate(Virus=gsub("Omicron BA ", "Omicron BA.", Virus))
a_df2<-a_df%>%select(-Virus)
rownames(a_df2)<-a_df$Virus
a_df2<-as.matrix(a_df2)

rownames(a_df2)

map_a <- acmap(
  titer_table = a_df2,
  ag_names = rownames(a_df2),
  sr_names = colnames(a_df2),
)

map_a <- optimizeMap(
  map                     = map_a,
  number_of_dimensions    = 2,
  number_of_optimizations = 1000,
  minimum_column_basis    = "none",
)
view(map_a)

map_a

map_a2<-relaxMap(map_a)
view(map_a2)

plotly_map_table_distance(map_a2)
map_a2 <- bootstrapMap(
  map                      = map_a,
  method = "resample",
  bootstrap_repeats        = 1000,
  optimizations_per_repeat = 100,
  ag_noise_sd              = 0.7,
  titer_noise_sd           = 0.7
)

boostrap_ag_coords_list <- mapBootstrap_agCoords(map_a2)
boostrap_sr_coords_list <- mapBootstrap_srCoords(map_a2)

agGroups(map_a2)<-c(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,1,1,1,1)
srGroups(map_a2)<-names_df_a$Serum.panel
plotly_map_table_distance(map_a2)

view(map_a2)

pdf(file = "Extended Data Fig 4a_2022.pdf", width = 16, height = 18)

plot(
  map_a2,
  optimization_number = 1,
  xlim = c(-4,4),
  ylim = c(-4,5),
  plot_ags = TRUE,
  plot_sr = TRUE,
  plot_labels = "antigens",
  plot_blobs = TRUE,
  show_procrustes = TRUE,
  show_error_lines = FALSE,
  plot_stress = FALSE,
  indicate_outliers = "arrowheads",
  grid.col = "grey90",
  grid.margin.col = "grey50",
  outlier.arrow.col = grid.col,
  fill.alpha = 0.8,
  outline.alpha = 0.8,
  label.offset = 0,
  padding = 1,
  cex = 1,
)
dev.off()

names_df_b<-read.xlsx("ACMAP_nature_microbe.xlsx",sheet = "Sheet2")%>%
  select(Serum.panel,ID)

b_df<-read.xlsx("ACMAP_nature_microbe.xlsx",sheet = "Sheet2")%>%
  select(-Serum.panel)%>%
  pivot_longer(-ID,names_to = "Virus",values_to = "NT50") %>% 
  mutate(NT50=round(NT50))%>%
  pivot_wider(names_from=ID, values_from=NT50) %>%
  mutate(Virus=gsub("\\.", " ", Virus))%>%
  mutate(Virus=gsub("Omicron BA ", "Omicron BA.", Virus))
b_df2<-b_df%>%select(-Virus)
rownames(b_df2)<-b_df$Virus
b_df2<-as.matrix(b_df2)

rownames(b_df2)

map_b <- acmap(
  titer_table = b_df2,
  ag_names = rownames(b_df2),
  sr_names = colnames(b_df2),
)

map_b <- optimizeMap(
  map                     = map_b,
  number_of_dimensions    = 2,
  number_of_optimizations = 1000,
  minimum_column_basis    = "none"
)
view(map_b)
map_b

map_b2<-relaxMap(map_b)
view(map_b2)

plotly_map_table_distance(map_b2)
map_b2 <- bootstrapMap(
  map                      = map_b,
  method = "resample",
  bootstrap_repeats        = 1000,
  optimizations_per_repeat = 100,
  ag_noise_sd              = 0.7,
  titer_noise_sd           = 0.7
)

boostrap_ag_coords_list <- mapBootstrap_agCoords(map_b2)
boostrap_sr_coords_list <- mapBootstrap_srCoords(map_b2)

agGroups(map_b2)<-c(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,1,1,1,1)
srGroups(map_b2)<-names_df_b$Serum.panel
plotly_map_table_distance(map_b2)

view(map_b2)
pdf(file = "Extended Data Fig 4b_2022.pdf", width = 10, height = 12)

plot(
  map_b2,
  optimization_number = 1,
  xlim = c(-3,2),
  ylim = c(-3,3),
  plot_ags = TRUE,
  plot_sr = TRUE,
  plot_labels = "antigens",
  plot_blobs = TRUE,
  show_procrustes = TRUE,
  show_error_lines = FALSE,
  plot_stress = FALSE,
  indicate_outliers = "arrowheads",
  grid.col = "grey90",
  grid.margin.col = "grey50",
  outlier.arrow.col = grid.col,
  fill.alpha = 0.8,
  outline.alpha = 0.8,
  label.offset = 0,
  padding = 1,
  cex = 1,
)
dev.off()

