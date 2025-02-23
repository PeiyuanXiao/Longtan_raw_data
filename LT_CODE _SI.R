# Library R packages------------------------------------------------------------
library("readxl")
library("tidyverse")
library("ggpmisc")
library("ggtext")
library("forcats")
library("cowplot")
library("plot3D")

# Import the raw data-----------------------------------------------------------
qn_scraper <-  read_excel("Longtan_lithic_tools.xlsx", sheet = 1) %>%
  mutate(Layer = as.character(Layer))
odn_scraper <- read_excel("Longtan_lithic_tools.xlsx", sheet = 2) %>%
  mutate(Level = as.character(Level))
notch_denti <- read_excel("Longtan_lithic_tools.xlsx", sheet = 3) %>%
  mutate(Layer = as.character(Layer))
mis_tool <- read_excel("Longtan_lithic_tools.xlsx", sheet = 4)
techno_flake <- read_excel("Longtan_lithic_data_flakes.xlsx", sheet = 1)
rsp_flake <- read_excel("Longtan_lithic_data_flakes.xlsx", sheet = 2)
odn_flake <- read_excel("Longtan_lithic_data_flakes.xlsx", sheet = 3)
incomp_flake <- read_excel("Longtan_lithic_data_flakes.xlsx", sheet = 4)
core <- read_excel("Longtan_lithic_data_cores.xlsx")
waste <- read_excel("Longtan_lithic_data_waste_products.xlsx") %>%
  mutate(Length = as.double(Length))
coord <- read_excel("Longtan_lithic_data_coordinates.xlsx")

# Cores-------------------------------------------------------------------------

# Core size attribute
core_attribute <- core %>%
  filter(Typology %in% c("Quina_core", 
                         "Discoidal_core", 
                         "Core_on_flake", 
                         "Surface_core")) %>% 
  rename("Length (mm)" = Length,
         "Width (mm)" = Width,
         "Thickness (mm)" = Thickness,
         "Weight (g)" = Mass,
         "Platform angle" = Ave_Platform_angle)

core_size <- 
  core_attribute %>%
  select(
    Typology,
    "Length (mm)",
    "Width (mm)",
    "Thickness (mm)",
    "Weight (g)") %>%
  mutate(Typology = case_when(
    Typology ==  "Quina_core" ~ "Quina cores", 
    Typology ==  "Surface_core" ~ "Surface cores",   
    Typology ==  "Discoidal_core" ~ "Discoidal cores",
    Typology ==  "Core_on_flake" ~ "C-O-Fs",
  )) %>%
  drop_na() %>%
  pivot_longer(-Typology) %>%
  mutate(Typology = factor(Typology, 
                           levels = c("Quina cores", 
                                      "Discoidal cores", 
                                      "C-O-Fs", 
                                      "Surface cores"))) %>%
  mutate(name = factor(name, levels = c("Length (mm)", 
                                        "Width (mm)", 
                                        "Thickness (mm)", 
                                        "Weight (g)")))

ggplot(core_size, 
       aes(x = Typology, 
           y = value, 
           fill = Typology)) +
  geom_violin(
    fill = "lightgrey",
    alpha = 1,
    linewidth = 0,
    color = "white",
    adjust = 2
  ) +
  stat_boxplot(geom = "errorbar", width = 0.1, linewidth = 0.5) +
  geom_boxplot(
    aes(fill = Typology),
    alpha = 1,
    linewidth = 0.5,
    color = "black",
    width = 0.4
  ) +
  geom_point(
    stat = "summary",
    fun = "mean",
    shape = 19,
    size = 2,
    color = "black",
    show.legend = FALSE
  ) +
  scale_fill_manual(
    values = c(
      "C-O-Fs" = "#30A5C2",
      "Surface cores" = "#CDEBB3",
      "Quina cores" = "#21318C",
      "Discoidal cores" = "#1E80B8"
    )
  ) +
  scale_color_manual(
    values = c(
      "C-O-Fs" = "#30A5C2",
      "Surface cores" = "#CDEBB3",
      "Quina cores" = "#21318C",
      "Discoidal cores" = "#1E80B8"
    )
  ) +
  facet_wrap(~ name, scales = "free_y") +   
  theme_bw() +
  theme(legend.position = "none", 
        axis.title.x = element_blank()) +
  theme(axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        axis.title.y = element_text(size = 14),
        strip.text = element_text(size = 14),
        panel.grid.major = element_line(color = "white"), 
        panel.grid.minor = element_line(color = "white")) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  ylab("")

# Platform angle of cores
core_butt_angle <- core_attribute %>%
  mutate(Typology = case_when(
    Typology ==  "Quina_core" ~ "Quina cores", 
    Typology ==  "Surface_core" ~ "Surface cores",   
    Typology ==  "Discoidal_core" ~ "Discoidal cores",
    Typology ==  "Core_on_flake" ~ "C-O-Fs",
  )) %>%
  mutate(Typology = factor(Typology, 
                           levels = c("Quina cores",       
                                                "Discoidal cores",
                                                "C-O-Fs",
                                                "Surface cores")))

ggplot(core_butt_angle, aes(x = Typology, 
                           y = `Platform angle`, 
                           fill = Typology)) +
  geom_violin(
    fill = "lightgrey",
    alpha = 1,
    linewidth = 0,
    color = "white",
    adjust = 2
  ) +
  stat_boxplot(geom = "errorbar", width = 0.1, size = 0.5) +
  geom_boxplot(
    aes(fill = Typology),
    alpha = 1,
    linewidth = 0.5,
    color = "black",
    width = 0.4
  ) +
  geom_point(
    stat = "summary",
    fun = "mean",
    shape = 19,
    size = 2,
    color = "black",
    show.legend = FALSE
  ) +
  scale_fill_manual(
    values = c(
      "C-O-Fs" = "#30A5C2",
      "Surface cores" = "#CDEBB3",
      "Quina cores" = "#21318C",
      "Discoidal cores" = "#1E80B8"
    )
  ) +
  scale_color_manual(
    values = c(
      "C-O-Fs" = "#30A5C2",
      "Surface cores" = "#CDEBB3",
      "Quina cores" = "#21318C",
      "Discoidal cores" = "#1E80B8"
    )
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        panel.grid.major = element_line(color = "white"), 
        panel.grid.minor = element_line(color = "white")) +
  theme(legend.position = "none", axis.title.x = element_blank()) +
  ylab("Platform angle (°)")

# Flakes------------------------------------------------------------------------

rsf_clean <- 
  rsp_flake %>%
  select(`Length (mm)` = Length,
         `Width (mm)` = Width,
         `Thickness (mm)` = Thickness,
         `Weight (g)` = Mass,
         `Interior platform angle` = IPA,
         `Platform length (mm)` = Platform_width,
         `Platform width (mm)` = Platform_depth) %>%
  mutate(Typology = "Resharpening flakes") %>%
  drop_na()

tf_clean <- 
  techno_flake %>%
  select(Typology,
         `Length (mm)` = Length,
         `Width (mm)` = Width,
         `Thickness (mm)` = Thickness,
         `Weight (g)` = Mass,
         `Interior platform angle` = IPA,
         `Platform length (mm)` = Platform_width,
         `Platform width (mm)` = Platform_depth) %>%
  mutate(Typology = case_when(
    Typology ==  "Kombewa_flake" ~ "Kombewa flakes", 
    Typology ==  "Surface_flake" ~ "Surface flakes",   
    Typology ==  "Discoidal_flake" ~ "Discoidal flakes",
    Typology ==  "Quina_flake" ~ "Quina flakes",
  )) %>%
  drop_na() 

all_flakes_clean <- 
  bind_rows(rsf_clean,
            tf_clean)

all_flakes_clean_long <- 
  all_flakes_clean %>%
  select(c("Typology",
           "Length (mm)", 
           "Width (mm)", 
           "Thickness (mm)", 
           "Weight (g)", 
           "Interior platform angle", 
           "Platform length (mm)", 
           "Platform width (mm)")) %>%
  pivot_longer(-Typology) %>%
  mutate(Typology = factor(Typology, 
                           levels = c("Quina flakes", 
                                      "Discoidal flakes", 
                                      "Kombewa flakes", 
                                      "Surface flakes", 
                                      "Resharpening flakes"))) 

# Flake size attribute with post-hoc test   
ggplot(all_flakes_clean_long %>%
         filter(name != "Interior platform angle")) +
  aes(Typology, value) +
  geom_violin(fill = "lightgrey", 
              alpha = 1,
              linewidth = 0, 
              color = "white", 
              adjust = 2) + 
  stat_boxplot(geom = "errorbar", 
               width = 0.1, 
               size = 0.5) +
  geom_boxplot(aes(fill = Typology), 
               alpha = 1, 
               linewidth = 0.5, 
               color = "black", 
               width = 0.4) + 
  geom_point(stat = "summary", 
             fun = "mean", 
             shape = 19, 
             size = 2,
             color = "black", 
             show.legend = FALSE) +
  facet_wrap( ~ name, scales = "free_y") +
  ylab("") +
  xlab("") +
  scale_fill_manual(values = c( "Kombewa flakes" = "#30A5C2", 
                                "Surface flakes" = "#CDEBB3", 
                                "Quina flakes" = "#21318C", 
                                "Discoidal flakes" = "#1E80B8",
                                "Resharpening flakes" = "#EEF8B4" )) +
  scale_color_manual(values = c( "Kombewa flakes" = "#30A5C2", 
                                 "Surface flakes" = "#CDEBB3", 
                                 "Quina flakes" = "#21318C", 
                                 "Discoidal flakes" = "#1E80B8",
                                 "Resharpening flakes" = "#EEF8B4")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 13),
        axis.title.y = element_text(size = 14),
        strip.text = element_text(size = 14),
        panel.grid.major = element_line(color = "white"), 
        panel.grid.minor = element_line(color = "white")) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  stat_multcomp(geom = "text_pairwise",
                size = 3, 
                small.p = F,
                contrasts = "Dunnet") +
  labs(fill = "Typology") +
  theme(legend.position = "none", 
        axis.title.x = element_blank())

# Flake IPA with post-hoc test
ggplot(all_flakes_clean_long %>%
         filter(name == "Interior platform angle")) +
  aes(Typology, value) +
  geom_violin(fill = "lightgrey", 
              alpha = 1, linewidth = 0, color = "white", adjust = 2) + 
  stat_boxplot(geom = "errorbar", width = 0.1, size = 0.5) +
  geom_boxplot(aes(fill = Typology), 
               alpha = 1, 
               linewidth = 0.5, 
               color = "black", 
               width = 0.4) + 
  geom_point(stat = "summary", 
             fun = "mean", 
             shape = 19, 
             size = 2, 
             color = "black", 
             show.legend = FALSE) +
  ylab("Interior platform angle") +
  xlab("") +
  scale_fill_manual(values = c( "Kombewa flakes" = "#30A5C2", 
                                "Surface flakes" = "#CDEBB3", 
                                "Quina flakes" = "#21318C", 
                                "Discoidal flakes" = "#1E80B8",
                                "Resharpening flakes" = "#EEF8B4" )) +
  scale_color_manual(values = c( "Kombewa flakes" = "#30A5C2", 
                                 "Surface flakes" = "#CDEBB3", 
                                 "Quina flakes" = "#21318C", 
                                 "Discoidal flakes" = "#1E80B8",
                                 "Resharpening flakes" = "#EEF8B4")) +
  theme_bw() +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        axis.title.y = element_text(size = 14),
        strip.text = element_text(size = 14),
        panel.grid.major = element_line(color = "white"), 
        panel.grid.minor = element_line(color = "white")) +
  stat_multcomp(geom = "text_pairwise",
                size = 4, 
                small.p = F,
                contrasts = rbind(c(0, 0, 0, -1, 1),
                                  c(0, 0, -1, 1, 0),
                                  c(0, -1, 0, 1, 0),
                                  c(-1, 0, 0, 1, 0))) +
  labs(fill = "Typology") +
  theme(legend.position = "none", axis.title.x = element_blank()) +
  ylab("Interior platform angle (°)")

# Flake platform types 
flake_platform_type <- 
  bind_rows(techno_flake %>%
            select(`Platform type` = Platform_type, Typology),
            rsp_flake %>% 
            select(`Platform type` = Platform_type, Typology)) %>%
  mutate(Typology = case_when(
    Typology ==  "Kombewa_flake" ~ "Kombewa flakes", 
    Typology ==  "Surface_flake" ~ "Surface flakes",   
    Typology ==  "Discoidal_flake" ~ "Discoidal flakes",
    Typology ==  "Quina_flake" ~ "Quina flakes",
    Typology ==  "Resharpening_flake" ~ "Resharpening flakes"
  )) %>%
  drop_na(`Platform type`)

ppt <- ggplot(data = flake_platform_type %>%
                mutate(Typology = fct_infreq(Typology),
                       `Platform type` = factor(`Platform type`, 
                                                levels = c("Plain", 
                                                           "Dihedral", 
                                                           "Faceted", 
                                                           "Linear", 
                                                           "Natural"))) %>%
                group_by(`Platform type`) %>%
                count(Typology), 
              aes(Typology,
                  n,
                  group = `Platform type`,
                  fill = `Platform type`)) +
  geom_col() +
  geom_hline(yintercept = 0) +
  scale_fill_manual(
    values = c(
      "Faceted" = "#30A5C2",
      "Dihedral" = "#1E80B8",
      "Natural" = "#EEF8B4",
      "Linear" = "#CDEBB3",
      "Plain" = "#21318C")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        axis.title.y = element_text(size = 14),
        strip.text = element_text(size = 14),
        panel.grid.major = element_line(color = "white"), 
        panel.grid.minor = element_line(color = "white")) +
  theme(legend.position = "right") +
  coord_flip() +
  guides(fill = guide_legend("Platform types")) +
  xlab("") + ylab("Count")

# Dorsal scar patterns
flake_dsp_type <- 
  bind_rows(techno_flake %>% 
              select(`Dorsal scar pattern` = Dorsal_scar_pattern,
                     Typology),
            rsp_flake %>% 
              select(`Dorsal scar pattern` = Dorsal_scar_pattern,
                     Typology)) %>%
  mutate(Typology = case_when(
    Typology ==  "Kombewa_flake" ~ "Kombewa flakes", 
    Typology ==  "Surface_flake" ~ "Surface flakes",   
    Typology ==  "Discoidal_flake" ~ "Discoidal flakes",
    Typology ==  "Quina_flake" ~ "Quina flakes",
    Typology ==  "Resharpening_flake" ~ "Resharpening flakes"
  )) %>%
  mutate(`Dorsal scar pattern`= case_when(
    `Dorsal scar pattern` == "Unidirectional_proximal"~ "Unidirectional-proximal",
    `Dorsal scar pattern` == "Unidirectional_distal"~ "Unidirectional-distal",
    `Dorsal scar pattern` == "Centripetal"~ "Centripetal",
    `Dorsal scar pattern` == "Multi_direction"~ "Multi-direction",
    `Dorsal scar pattern` == "Lateral"~ "Lateral",
    `Dorsal scar pattern` == "Unidentified"~ "Unidentified",
    `Dorsal scar pattern` == "NA"~ "Unidentified",
    .default = `Dorsal scar pattern`
  )) %>% 
  drop_na(`Dorsal scar pattern`)

pdsp <- ggplot(data = flake_dsp_type %>%
                 mutate(Typology= fct_infreq(Typology),
                        `Dorsal scar pattern` = factor(`Dorsal scar pattern`, 
                                                       levels = c("Unidirectional-proximal", 
                                                                  "Unidirectional-distal", 
                                                                  "Lateral", 
                                                                  "Multi-direction", 
                                                                  "Centripetal",
                                                                  "Unidentified"))) %>%
                 group_by(`Dorsal scar pattern`) %>%
                 count(Typology), 
               aes(Typology,
                   n,
                   group = `Dorsal scar pattern`,
                   fill = `Dorsal scar pattern`)) +
  geom_col() +
  geom_hline(yintercept = 0) +
  scale_fill_manual(
    values = c(
      "Multi-direction" = "#CDEBB3",
      "Unidirectional-distal" = "#1E80B8",
      "Lateral" = "#30A5C2",
      "Unidentified" = "#FFD082",
      "Centripetal" = "#EEF8B4",
      "Unidirectional-proximal" = "#21318C")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        axis.title.y = element_text(size = 14),
        strip.text = element_text(size = 14),
        panel.grid.major = element_line(color = "white"), 
        panel.grid.minor = element_line(color = "white")) +
  theme(legend.position = "right") +
  coord_flip() +
  guides(fill = guide_legend("Dorsal scar pattern")) +
  xlab("") + ylab("Count") 

plot_grid(
  ppt, 
  pdsp, 
  ncol = 1, 
  rel_heights = c(1, 1), 
  align = "v"
)

# Tools-------------------------------------------------------------------------

tools_clean <- 
  bind_rows(list(
      `Quina scraper` =
        qn_scraper %>%
        select(Typology, Length, Width, Thickness, Mass),
      Scraper =
        odn_scraper %>%
        select(Typology, Length, Width, Thickness, Mass),
      N_D =
        notch_denti %>%
        select(Typology, Length, Width, Thickness, Mass),
      Misc = 
        mis_tool %>%
        select(Typology, Length, Width, Thickness, Mass)
    )) %>%
  drop_na() %>%
  pivot_longer(-Typology) %>%
  mutate(Typology = case_when(
    Typology == "Quina_scraper" ~ "Quina scrapers",
    Typology == "Ordinary_scraper" ~ "Scrapers",
    Typology == "Notch" ~ "Notches",
    Typology == "Denticulate" ~ "Denticulates",
    Typology == "Miscellaneous_tool" ~ "Misc"
  )) %>%
  mutate(name = case_when(
    name == "Length" ~ "Length (mm)",
    name == "Width" ~ "Width (mm)",
    name == "Thickness" ~ "Thickness (mm)",
    name == "Mass" ~ "Weight (g)"
  )) %>%
  mutate(Typology = factor(Typology, 
                           levels = c("Quina scrapers",
                                      "Scrapers",
                                      "Notches",
                                      "Denticulates",
                                      "Misc"))) %>%
  mutate(name = factor(name, 
                       levels = c("Length (mm)", 
                                  "Width (mm)", 
                                  "Thickness (mm)", 
                                  "Weight (g)")))

# Tool size attribute  
ggplot(tools_clean) +
  aes(Typology, value)+
  geom_violin(fill = "lightgrey",
              alpha = 1,
              linewidth = 0,
              color = "white",
              adjust = 2) +
  stat_boxplot(geom = "errorbar", width = 0.1, size = 0.5) +
  geom_boxplot(aes(fill = Typology),
               alpha = 1,
               linewidth = 0.5,
               color = "black",
               width = 0.4
  ) +
  geom_point(
    stat = "summary",
    fun = "mean",
    shape = 19,
    size = 2,
    color = "black",
    show.legend = FALSE
  ) +
  scale_fill_manual(
    values = c(
      "Notches" = "#30A5C2",
      "Denticulates" = "#CDEBB3",
      "Quina scrapers" = "#21318C",
      "Scrapers" = "#1E80B8",
      "Misc" = "#EEF8B4"
    )) +
  scale_color_manual(
    values = c(
      "Notches" = "#30A5C2",
      "Denticulates" = "#CDEBB3",
      "Quina scrapers" = "#21318C",
      "Scrapers" = "#1E80B8",
      "Misc" = "#EEF8B4"
    )) +
  facet_wrap(~name, scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        axis.title.y = element_text(size = 14),
        strip.text = element_text(size = 14),
        panel.grid.major = element_line(color = "white"), 
        panel.grid.minor = element_line(color = "white")) +
  ylab("") +
  theme(legend.position = "none", axis.title.x = element_blank()) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

# Scatter of size of stone artifacts--------------------------------------------

# Total types
integration <- function(data) {
  data %>%
    select(Typology, Length, Width) %>%
    drop_na()
}

SIZE_STONE_ARTIFACT <- 
  bind_rows(integration(qn_scraper),
            integration(odn_scraper),
            integration(notch_denti),
            integration(mis_tool),
            integration(techno_flake),
            integration(rsp_flake),
            integration(odn_flake),
            integration(incomp_flake),
            integration(core),
            integration(waste)
  ) %>%
  mutate(Typology = case_when(
    Typology %in% c('Quina_scraper', 
                    'Ordinary_scraper', 
                    'Notch', 
                    'Denticulate', 
                    'Miscellaneous_tool') ~ 'Tool',
    Typology %in% c('Kombewa_flake', 
                    'Surface_flake', 
                    'Discoidal_flake', 
                    'Quina_flake', 
                    'Resharpening_flake', 
                    'Ordinary_flake', 
                    'Incomplete_flake') ~ 'Flake',
    Typology %in% c('Surface_core', 
                    'Core_on_flake', 
                    'Discoidal_flake', 
                    'Discoidal_core', 
                    'Quina_core', 
                    'Expedient_core') ~ 'Core',
    TRUE ~ Typology 
  ))

SIZE_STONE_ARTIFACT$Typology <- factor(
  SIZE_STONE_ARTIFACT$Typology, 
  levels = c("Core", "Flake", "Tool", "Waste_product", "Manuport")
)

p1 <-
  ggplot(SIZE_STONE_ARTIFACT, 
         aes(x = Length, y = Width, fill = Typology)) +
  geom_point(shape = 21, alpha = 1, size = 2.5, color = "black", stroke = 0.25) + 
  geom_polygon(data = data.frame(Length = c(0, 50, 50, 0),
                                 Width = c(0, 0, 50, 50)),
    aes(x = Length, y = Width),
    fill = "transparent",   
    color = "black",          
    size = 0.35,
    linetype = "dashed") +
  labs(x = "Length (mm)", y = "Width (mm)") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 12),
        panel.grid.major = element_line(color = "white"), 
        panel.grid.minor = element_line(color = "white"),
        legend.position = c(0.95, 0.05), 
        legend.justification = c(1, 0),
        legend.key.size = unit(0.5, "cm"), 
        legend.background = element_rect(fill = "white", color = "black", size = 0.05), 
        legend.box.background = element_rect(color = "black") 
  ) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "Core" = "#21318C", 
      "Flake" = "#1E80B8", 
      "Tool" = "#00d8c7", 
      "Waste_product" = "#EEF8B4", 
      "Manuport" = "#CDEBB3"
    ),
    labels = c(
      "Core" = "Core",
      "Flake" = "Flake",
      "Tool" = "Tool",
      "Waste_product" = "Chunk & debris",
      "Manuport" = "Manuport"
    ))

# Core types
core$Typology <- factor(
  core$Typology, 
  levels = c("Quina_core", "Discoidal_core", "Core_on_flake", "Surface_core", "Expedient_core")
)

qc_data <- core %>%
  filter(Typology == "Quina_core") %>%
  arrange(Length, Width)

convex_hull_qc <- qc_data[chull(qc_data$Length, qc_data$Width), ]

p2 <-
  ggplot(core, 
       aes(x = Length, y = Width, fill = Typology)) +
  geom_polygon(data = convex_hull_qc, aes(fill = Typology, group = Typology), 
               alpha = 0.2, size = 0, color = "#21318C") +
  geom_point(shape = 21, color = "black", alpha = 1, size = 2.5, stroke = 0.25) + 
  labs(x = "Length (mm)", y = "Width (mm)") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 12),
        panel.grid.major = element_line(color = "white"), 
        panel.grid.minor = element_line(color = "white"),
        legend.position = c(0.95, 0.05), 
        legend.justification = c(1, 0),
        legend.key.size = unit(0.5, "cm"),  
        legend.background = element_rect(fill = "white", color = "black", size = 0.05), 
        legend.box.background = element_rect(color = "black")
  ) + 
  scale_fill_manual(
    name = NULL,
    values = c(
      "Quina_core" = "#21318C", 
      "Discoidal_core" = "#1E80B8", 
      "Core_on_flake" = "#00d8c7", 
      "Expedient_core" = "#EEF8B4", 
      "Surface_core" = "#CDEBB3"
    ),
    labels = c(
      "Quina_core" = "Quina core",
      "Discoidal_core" = "Discoidal core",
      "Core_on_flake" = "Core on flake",
      "Expedient_core" = "Expedient core",
      "Surface_core" = "Surface core"
    ))

# Flake types
flake_types <- 
    bind_rows(techno_flake,
              rsp_flake,
              odn_flake)
  
  flake_types$Typology <- factor(
    flake_types$Typology, 
    levels = c("Quina_flake",  
               "Resharpening_flake", 
               "Discoidal_flake", 
               "Kombewa_flake", 
               "Surface_flake",
               "Ordinary_flake")
  )
  
qf_data <- flake_types %>%
    filter(Typology == "Quina_flake") %>%
    arrange(Length, Width)
  
convex_hull_qf <- qf_data[chull(qf_data$Length, qf_data$Width), ]
  
p3 <-
  ggplot() +
    geom_polygon(data = convex_hull_qf, aes(x = Length, y = Width, fill = Typology, group = Typology), 
               alpha = 0.2, size = 0, color = "#21318C") +
    geom_point(data = subset(flake_types, Typology == "Ordinary_flake"), 
               aes(x = Length, y = Width, fill = Typology), 
               shape = 21, color = "black", alpha = 1, size = 2.5, stroke = 0.25) +
    geom_point(data = subset(flake_types, Typology != "Ordinary_flake"), 
               aes(x = Length, y = Width, fill = Typology), 
               shape = 21, color = "black", alpha = 1, size = 2.5, stroke = 0.25) +
    labs(x = "Length (mm)", y = "Width (mm)") +
    theme_bw() +
  theme(axis.text.x = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 12),
        panel.grid.major = element_line(color = "white"), 
        panel.grid.minor = element_line(color = "white"),
        legend.position = c(0.95, 0.05), 
        legend.justification = c(1, 0),
        legend.key.size = unit(0.5, "cm"),
        legend.background = element_rect(fill = "white", color = "black", size = 0.05), 
        legend.box.background = element_rect(color = "black")
  ) + 
    scale_fill_manual(
      name = NULL,
      values = c(
        "Quina_flake" = "#21318C", 
        "Discoidal_flake" = "#1E80B8", 
        "Kombewa_flake" = "#00d8c7", 
        "Ordinary_flake" = "#EEF8B4", 
        "Surface_flake" = "#CDEBB3", 
        "Resharpening_flake" = "#73c2b1"
      ),
      labels = c(
        "Quina_flake" = "Quina flake",
        "Discoidal_flake" = "Discoidal flake",
        "Kombewa_flake" = "Kombewa flake",
        "Ordinary_flake" = "Ordinary flake",
        "Surface_flake" = "Surface flake",
        "Resharpening_flake" = "Resharpening flake"
      ))

 # Tool types
tool_types <- 
    bind_rows(qn_scraper,
              odn_scraper,
              notch_denti,
              mis_tool)

  tool_types$Typology <- factor(
    tool_types$Typology, 
    levels = c("Quina_scraper",  
               "Ordinary_scraper", 
               "Notch", 
               "Denticulate", 
               "Miscellaneous_tool")
  )

qs_data <- tool_types %>%
  filter(Typology == "Quina_scraper") %>%
  arrange(Length, Width)

convex_hull_qs <- qs_data[chull(qs_data$Length, qs_data$Width), ]

p4 <-  
ggplot(tool_types, aes(x = Length, y = Width)) +
  geom_polygon(data = convex_hull_qs, aes(fill = Typology, group = Typology), 
               alpha = 0.2, size = 0, color = "#21318C") +
  geom_point(aes(fill = Typology), shape = 21, color = "black", 
             alpha = 1, size = 2.5, stroke = 0.25) +
  labs(x = "Length (mm)", y = "Width (mm)") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 12),
        panel.grid.major = element_line(color = "white"), 
        panel.grid.minor = element_line(color = "white"),
        legend.position = c(0.95, 0.05), 
        legend.justification = c(1, 0),
        legend.key.size = unit(0.5, "cm"),  
        legend.background = element_rect(fill = "white", color = "black", size = 0.05), 
        legend.box.background = element_rect(color = "black")) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "Quina_scraper" = "#21318C", 
      "Ordinary_scraper" = "#1E80B8", 
      "Notch" = "#00d8c7", 
      "Miscellaneous_tool" = "#EEF8B4", 
      "Denticulate" = "#CDEBB3"),
    labels = c(
      "Quina_scraper" = "Quina scraper",
      "Ordinary_scraper" = "Ordinary scraper",
      "Notch" = "Notch",
      "Miscellaneous_tool" = "Miscellaneous tool",
      "Denticulate" = "Denticulate"
    ))

p1 <- p1 + ggtitle("A")
p2 <- p2 + ggtitle("B")
p3 <- p3 + ggtitle("C")
p4 <- p4 + ggtitle("D")

combined_plot <- plot_grid(
  p1, p2, p3, p4,
  nrow = 2,      
  align = "hv",  
  axis = "tblr"  
)

# Sites to river distance-------------------------------------------------------

Site_river_distance <- read_excel("Site_river_distance.xlsx", skip = 0) %>%
  mutate(Site = factor(Site, levels = c("Tianhuadong",
                                        "Dazhuang",
                                        "Longtan", 
                                        "Songping",
                                        "Guanshan")))

ggplot(Site_river_distance) +
  aes(Site, Distance)+
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(axis.text.x = element_markdown(angle = 45, hjust = 1, size = 15), 
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 16.5),
        panel.grid.major = element_line(color = "white"), 
        panel.grid.minor = element_line(color = "white")) +
  scale_x_discrete(labels = c("Tianhuadong", 
                              "Dazhuang", 
                              "**Longtan**", 
                              "Songping", 
                              "Guanshan")) +
  labs(x = "",
       y = "Distance to river (m)")

# Taphonomic graphs-------------------------------------------------------------

Taphonomic <- read_excel("LT_taphonomic_information.xlsx", sheet = 1)

# Strike
Taphonomic1 <- 
  Taphonomic %>%
  mutate(Strike = factor(Strike, levels = c("0-15°", 
                                            "16-30°", 
                                            "31-45°", 
                                            "46-60°", 
                                            "61-75°", 
                                            "76-90°", 
                                            "91-105°", 
                                            "106-120°",
                                            "121-135°",
                                            "136-150°",
                                            "151-165°",
                                            "166-180°")))

Strike_rose <- 
  ggplot(Taphonomic1, 
                      aes(Strike, Nstrike)) +
  geom_col(fill = "#868686", 
           color = "black", 
           width = 0.8,
           alpha = 0.6) + 
  coord_polar() +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 14,
                                   color = "black"),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major = element_line(color = "gray"), 
        panel.grid.minor = element_line(color = "gray")) +
  ylim(0, max(Taphonomic$Nstrike)) 

# Dip
Taphonomic2 <- 
  Taphonomic %>%
  mutate(Dip_direction = factor(Dip_direction, levels = c("0-30°", 
                                                          "31-60°", 
                                                          "61-90°", 
                                                          "91-120°", 
                                                          "121-150°", 
                                                          "151-180°", 
                                                          "181-210°", 
                                                          "211-240°",
                                                          "241-270°",
                                                          "271-300°",
                                                          "301-330°",
                                                          "331-360°")))

DD_rose <- 
  ggplot(Taphonomic2, 
                  aes(Dip_direction, Ndd)) +
  geom_col(fill = "#868686", 
           color = "black", 
           width = 0.8,
           alpha = 0.6) + 
  coord_polar() +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 14,
                                   color = "black"),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major = element_line(color = "gray"), 
        panel.grid.minor = element_line(color = "gray"))

# Plunge
AD_df <- 
  Taphonomic %>%
  filter(!is.na(Angle_of_dip) & !is.na(Nad))

AD_rose <- 
  ggplot(AD_df,
         aes(Angle_of_dip, Nad)) +
  geom_col(fill = "#868686", 
           color = "black", 
           width = 0.5,
           alpha = 0.6) + 
  coord_polar() +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 14,
                                   color = "black"),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major = element_line(color = "gray"), 
        panel.grid.minor = element_line(color = "gray"))

# Weather
Weather_df <- 
  Taphonomic %>%
  filter(!is.na(Weathering) & !is.na(Nweather))

Weather_df1 <- 
  Weather_df %>%
  mutate(Weathering = factor(Weathering, levels = c("Fresh", 
                                                    "Slight", 
                                                    "Moderate", 
                                                    "High")))

Weather_df1 <- Weather_df1 %>%
  mutate(Proportion = Nweather / sum(Nweather, na.rm = TRUE))

Weather_rose <- 
  ggplot(Weather_df1,
         aes(Weathering, Proportion)) +
  geom_col(fill = "#868686", 
           color = "black", 
           width = 0.5,
           alpha = 0.6) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

# Size distribution
Size_df <- 
  Taphonomic %>%
  filter(!is.na(Size) & !is.na(Nsize))

Size_df$Size <- 
  factor(Size_df$Size,
         levels = c("< 3 cm", 
                    "3-5 cm", 
                    "5-7 cm", 
                    "7-10 cm", 
                    "> 10 cm"))

Size_line <- 
  ggplot(Size_df, 
         aes(Size, Nsize, group = 1)) +
  geom_line(linetype = "dashed") +
  geom_point(size = 3) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_y_continuous(
    breaks = seq(min(Size_df$Nsize, na.rm = TRUE), 
                 max(Size_df$Nsize, na.rm = TRUE), 
                 length.out = 6
  ))

# 3D coordinate of stone artifacts----------------------------------------------

# All types
custom_order <- c(
  "Quina core", "Discoidal core", "Core on flake", "Surface core", "Expedient core", 
  "Quina flake", "Discoidal flake", "Kombewa flake", "Surface flake", 
  "Resharpening flake", "Ordinary flake", "Incomplete flake", 
  "Quina scraper", "Ordinary scraper", 
  "Notch", "Denticulate", "Miscellaneous tool", 
  "Manuport", "Chunk & debris"
)

png("3d_plot.png", width = 3000, height = 3000, res = 250)
with(coord, 
     scatter3D(x = X, y = Y, z = Z,
               pch = 21, 
               cex = 2.5,
               col = "white",
               bg = "#47a579", 
               xlab = "N (m)", 
               ylab = "E (m)", 
               zlab = "Altitude (m)",
               xlim = c(0, 10),
               ylim = c(0, 5),
               zlim = c(1510.65, 1511.7),
               ticktype = "detailed",
               bty = "f", 
               box = TRUE,
               theta = -20, 
               phi = 0, 
               d = 3,
               colkey = FALSE))
par(mar = c(5, 0, 5, 0))
dev.off()

# Filtered types
custom_order_filtered <- c(
  "Quina core", "Quina flake", "Quina scraper", "Resharpening flake", 
  "Discoidal core", "Discoidal flake", 
  "Core on flake", "Kombewa flake", 
  "Surface core", "Surface flake"
)
coord_filtered <- coord[coord$Typology %in% custom_order_filtered, ]
color_palette_custom <- c(
  "Quina core" = "#922b21", "Quina flake" = "#c0392b", "Quina scraper" = "#ec7063", "Resharpening flake" = "#f5b7b1", 
  "Discoidal core" = "#5b2c6f", "Discoidal flake" = "#d2b4de", 
  "Core on flake" = "#224a80", "Kombewa flake" = "#95d5f7", 
  "Surface core" = "#935116", "Surface flake" = "#f9e79f"
)

shape_mapping <- c(
  "Quina core" = 21, "Quina flake" = 21,  "Quina scraper" = 21, "Resharpening flake" = 21, 
  "Discoidal core" = 24,   "Discoidal flake" = 24, 
  "Core on flake" = 22,  "Kombewa flake" = 22, 
  "Surface core" = 23,  "Surface flake" = 23
)

typology_colors_custom <- setNames(color_palette_custom, custom_order_filtered)

coord_filtered$color <- typology_colors_custom[coord_filtered$Typology]

coord_filtered$pch <- shape_mapping[coord_filtered$Typology]

png("3d_plot_filtered_technological_types.png", width = 3000, height = 3000, res = 250)

with(coord_filtered, 
     scatter3D(x = X, y = Y, z = Z,
               pch = coord_filtered$pch, 
               cex = 2.5,
               col = "white",
               bg = color, 
               xlab = "N (m)", 
               ylab = "E (m)", 
               zlab = "Altitude (m)",
               xlim = c(0, 10),
               ylim = c(0, 5),
               zlim = c(1510.65, 1511.7),
               ticktype = "detailed",
               bty = "f", 
               box = TRUE,
               theta = -20, 
               phi = 0, 
               d = 3,
               colkey = FALSE))
dev.off()
