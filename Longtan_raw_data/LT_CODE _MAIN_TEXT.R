# Library R packages------------------------------------------------------------
library("readxl")
library("readr")
library("dplyr")
library("tidyr")   
library("ggplot2")
library("cowplot")

# Import the raw data-----------------------------------------------------------
qn_scraper <-  read_excel("Longtan_lithic_tools.xlsx", sheet = 1)
odn_scraper <- read_excel("Longtan_lithic_tools.xlsx", sheet = 2)
notch_denti <- read_excel("Longtan_lithic_tools.xlsx", sheet = 3)
techno_flake <- read_excel("Longtan_lithic_data_flakes.xlsx", sheet = 1)
rsp_flake <- read_excel("Longtan_lithic_data_flakes.xlsx", sheet = 2)
odn_flake <- read_excel("Longtan_lithic_data_flakes.xlsx", sheet = 3)

# Statistical tests in main text------------------------------------------------

# MANOVA for size of technological flakes
integration <- function(data) {
  data %>%
    select(Typology, Length, Width, Thickness, Mass, IPA, 
           Platform_depth, Platform_width) %>%
    drop_na() %>%
    mutate(Length = log(Length),
           Width = log(Width),
           Thickness = log(Thickness),
           Mass = log(Mass),
           IPA = log(IPA),
           Platform_depth = log(Platform_depth),
           Platform_width = log(Platform_width))
}

SIZE_FLAKE <- 
  bind_rows(integration(techno_flake),
    integration(rsp_flake))

MANOVA_SIZE_FLAKE <- 
  manova(cbind(Length, 
               Width, 
               Thickness, 
               Mass, 
               IPA, 
               Platform_depth, 
               Platform_width) ~ Typology, 
         data = SIZE_FLAKE)

summary(MANOVA_SIZE_FLAKE)

# Welch's ANOVA for IPA of technological flakes
SIZE_FLAKE <- SIZE_FLAKE %>%
  mutate(IPA_log = log(IPA))

oneway.test(IPA_log ~ Typology, 
            data = SIZE_FLAKE, 
            var.equal = FALSE)

# Welch's ANOVA for thickness of tools
integration <- function(data) {
  data %>%
    select(Typology, Thickness) %>%
    drop_na() %>%
    mutate(
      Thickness_log = log(Thickness))
}

TH_TOOL <- 
  bind_rows(
    integration(qn_scraper),
    integration(odn_scraper),
    integration(notch_denti)
  )

oneway.test(Thickness_log ~ Typology, 
            data = TH_TOOL, 
            var.equal = FALSE) 

# t test for edge angle of Quina scrapers and ordinary scrapers
remove_outliers <- function(df, col) {
  Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  df %>% filter(df[[col]] >= lower_bound & df[[col]] <= upper_bound)
}

QS_EA <- qn_scraper %>%
  select(a = E1_Ave_Angle, b = E2_Ave_Angle, c = E3_Ave_Angle) %>%
  mutate(a = suppressWarnings(as.numeric(as.character(a))),
         b = suppressWarnings(as.numeric(as.character(b))),
         c = suppressWarnings(as.numeric(as.character(c)))) %>%
  rowwise() %>%
  mutate(ave = mean(c_across(c("a", "b", "c")), na.rm = TRUE)) %>%
  ungroup()

QS_EA <- remove_outliers(QS_EA, "ave") %>% mutate(Typology = "Quina scrapers")

ODN_EA <- odn_scraper %>%
  mutate(ave = suppressWarnings(as.numeric(as.character(E1_Ave_Angle)))) %>%
  select(ave)

ODN_EA <- remove_outliers(ODN_EA, "ave") %>% 
  mutate(Typology = "Ordinary scrapers")

TOTAL_EA <- bind_rows(QS_EA, ODN_EA) %>%
  mutate(Typology = factor(Typology, 
                           levels = c("Quina scrapers", "Ordinary scrapers"))) %>%
  mutate(ave_log = log(ave))

t.test(ave_log ~ Typology, data = TOTAL_EA)

# t test for reduction intensity of Quina scrapers and ordinary scrapers
integration <- function(data) {
  data %>%
    select(Typology, Ave_GIUR) %>%
    drop_na() %>%
    mutate(Ave_GIUR_log = log(Ave_GIUR + 1))
}

RI_QS_ODN <- 
  bind_rows(
    integration(qn_scraper),
    integration(odn_scraper)
  )

t.test(Ave_GIUR_log ~ Typology, data = RI_QS_ODN)

# t test for Edge angle (Quina scrapers) and EPA (Resharpening flakes)
QS_ave <- QS_EA %>%
  select(ave) %>% 
  drop_na()

rsp_EPA <- rsp_flake %>% 
  select(EPA) %>%
  drop_na()

combined_edge_angle <- bind_rows(
  QS_ave %>% mutate(Typology = "Quina_scraper"),
  rsp_EPA %>% rename(ave = EPA) %>% mutate(Typology = "Resharpening_flake")
) %>%
  mutate(ave_log = log(ave))

t.test(ave_log ~ Typology, data = combined_edge_angle)

# Graphs in main text-----------------------------------------------------------

base_size_value <- 10

# Box-plot of thickness
TH_TOOL_clean <- TH_TOOL %>%
  filter(Typology %in% c("Quina_scraper", "Ordinary_scraper"))

colors <- c("#EE7E77", "#68A7BE")

plot_thick <- 
  ggplot(TH_TOOL_clean, fill = Typology) +
  aes(x = reorder(Typology, -Thickness), 
      y = Thickness) +
  stat_halfeye(mapping = aes(fill = Typology), 
               alpha = 1, 
               adjust = 0.5, 
               width = 0.3, 
               .width = 0, 
               justification = -0.5) +
  geom_errorbar(mapping = aes(color = Typology), 
                stat = "boxplot", 
                width = 0.1, 
                linewidth = 1, 
                position = position_dodge(width = 0.75)) +
  geom_boxplot(mapping = aes(color = Typology), 
               width = 0.2, 
               size = 1, 
               outlier.shape = NA, alpha = 1) +  
  geom_point(stat = "summary", 
             fun = "mean", 
             shape = 19, 
             size = 3, 
             color = "black", 
             show.legend = FALSE) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  geom_vline(xintercept = 1.55, 
             linetype = "dashed", 
             color = "black", size = 0.5) +
  scale_x_discrete(expand = expansion(add = c(0.5, 0.5))) +
  xlab("") +
  ylab("Thickness (mm)") +
  theme_bw(base_size = base_size_value) +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title.y = element_text(size = 18, color = "black"),
        axis.ticks.x = element_blank(), 
        panel.grid.major = element_line(color = "white"), 
        panel.grid.minor = element_line(color = "white")) +
  theme(legend.position = "none")
print(plot_thick)

# Box-plot of Edge angle
colors <- c("#68A7BE", "#EE7E77")

plot_edge_angle <- 
  ggplot(TOTAL_EA, 
         fill = Typology, 
         aes(x = Typology, y = ave)) +
  stat_halfeye(mapping = aes(fill = Typology), 
               alpha = 1, 
               adjust = 0.5, 
               width = 0.3, 
               .width = 0, 
               justification = -0.5) +
  geom_errorbar(mapping = aes(color = Typology), 
                stat = "boxplot", 
                width = 0.1, 
                linewidth = 1, 
                position = position_dodge(width = 0.75)) +
  geom_boxplot(mapping = aes(color = Typology), 
               width = 0.2, 
               size = 1, 
               outlier.shape = NA, 
               alpha = 1) +  
  geom_point(stat = "summary", 
             fun = "mean", 
             shape = 19, 
             size = 3, 
             color = "black", 
             show.legend = FALSE) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  geom_vline(xintercept = 1.55, 
             linetype = "dashed", 
             color = "black", 
             size = 0.5) +
  scale_x_discrete(expand = expansion(add = c(0.5, 0.5))) +
  xlab("") +
  ylab("Edge angle (°)") +
  theme_bw(base_size = base_size_value) +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title.y = element_text(size = 18),
        axis.ticks.x = element_blank(), 
        panel.grid.major = element_line(color = "white"), 
        panel.grid.minor = element_line(color = "white")) +
  theme(legend.position = "none")
print(plot_edge_angle)

# Box-plot of GIUR
colors <- c("#68A7BE", "#EE7E77")

RI_QS_ODN <- RI_QS_ODN %>%
  mutate(Typology = factor(Typology, 
                           levels = c("Quina_scraper", "Ordinary_scraper")))

plot_giur <- 
  ggplot(RI_QS_ODN, 
         fill = Typology, 
         aes(x = Typology, y = Ave_GIUR)) +
  stat_halfeye(mapping = aes(fill = Typology),
               alpha = 1, 
               adjust = 0.5, 
               width = 0.3, 
               .width = 0, 
               justification = -0.5) +
  geom_errorbar(mapping = aes(color = Typology), 
                stat = "boxplot", 
                width = 0.1, 
                linewidth = 1, 
                position = position_dodge(width = 0.75)) +
  geom_boxplot(mapping = aes(color = Typology), 
               width = 0.2, 
               size = 1, 
               outlier.shape = NA, alpha = 1) +  
  geom_point(stat = "summary", 
             fun = "mean", 
             shape = 19, 
             size = 3, 
             color = "black", 
             show.legend = FALSE) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  geom_vline(xintercept = 1.55, 
             linetype = "dashed", 
             color = "black", size = 0.5) +
  scale_x_discrete(expand = expansion(add = c(0.5, 0.5))) +
  xlab("") +
  ylab("Reduction intensity") +
  theme_bw(base_size = base_size_value) +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title.y = element_text(size = 18),
        axis.ticks.x = element_blank(), 
        panel.grid.major = element_line(color = "white"), 
        panel.grid.minor = element_line(color = "white")) +
  theme(legend.position = "none")
print(plot_giur)

# Box-plot of Edge angle (Quina scrapers) and EPA (Resharpening flakes)
rsp_EPA_clean <- rsp_flake %>% 
  select(EPA) %>%
  remove_outliers("EPA") %>%
  drop_na()

combined_edge_angle_clean <- bind_rows(
  QS_ave %>% mutate(Typology = "Quina_scraper"),
  rsp_EPA_clean %>% rename(ave = EPA) %>% 
    mutate(Typology = "Resharpening_flake")
)

plot_EA_EPA <- 
  ggplot(combined_edge_angle_clean, 
         fill = Typology, 
         aes(x = Typology, y = ave)) +
  stat_halfeye(mapping = aes(fill = Typology), 
               alpha = 1, adjust = 0.5, 
               width = 0.3, 
               .width = 0, 
               justification = -0.5) +
  geom_errorbar(mapping = aes(color = Typology), 
                stat = "boxplot", 
                width = 0.1, 
                linewidth = 1, 
                position = position_dodge(width = 0.1)) +
  geom_boxplot(mapping = aes(color = Typology), 
               width = 0.2, 
               size = 1, 
               outlier.shape = NA, 
               alpha = 1) +  
  geom_point(stat = "summary", 
             fun = "mean", 
             shape = 19, 
             size = 3, 
             color = "black", 
             show.legend = FALSE) +
  geom_vline(xintercept = 1.55, 
             linetype = "dashed", 
             color = "black", 
             size = 0.5) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  scale_x_discrete(limits = c("Quina_scraper", "Resharpening_flake"), 
                   expand = expansion(add = c(0.5, 0.5))) +
  xlab("") +
  ylab("Edge angle/EPA (°)") +
  theme_bw(base_size = base_size_value) +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title.y = element_text(size = 18),
        axis.ticks.x = element_blank(), 
        panel.grid.major = element_line(color = "white"), 
        panel.grid.minor = element_line(color = "white")) +
  theme(legend.position = "none")
print(plot_EA_EPA)

# Combine graphs into one panel
plot_grid(plot_thick,
          plot_edge_angle,
          plot_giur,
          plot_EA_EPA,
          nrow = 1)

