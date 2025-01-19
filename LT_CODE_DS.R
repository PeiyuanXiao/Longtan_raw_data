# Library R packages------------------------------------------------------------
library("readxl")
library("dplyr")
library("tidyr")

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

# Core attributes descriptive statistics----------------------------------------
core_stats <- core %>%
  select(Typology, 
         Length, Width, Thickness, Mass, 
         N_Platform, N_Flaking_face, 
         Ave_Platform_angle, 
         Cortex) %>%
  drop_na()

core_length <- 
  core_stats %>% 
  group_by(Typology) %>%
  summarize(
    Mean = mean(Length, na.rm = TRUE),
    SD = sd(Length, na.rm = TRUE),
    Median = median(Length, na.rm = TRUE),
    Q1 = quantile(Length, 0.25, na.rm = TRUE),
    Q3 = quantile(Length, 0.75, na.rm = TRUE)
     )

print(core_length)

core_width <- 
  core_stats %>% 
  group_by(Typology) %>%
  summarize(
    Mean = mean(Width, na.rm = TRUE),
    SD = sd(Width, na.rm = TRUE),
    Medianh = median(Width, na.rm = TRUE),
    Q1 = quantile(Width, 0.25, na.rm = TRUE),
    Q3 = quantile(Width, 0.75, na.rm = TRUE)
  )

print(core_width)

core_thickness <- 
  core_stats %>% 
  group_by(Typology) %>%
  summarize(
    Mean = mean(Thickness, na.rm = TRUE),
    SD = sd(Thickness, na.rm = TRUE),
    Medianh = median(Thickness, na.rm = TRUE),
    Q1 = quantile(Thickness, 0.25, na.rm = TRUE),
    Q3 = quantile(Thickness, 0.75, na.rm = TRUE)
  )

print(core_thickness)

core_mass <- 
  core_stats %>% 
  group_by(Typology) %>%
  summarize(
    Mean = mean(Mass, na.rm = TRUE),
    SD = sd(Mass, na.rm = TRUE),
    Medianh = median(Mass, na.rm = TRUE),
    Q1 = quantile(Mass, 0.25, na.rm = TRUE),
    Q3 = quantile(Mass, 0.75, na.rm = TRUE)
  )

print(core_mass)

core_N_Platform <- 
  core_stats %>% 
  group_by(Typology) %>%
  summarize(
    Mean = mean(N_Platform, na.rm = TRUE),
    SD = sd(N_Platform, na.rm = TRUE),
    Medianh = median(N_Platform, na.rm = TRUE),
    Q1 = quantile(N_Platform, 0.25, na.rm = TRUE),
    Q3 = quantile(N_Platform, 0.75, na.rm = TRUE)
  )

print(core_N_Platform)

core_N_Flaking_face <- 
  core_stats %>% 
  group_by(Typology) %>%
  summarize(
    Mean = mean(N_Flaking_face, na.rm = TRUE),
    SD = sd(N_Flaking_face, na.rm = TRUE),
    Medianh = median(N_Flaking_face, na.rm = TRUE),
    Q1 = quantile(N_Flaking_face, 0.25, na.rm = TRUE),
    Q3 = quantile(N_Flaking_face, 0.75, na.rm = TRUE)
  )

print(core_N_Flaking_face)

core_Ave_Platform_angle <- 
  core_stats %>% 
  group_by(Typology) %>%
  summarize(
    Mean = mean(Ave_Platform_angle, na.rm = TRUE),
    SD = sd(Ave_Platform_angle, na.rm = TRUE),
    Medianh = median(Ave_Platform_angle, na.rm = TRUE),
    Q1 = quantile(Ave_Platform_angle, 0.25, na.rm = TRUE),
    Q3 = quantile(Ave_Platform_angle, 0.75, na.rm = TRUE)
  )

print(core_Ave_Platform_angle)

core_Cortex <- 
  core_stats %>% 
  group_by(Typology) %>%
  summarize(
    Mean = mean(Cortex, na.rm = TRUE),
    SD = sd(Cortex, na.rm = TRUE),
    Medianh = median(Cortex, na.rm = TRUE),
    Q1 = quantile(Cortex, 0.25, na.rm = TRUE),
    Q3 = quantile(Cortex, 0.75, na.rm = TRUE)
  )

print(core_Cortex)

# Flake attributes descriptive statistics---------------------------------------
flake_stats <- techno_flake %>%
  select(Typology, 
         Length, Width, Thickness, Mass, 
         Platform_depth, Platform_width, 
         IPA, Cortex) %>%
  drop_na() %>%
  bind_rows(
    rsp_flake %>% select(Typology, 
                         Length, Width, Thickness, Mass, 
                         Platform_depth, Platform_width, 
                         IPA, Cortex),
    odn_flake %>% select(Typology, 
                         Length, Width, Thickness, Mass, 
                         Platform_depth, Platform_width, 
                         IPA, Cortex),
    incomp_flake %>% select(Typology, 
                            Length, Width, Thickness, Mass, 
                            Platform_depth, Platform_width, 
                            IPA, Cortex)
  )

flake_Length <- 
  flake_stats %>% 
  group_by(Typology) %>%
  summarize(
    Mean = mean(Length, na.rm = TRUE),
    SD = sd(Length, na.rm = TRUE),
    Median = median(Length, na.rm = TRUE),
    Q1 = quantile(Length, 0.25, na.rm = TRUE),
    Q3 = quantile(Length, 0.75, na.rm = TRUE)
  )

print(flake_Length)

flake_Width <- 
  flake_stats %>% 
  group_by(Typology) %>%
  summarize(
    Mean = mean(Width, na.rm = TRUE),
    SD = sd(Width, na.rm = TRUE),
    Medianh = median(Width, na.rm = TRUE),
    Q1 = quantile(Width, 0.25, na.rm = TRUE),
    Q3 = quantile(Width, 0.75, na.rm = TRUE)
  )

print(flake_Width)

flake_Thickness <- 
  flake_stats %>% 
  group_by(Typology) %>%
  summarize(
    Mean = mean(Thickness, na.rm = TRUE),
    SD = sd(Thickness, na.rm = TRUE),
    Medianh = median(Thickness, na.rm = TRUE),
    Q1 = quantile(Thickness, 0.25, na.rm = TRUE),
    Q3 = quantile(Thickness, 0.75, na.rm = TRUE)
  )

print(flake_Thickness)

flake_Mass <- 
  flake_stats %>% 
  group_by(Typology) %>%
  summarize(
    Mean = mean(Mass, na.rm = TRUE),
    SD = sd(Mass, na.rm = TRUE),
    Medianh = median(Mass, na.rm = TRUE),
    Q1 = quantile(Mass, 0.25, na.rm = TRUE),
    Q3 = quantile(Mass, 0.75, na.rm = TRUE)
  )

print(flake_Mass)

flake_Platform_depth <- 
  flake_stats %>% 
  group_by(Typology) %>%
  summarize(
    Mean = mean(Platform_depth, na.rm = TRUE),
    SD = sd(Platform_depth, na.rm = TRUE),
    Medianh = median(Platform_depth, na.rm = TRUE),
    Q1 = quantile(Platform_depth, 0.25, na.rm = TRUE),
    Q3 = quantile(Platform_depth, 0.75, na.rm = TRUE)
  )

print(flake_Platform_depth)

flake_Platform_width <- 
  flake_stats %>% 
  group_by(Typology) %>%
  summarize(
    Mean = mean(Platform_width, na.rm = TRUE),
    SD = sd(Platform_width, na.rm = TRUE),
    Medianh = median(Platform_width, na.rm = TRUE),
    Q1 = quantile(Platform_width, 0.25, na.rm = TRUE),
    Q3 = quantile(Platform_width, 0.75, na.rm = TRUE)
  )

print(flake_Platform_width)

flake_IPA <- 
  flake_stats %>% 
  group_by(Typology) %>%
  summarize(
    Mean = mean(IPA, na.rm = TRUE),
    SD = sd(IPA, na.rm = TRUE),
    Medianh = median(IPA, na.rm = TRUE),
    Q1 = quantile(IPA, 0.25, na.rm = TRUE),
    Q3 = quantile(IPA, 0.75, na.rm = TRUE)
  )

print(flake_IPA)

flake_Cortex <- 
  flake_stats %>% 
  group_by(Typology) %>%
  summarize(
    Mean = mean(Cortex, na.rm = TRUE),
    SD = sd(Cortex, na.rm = TRUE),
    Medianh = median(Cortex, na.rm = TRUE),
    Q1 = quantile(Cortex, 0.25, na.rm = TRUE),
    Q3 = quantile(Cortex, 0.75, na.rm = TRUE)
  )

print(flake_Cortex)

# Tool attributes descriptive statistics----------------------------------------
tool_stats <- qn_scraper %>%
  select(Typology, Length, Width, Thickness, Mass, Cortex) %>%
  drop_na() %>%
  bind_rows(
    odn_scraper %>% select(Typology, Length, Width, Thickness, Mass, Cortex),
    notch_denti %>% select(Typology, Length, Width, Thickness, Mass, Cortex),
    mis_tool %>% select(Typology, Length, Width, Thickness, Mass, Cortex)
  )

tool_scars <- qn_scraper %>%
  select(Typology, N_Scar) %>%
  drop_na() %>%
  bind_rows(
    odn_scraper %>% select(Typology, N_Scar),
    notch_denti %>% select(Typology, N_Scar)
  )

tool_RG <- qn_scraper %>%
  select(Typology, Ave_RG) %>%
  drop_na() %>%
  bind_rows(
    odn_scraper %>% select(Typology, Ave_RG)
   )

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

tool_Length <- 
  tool_stats %>% 
  group_by(Typology) %>%
  summarize(
    Mean = mean(Length, na.rm = TRUE),
    SD = sd(Length, na.rm = TRUE),
    Median = median(Length, na.rm = TRUE),
    Q1 = quantile(Length, 0.25, na.rm = TRUE),
    Q3 = quantile(Length, 0.75, na.rm = TRUE)
  )

print(tool_Length)

tool_Width <- 
  tool_stats %>% 
  group_by(Typology) %>%
  summarize(
    Mean = mean(Width, na.rm = TRUE),
    SD = sd(Width, na.rm = TRUE),
    Medianh = median(Width, na.rm = TRUE),
    Q1 = quantile(Width, 0.25, na.rm = TRUE),
    Q3 = quantile(Width, 0.75, na.rm = TRUE)
  )

print(tool_Width)

tool_Thickness <- 
  tool_stats %>% 
  group_by(Typology) %>%
  summarize(
    Mean = mean(Thickness, na.rm = TRUE),
    SD = sd(Thickness, na.rm = TRUE),
    Medianh = median(Thickness, na.rm = TRUE),
    Q1 = quantile(Thickness, 0.25, na.rm = TRUE),
    Q3 = quantile(Thickness, 0.75, na.rm = TRUE)
  )

print(tool_Thickness)

tool_Mass <- 
  tool_stats %>% 
  group_by(Typology) %>%
  summarize(
    Mean = mean(Mass, na.rm = TRUE),
    SD = sd(Mass, na.rm = TRUE),
    Medianh = median(Mass, na.rm = TRUE),
    Q1 = quantile(Mass, 0.25, na.rm = TRUE),
    Q3 = quantile(Mass, 0.75, na.rm = TRUE)
  )

print(tool_Mass)

tool_edge_angle <- 
  TOTAL_EA %>% 
  group_by(Typology) %>%
  summarize(
    Mean = mean(ave, na.rm = TRUE),
    SD = sd(ave, na.rm = TRUE),
    Medianh = median(ave, na.rm = TRUE),
    Q1 = quantile(ave, 0.25, na.rm = TRUE),
    Q3 = quantile(ave, 0.75, na.rm = TRUE)
  )

print(tool_edge_angle)

tool_scar_number <- 
  tool_scars %>% 
  group_by(Typology) %>%
  summarize(
    Mean = mean(N_Scar, na.rm = TRUE),
    SD = sd(N_Scar, na.rm = TRUE),
    Medianh = median(N_Scar, na.rm = TRUE),
    Q1 = quantile(N_Scar, 0.25, na.rm = TRUE),
    Q3 = quantile(N_Scar, 0.75, na.rm = TRUE)
  )

print(tool_scar_number)

tool_retouch_generation <- 
  tool_RG %>% 
  group_by(Typology) %>%
  summarize(
    Mean = mean(Ave_RG, na.rm = TRUE),
    SD = sd(Ave_RG, na.rm = TRUE),
    Medianh = median(Ave_RG, na.rm = TRUE),
    Q1 = quantile(Ave_RG, 0.25, na.rm = TRUE),
    Q3 = quantile(Ave_RG, 0.75, na.rm = TRUE)
  )

print(tool_retouch_generation)

tool_GUIR <- 
  RI_QS_ODN %>% 
  group_by(Typology) %>%
  summarize(
    Mean = mean(Ave_GIUR, na.rm = TRUE),
    SD = sd(Ave_GIUR, na.rm = TRUE),
    Medianh = median(Ave_GIUR, na.rm = TRUE),
    Q1 = quantile(Ave_GIUR, 0.25, na.rm = TRUE),
    Q3 = quantile(Ave_GIUR, 0.75, na.rm = TRUE)
  )

print(tool_GUIR)

tool_cortex <- 
  tool_stats %>% 
  group_by(Typology) %>%
  summarize(
    Mean = mean(Cortex, na.rm = TRUE),
    SD = sd(Cortex, na.rm = TRUE),
    Medianh = median(Cortex, na.rm = TRUE),
    Q1 = quantile(Cortex, 0.25, na.rm = TRUE),
    Q3 = quantile(Cortex, 0.75, na.rm = TRUE)
  )

print(tool_cortex)

qn_section <- 
  qn_scraper %>%
  group_by(Sub_typology) %>%
  summarize(
    Max = max(Section_asymmetric, na.rm = TRUE),
    Min = min(Section_asymmetric, na.rm = TRUE),
    Mean = mean(Section_asymmetric, na.rm = TRUE),
    SD = sd(Section_asymmetric, na.rm = TRUE),
    Medianh = median(Section_asymmetric, na.rm = TRUE),
    Q1 = quantile(Section_asymmetric, 0.25, na.rm = TRUE),
    Q3 = quantile(Section_asymmetric, 0.75, na.rm = TRUE)
  )

print(qn_section)

# Chunks and debris attributes descriptive statistics---------------------------
chunk_debris_size <- waste %>%
  filter(Typology == "Waste_product") 

length_cb <- chunk_debris_size %>%
  summarise(
    Max = max(Length, na.rm = TRUE),
    Min = min(Length, na.rm = TRUE),
    Mean = mean(Length, na.rm = TRUE),
    SD = sd(Length, na.rm = TRUE),
    Median = median(Length, na.rm = TRUE),
    Q1 = quantile(Length, 0.25, na.rm = TRUE),
    Q3 = quantile(Length, 0.75, na.rm = TRUE)
  )

print(length_cb)

Width_cb <- chunk_debris_size %>%
  summarise(
    Max = max(Width, na.rm = TRUE),
    Min = min(Width, na.rm = TRUE),
    Mean = mean(Width, na.rm = TRUE),
    SD = sd(Width, na.rm = TRUE),
    Median = median(Width, na.rm = TRUE),
    Q1 = quantile(Width, 0.25, na.rm = TRUE),
    Q3 = quantile(Width, 0.75, na.rm = TRUE)
  )

print(Width_cb)

Thickness_cb <- chunk_debris_size %>%
  summarise(
    Max = max(Thickness, na.rm = TRUE),
    Min = min(Thickness, na.rm = TRUE),
    Mean = mean(Thickness, na.rm = TRUE),
    SD = sd(Thickness, na.rm = TRUE),
    Median = median(Thickness, na.rm = TRUE),
    Q1 = quantile(Thickness, 0.25, na.rm = TRUE),
    Q3 = quantile(Thickness, 0.75, na.rm = TRUE)
  )

print(Thickness_cb)

Mass_cb <- chunk_debris_size %>%
  summarise(
    Max = max(Mass, na.rm = TRUE),
    Min = min(Mass, na.rm = TRUE),
    Mean = mean(Mass, na.rm = TRUE),
    SD = sd(Mass, na.rm = TRUE),
    Median = median(Mass, na.rm = TRUE),
    Q1 = quantile(Mass, 0.25, na.rm = TRUE),
    Q3 = quantile(Mass, 0.75, na.rm = TRUE)
  )

print(Mass_cb)
