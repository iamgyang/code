rm(list = ls()) # clear the workspace
# Options: ----------------------------------------------------------------

# debugging
options(error=browser)
options(error=NULL)

# disable data.table auto-indexing (causes errors w/ dplyr functions)
options(datatable.auto.index = FALSE)

# Directories -------------------------------------------------------------

# You will have to edit this to be your own computer's working directories:
user <- Sys.info()["user"]
root_dir <- paste0("C:/Users/", user, "/Dropbox/CGD/Projects/wb-award-database/")
input_dir <- paste0(root_dir, "input")
output_dir <- paste0(root_dir, "output")
code_dir <- paste0(root_dir, "code")
raw_dir <- paste0(root_dir, "raw_data")
setwd(root_dir)

# Packages ---------------------------------------------------------------
list.of.packages <- c("data.table", "dplyr", "stringdist", "countrycode", "ggplot2", 
                      "ggthemes", "readxl", "tidyr", "glue", "fst", "readstata13")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) install.packages(new.packages, dependencies = TRUE)
for (package in list.of.packages) {library(eval((package)), character.only = TRUE)}

# set GGPLOT default theme:
theme_set(theme_clean() + theme(plot.background = element_rect(color = "white")))

# load personal functions
source(paste0("C:/Users/", user, "/Dropbox/Coding_General/personal.functions.R"))


###########################################################################
# THIS IS WHERE THE ACTUAL CODE BEGINS
###########################################################################

# import
setwd(input_dir)
awd_df <- as.data.table(readstata13::read.dta13("output.dta"))

# convert to readable country names
awd_df[, suppliercountrycode_3 := countrycode(
  suppliercountrycode,
  "iso2c",
  "iso3c",
  custom_match = c(
    "GZ" = name2code("West Bank and Gaza"),
    "RY" = name2code("Yemen"),
    "TP" = name2code("Timor Leste"),
    "XK" = "XKX", # kosovo
    "YF" = name2code("Serbia"),
    "ZR" = name2code("Democratic Republic of the Congo")
  )
)]
awd_df[,suppliercountryname:=code2name(suppliercountrycode_3, custom_match = c("XKX" = "Kosovo"))]

# check this manually:
unique(awd_df[,.(suppliercountry, suppliercountryname)]) %>% View()
awd_df[,suppliercountry:=NULL]

# adjust colors:
color_ordered <-
  c(
    '#f0f7ee',
    '#a8dadc',
    '#1d3557',
    "#4aac8b",
    '#335c67',
    '#540b0e',
    "#e09f3e",
    '#9e2a2b',
    "#be7098",
    "#64ac48",
    "#9a963f",
    '#457b9d',
    "#7866ca",
    '#e63946',
    "#c98746",
    "#6890ce",
    "#ca5237",
    '#fff3b0',
    "#cd486e"
  )

scale_color_custom <-
  list(
    scale_color_manual(values = color_ordered),
    scale_fill_manual(values = color_ordered)
  )

awd_df[, rank_overall := rank(-supplier_country_share),
       by = .(fy_group, procurementcategory2)]

# relevel the fiscal year groups:
awd_df[, fy_group := factor(
  fy_group,
  levels = c(
    "2001-2004",
    "2005-2009",
    "2010-2014",
    "2015-2019",
    "2020-2021"
  )
)]

# Civil works & overall
awd_df[suppliercountrycode_3 %in% c("CHN", "JPN", "USA", "GBR", "IND"), cname := suppliercountryname]
awd_df[!(suppliercountrycode_3 %in% c("CHN", "JPN", "USA", "GBR", "IND")), cname := "Other"]
awd_df <- awd_df[,.(supplier_country_share = 
                  sum(supplier_country_share, na.rm = T),
                  tot_suppl_contract_value = 
                  sum(tot_suppl_contract_value, na.rm = T)
                  ),
              by = .(cname, fy_group, procurementcategory2)]

# order the countries with China, U.S., and U.K. in front:
core_countries <- c("United Kingdom", "United States", "China")
other_countries <- setdiff(unique(awd_df$cname), c(core_countries, "Other"))
c_order <- c("Other", other_countries, core_countries)
awd_df[, cname := factor(cname, levels = c_order)]

# For 'civil works' and 'overall', create country classification based on top 3;
# for goods and other, create for top 5.

waitifnot(all(abs(awd_df[,.(X=sum(supplier_country_share)), 
                      by = .(fy_group)]$X-4)<0.01))

# make sure I haven't introduced any missing values
waitifnot(nrow(awd_df)==nrow(na.omit(awd_df)))
awd_df[, max_tot_suppl_contract_value:=
  sum(tot_suppl_contract_value, na.rm = T), 
by = .(procurementcategory2, fy_group)]

# create bar plot
plot <- ggplot(data = awd_df,
    aes(
      x = fy_group,
      y = tot_suppl_contract_value, 
      fill = cname
      )
    ) +
  geom_col(
    position = "fill",
    width = 0.7
  ) +
  geom_text(aes(label = paste0("$",
    signif(max_tot_suppl_contract_value / (10 ^ 9), 2), "B"
  ), x = fy_group, y = 1.05), check_overlap = TRUE) +
  my_custom_theme +
  theme(axis.ticks.x = element_blank()) + 
  labs(
    x = "",
    y = "",
    subtitle = "World Bank Financed Contracts Awarded to Non-Recipient Contractors by Country of Contractor"
  ) +
  facet_wrap(~procurementcategory2, ncol = 2, scales = "free") +
  scale_color_custom +
  guides(fill = guide_legend(reverse = TRUE))+
  scale_y_continuous(expand = expansion(mult = c(0, .1)), breaks = seq(0, 1, 0.25))

setwd(output_dir)
ggsave(glue("BAR world bank project proportions.pdf"),
       plot, 
       width = 10,
       height = 7)
ggsave(glue("BAR world bank project proportions.png"),
       plot, 
       width = 10,
       height = 7)
