# Note: I had to do this in R, because otherwise in STATA it would take several 
# days to complete.

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
    "#be7098",
    "#64ac48",
    "#c25abc",
    "#9a963f",
    "#7866ca",
    "#c98746",
    "#6890ce",
    "#ca5237",
    "#4aac8b",
    "#cd486e"
  )


# scale_color_custom <- 
#   list(
#     scale_color_manual(values = color_ordered),
#     scale_fill_manual(values = color_ordered)
#   )
# 
# 
# # Produce graphs tracking share of US, Japan, China, Germany, UK, France, India, Russia, Canada, Italy in 4/5/6/7 above over time
# plot <- ggplot() +
#   geom_line(
#     data = awd_df[suppliercountrycode_3 %in% c(
#       name2code(
#        c('USA',
#         'Japan',
#         'China',
#         'Germany',
#         'UK',
#         'France',
#         'India',
#         'Russia',
#         'Canada',
#         'Italy')
#       )
#     )],
#     aes(x = fiscalyear,
#         y = supplier_country_share * 100, 
#         color = procurementcategory2)
#   ) +
#   my_custom_theme +
#   labs(x = "", 
#        y = "", 
#        subtitle = "World Bank proportions of total annual project money by supplier") +
#   scale_color_custom +
#   facet_wrap( ~ suppliercountryname)
# 
# setwd(output_dir)
# ggsave("world bank project proportions.pdf",
#        plot,
#        width = 10,
#        height = 7)

# For 2021, *2020 and 2001*, the top twenty list of countries in terms of contract wins overall, for civil works, goods, and [everything that isn’t goods and civil works]

awd_df[, rank_overall := rank(-supplier_country_share),
       by = .(fy_group, procurementcategory2)]

# create new country classification:
awd_df[, c_class:=]


country_name_order <- unique(awd_df[order(-rank_overall)]$suppliercountryname)
awd_df[,suppliercountryname:=factor(suppliercountryname, levels = country_name_order)]

waitifnot(nrow(awd_df)==nrow(na.omit(awd_df)))

for (i in unique( awd_df$procurementcategory2)) {
  
plot <- ggplot() +
  geom_col(
    data = awd_df[fy_group %in% c(2021, 2001, 2020) & 
                    rank_overall%in%1:20 & 
                    procurementcategory2 == i],
    aes(y = suppliercountryname,
        x = supplier_country_share * 100),
    fill = "grey40",
    position = position_dodge2(preserve = "single")
  ) +
  my_custom_theme +
  theme(
    axis.text.y = element_text(
      size = 12,
      vjust = 0.2,
      margin = unit(c(
        t = 0,
        r = 0,
        b = 0,
        l = 0
      ), "mm"),
      color = "turquoise4"
    )
  )+
  labs(x = "", 
       y = "", 
       subtitle = glue("{i} World Bank proportions of total annual project money by supplier")) +
  scale_color_custom +
  facet_wrap( ~ fy_group, ncol = 3)

setwd(output_dir)
ggsave(glue("world bank project proportions BAR {i}.pdf"),
       plot,
       width = 10,
       height = 7)

}

write.csv(awd_df, 
          "wb_contract_awards_ranked_proportions.csv", 
          na = "", 
          row.names = FALSE)

