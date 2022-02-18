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
    '#f0f7ee',
    '#a8dadc',
    '#457b9d',
    '#1d3557',
    '#335c67',
    '#fff3b0',
    "#e09f3e",
    '#9e2a2b',
    '#540b0e',
    "#be7098",
    "#64ac48",
    "#9a963f",
    "#7866ca",
    '#e63946',
    "#c98746",
    "#6890ce",
    "#ca5237",
    "#4aac8b",
    "#cd486e"
  )

scale_color_custom <-
  list(
    scale_color_manual(values = color_ordered),
    scale_fill_manual(values = color_ordered)
  )


awd_df[, rank_overall := rank(-supplier_country_share),
       by = .(fy_group, procurementcategory2)]

# create new country classification:
awd_df[suppliercountrycode_3 == "CHN", cname := "China"]
awd_df[suppliercountrycode_3 != "CHN" & rank_overall%in%c(1, 2, 3), 
       cname := suppliercountryname]
awd_df[suppliercountrycode_3 != "CHN" & 
         !(rank_overall%in%c(1, 2, 3)), 
       cname := "Other"]
awd_df <- awd_df[,.(supplier_country_share = 
                    sum(supplier_country_share, na.rm = T),
                    tot_suppl_contract_value = 
                    sum(tot_suppl_contract_value, na.rm = T)
                    ),
                 by = .(cname, fy_group, procurementcategory2)]
waitifnot(all(abs(awd_df[,.(X=sum(supplier_country_share)), 
                         by = .(fy_group)]$X-4)<0.01))

# order the countries with China, U.S., and U.K. in front:
core_countries <- c("United Kingdom", "United States", "China")
other_countries <- setdiff(unique(awd_df$cname), c(core_countries, "Other"))
c_order <- c("Other", other_countries, core_countries)
awd_df[, cname := factor(cname, levels = c_order)]

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

# make sure I haven't introduced any missing values
waitifnot(nrow(awd_df)==nrow(na.omit(awd_df)))

# write to CSV
awd_df <- awd_df %>% split(awd_df$procurementcategory2)

awd_df <- lapply(awd_df, function(x) {
  x <-
    x %>%
    as.data.table() %>% 
    dcast(., cname ~ fy_group, value.var = 'tot_suppl_contract_value') %>%
    as.data.frame()
  x[is.na(x)] <- 0
  x
})

# write to google sheets
write_sheet(awd_df$`Civil Works`, ss = "https://docs.google.com/spreadsheets/d/1_W2crtW5wViEMU4Q8idZXKmvavjiHIwCVF0CNy9zarU/edit?usp=sharing", sheet = 1)
write_sheet(awd_df$Goods, ss = "https://docs.google.com/spreadsheets/d/1_W2crtW5wViEMU4Q8idZXKmvavjiHIwCVF0CNy9zarU/edit?usp=sharing", sheet = 2)
write_sheet(awd_df$Other, ss = "https://docs.google.com/spreadsheets/d/1_W2crtW5wViEMU4Q8idZXKmvavjiHIwCVF0CNy9zarU/edit?usp=sharing", sheet = 3)
write_sheet(awd_df$Overall, ss = "https://docs.google.com/spreadsheets/d/1_W2crtW5wViEMU4Q8idZXKmvavjiHIwCVF0CNy9zarU/edit?usp=sharing", sheet = 4)






write.csv(awd_df, "awd_df.csv", na = "", row.names = FALSE)


# create plot
plot <- ggplot() +
  geom_col(
    data = awd_df,
    aes(x = fy_group,
        y = tot_suppl_contract_value, fill = cname),
    position = "fill",
    width = 0.7
  ) +
  ggrepel::geom_text_repel(
    data = awd_df,
    aes(x = fy_group,
                               y = supplier_country_share,
                               label = tot_suppl_contract_value
                               )) + 
  my_custom_theme +
  labs(x = "", 
       y = "", 
       subtitle = glue("World Bank proportions of total annual project money by supplier")) +
  facet_wrap( ~ procurementcategory2, ncol = 2, scales = "free") + 
  scale_color_custom + 
  guides(fill = guide_legend(reverse=TRUE))

setwd(output_dir)
ggsave(glue("BAR world bank project proportions.pdf"),
       plot,
       width = 10,
       height = 7)

