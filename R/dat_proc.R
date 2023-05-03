# setup -------------------------------------------------------------------

box::use(
  dplyr[...], 
  tidyr[...],
  here[...],
  lubridate[...],
  sf[...],
  googlesheets4[read_sheet,gs4_deauth],
  googledrive[drive_deauth]
)

gs4_deauth()
drive_deauth()

# import, format data from Google Drive -----------------------------------

# data here https://drive.google.com/drive/u/0/folders/1YVkBNr8dj2lSnwogAOeZDosxKXv435E3
id <- '1qL7sLwTcBUIKeqU6QFGWfpOgUD4-xchdVxCCCNK7WpI'

# do not use individual year sheets because the zone names are not corrected
vegraw <- read_sheet(id, sheet = 'VegData', col_types = 'ccccccccdcccddc')

# combine, minor data edits
vegdat <- vegraw %>% 
  select(
    site = `Main site name`, 
    trt = Treatment, 
    scnd = `Secondary site name`,
    zone = `Habitat Type`, 
    quarter = Quarter, 
    date = Date, 
    plot = `Plot #`,
    species = `Vegetation species`, 
    pcent_basal_cover = `Basal cover (%)`, 
    pcent_canopy_cover = `Canopy cover (%)`
    ) %>% 
  mutate(
    date = mdy(date)
  ) %>% 
  arrange(site, trt)

save(vegdat, file = here('data/vegdat.RData'))
write.csv(vegdat, file = here('data/vegdat.csv'), row.names = F)

# get tree data -----------------------------------------------------------

# data here https://docs.google.com/spreadsheets/d/18Zgvo9QjLJDYkK8LCBmWDIGN1NYmYTeHUr5W0BIfsLs/edit#gid=875019540
id <- '18Zgvo9QjLJDYkK8LCBmWDIGN1NYmYTeHUr5W0BIfsLs'

treeraw <- read_sheet(id, sheet = 'PCQ_Data',  col_types = 'cccccccccdccddddddcccccccc')

treedat <- treeraw %>% 
  select(
    site = `Main site name`, 
    trt = Treatment, 
    scnd = `Secondary site name`,
    zone = `Habitat Type`, 
    quarter = Quarter, 
    date = Date, 
    plot = `Plot #`,
    pcq_direction = `PCQ direction`, 
    species = `Tree species`,
    dbh_cm = `DBH (cm)`, 
    tree_height = `Tree height (m)`, 
    eye_height_m = `Eye height (m)`, 
    dist_to_tree_m = `Distance from center (m)`, 
    angle = `Angle`
  ) %>% 
  mutate(
    date = mdy(date),
    site = case_when(
      site == 'Cockroach Bay' ~ 'Cockroach', 
      site == 'Ft DeSoto' ~ 'Fort DeSoto',
      T ~ site
    )
  )

save(treedat, file = here('data/treedat.RData'))
write.csv(treedat, file = here('data/treedat.csv'), row.names = F)
