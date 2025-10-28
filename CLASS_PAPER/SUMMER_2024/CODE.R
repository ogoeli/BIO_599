library(tidyverse)

df1 <- read.csv("SUMMER_2024.csv")
df2 <- read.csv("Fielddata_analysis_01Sep24.csv")
colnames(df2)
df2_long <- df2 %>%
  # gather the paired columns into long form
  pivot_longer(
    cols = c(H_l1, WP1, FW1, DW1, TW1, H_l2, WP2, FW2, DW2, TW2),
    names_to = c(".value", "Level"),
    names_pattern = "(.*)([12])"
  ) %>%
  mutate(
    Level = ifelse(Level == "1", "L1", "L2"),
    # match df1 column names
    Tree_number = Tree_name,
    Plot = sub("^(.*)-T\\d+$", "\\1", Tree_name),  # pull plot ID from tree name
    Year = 2024  # hardcode since df1 shows only 2024
  ) %>%
  select(
    Plot,
    Tree_number,
    DBH,
    Height,
    X = fid,   # if df1 uses spatial coords, swap real X/Y columns here
    Y = id,
    Species,
    SP_CODE = Species, # df1 uses a short code, you may need to join later
    Stress = Stress_Ind,
    Rep = id,          # placeholder, adjust if df1 has actual Rep column
    Dendro_number = id, # placeholder again
    Level,
    WP,
    FW,
    DW,
    LWC = RWC1,        # df2 has RWC1/RWC2 instead of LWC; pick the right one
    Time = T_coll,
    Year,
    TW
  )


# Export to CSV
write.csv(df2_long, "df2_reshaped.csv", row.names = FALSE)
