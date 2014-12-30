# party colors

colors = c(
  "VR"   = "#AAAAAA", # Vasenryhmä                      -- light grey
  "VAS"  = "#E41A1C", # Vasemmistoliitto                -- red
  "VIHR" = "#B3DE69", # Vihreä liitto                   -- light green
  "SD"   = "#F781BF", # Sosialidemokraattinen Puolue    -- pink
  "KESK" = "#4DAF4A", # Suomen Keskusta                 -- green
  "R"    = "#FFFF33", # Ruotsalainen kansanpuolue (RKP) -- yellow
  "PS"   = "#053061", # Perussuomalaiset                -- dark blue
  "KD"   = "#FF7F00", # Kristillisdemokraatit           -- orange
  "KOK"  = "#01665E", # Kansallinen Kokoomus            -- dark green/teal
  "M11"  = "#80B1D3"  # Muutos 2011                     -- light blue
)

# ParlGov Left/Right scores

scores = c(
  "VR"   = 2.2,
  "VAS"  = 2.2,
  "VIHR" = 3.6,
  "SD"   = 3.6,
  "KESK" = 5.8,
  "R"    = 6.4,
  "PS"   = 6.6,
  "KD"   = 7.2,
  "KOK"  = 7.2,
  "M11"  = Inf # missing
)

stopifnot(names(colors) == names(scores))
order = names(colors)[ order(scores) ]
