library(tidyverse)
library(ggimage)
library(scales)

##################### Bottom of the Zone #######################

abs_bottom_FB <- read.csv("abs-challenges-2026-batting-team-bottom-FB.csv")
abs_bottom_FB$zone <- "bottom"
abs_bottom_FB$pitch_type <- "FB"

abs_bottom_OS <- read.csv("abs-challenges-2026-batting-team-bottom-OS.csv")
abs_bottom_OS$zone <- "bottom"
abs_bottom_OS$pitch_type <- "OS"

abs_bottom_BR <- read.csv("abs-challenges-2026-batting-team-bottom-BR.csv")
abs_bottom_BR$zone <- "bottom"
abs_bottom_BR$pitch_type <- "BR"

####################### Side of the Zone ##############################

abs_side_FB <- read.csv("abs-challenges-2026-batting-team-side-FB.csv")
abs_side_FB$zone <- "side"
abs_side_FB$pitch_type <- "FB"

abs_side_OS <- read.csv("abs-challenges-2026-batting-team-side-OS.csv")
abs_side_OS$zone <- "side"
abs_side_OS$pitch_type <- "OS"

abs_side_BR <- read.csv("abs-challenges-2026-batting-team-side-BR.csv")
abs_side_BR$zone <- "side"
abs_side_BR$pitch_type <- "BR"

########################## Top of the Zone ################################

abs_top_FB <- read.csv("abs-challenges-2026-batting-team-top-FB.csv")
abs_top_FB$zone <- "top"
abs_top_FB$pitch_type <- "FB"

abs_top_OS <- read.csv("abs-challenges-2026-batting-team-top-OS.csv")
abs_top_OS$zone <- "top"
abs_top_OS$pitch_type <- "OS"

abs_top_BR <- read.csv("abs-challenges-2026-batting-team-top-BR.csv")
abs_top_BR$zone <- "top"
abs_top_BR$pitch_type <- "BR"

#########################################################################
# Combine Data
#########################################################################

abs_batting_team <- bind_rows(
  abs_bottom_FB,
  abs_bottom_OS,
  abs_bottom_BR,
  abs_side_FB,
  abs_side_OS,
  abs_side_BR,
  abs_top_FB,
  abs_top_OS,
  abs_top_BR
) %>%
  mutate(
    zone = factor(zone, levels = c("bottom", "side", "top")),
    pitch_type = factor(pitch_type, levels = c("FB", "OS", "BR"))
  )

#########################################################################
# Team logo URLs
#########################################################################

team_logos <- tibble::tribble(
  ~team_abbr, ~logo_url,
  "AZ",  "https://a.espncdn.com/i/teamlogos/mlb/500/ari.png",
  "ARI", "https://a.espncdn.com/i/teamlogos/mlb/500/ari.png",
  "ATH", "https://a.espncdn.com/i/teamlogos/mlb/500/ath.png",
  "ATL", "https://a.espncdn.com/i/teamlogos/mlb/500/atl.png",
  "BAL", "https://a.espncdn.com/i/teamlogos/mlb/500/bal.png",
  "BOS", "https://a.espncdn.com/i/teamlogos/mlb/500/bos.png",
  "CHC", "https://a.espncdn.com/i/teamlogos/mlb/500/chc.png",
  "CWS", "https://a.espncdn.com/i/teamlogos/mlb/500/chw.png",
  "CIN", "https://a.espncdn.com/i/teamlogos/mlb/500/cin.png",
  "CLE", "https://a.espncdn.com/i/teamlogos/mlb/500/cle.png",
  "COL", "https://a.espncdn.com/i/teamlogos/mlb/500/col.png",
  "DET", "https://a.espncdn.com/i/teamlogos/mlb/500/det.png",
  "HOU", "https://a.espncdn.com/i/teamlogos/mlb/500/hou.png",
  "KC",  "https://a.espncdn.com/i/teamlogos/mlb/500/kc.png",
  "LAA", "https://a.espncdn.com/i/teamlogos/mlb/500/laa.png",
  "LAD", "https://a.espncdn.com/i/teamlogos/mlb/500/lad.png",
  "MIA", "https://a.espncdn.com/i/teamlogos/mlb/500/mia.png",
  "MIL", "https://a.espncdn.com/i/teamlogos/mlb/500/mil.png",
  "MIN", "https://a.espncdn.com/i/teamlogos/mlb/500/min.png",
  "NYM", "https://a.espncdn.com/i/teamlogos/mlb/500/nym.png",
  "NYY", "https://a.espncdn.com/i/teamlogos/mlb/500/nyy.png",
  "PHI", "https://a.espncdn.com/i/teamlogos/mlb/500/phi.png",
  "PIT", "https://a.espncdn.com/i/teamlogos/mlb/500/pit.png",
  "SD",  "https://a.espncdn.com/i/teamlogos/mlb/500/sd.png",
  "SEA", "https://a.espncdn.com/i/teamlogos/mlb/500/sea.png",
  "SF",  "https://a.espncdn.com/i/teamlogos/mlb/500/sf.png",
  "STL", "https://a.espncdn.com/i/teamlogos/mlb/500/stl.png",
  "TB",  "https://a.espncdn.com/i/teamlogos/mlb/500/tb.png",
  "TEX", "https://a.espncdn.com/i/teamlogos/mlb/500/tex.png",
  "TOR", "https://a.espncdn.com/i/teamlogos/mlb/500/tor.png",
  "WSH", "https://a.espncdn.com/i/teamlogos/mlb/500/wsh.png"
)

##############################################################
# 1. What gets challenged?
##############################################################

challenge_freq <- abs_batting_team %>%
  group_by(zone, pitch_type) %>%
  summarise(
    total_challenges = sum(n_challenges, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(zone) %>%
  mutate(
    pct_challenges = total_challenges / sum(total_challenges)
  )

ggplot(challenge_freq, aes(
  x = zone,
  y = pct_challenges,
  fill = pitch_type
)) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  scale_y_continuous(
    labels = percent_format()
  ) +
  labs(
    title = "What Gets Challenged?",
    subtitle = "Percent of challenges within each zone",
    x = "Zone",
    y = "Percent of Challenges",
    fill = "Pitch Type"
  ) +
  theme_minimal()


##############################################################
# 2. Where are hitters aggressive/passive?
##############################################################

challenge_aggression <- abs_batting_team %>%
  group_by(zone, pitch_type) %>%
  summarise(
    avg_challenge_rate_diff = mean(
      exp_rate_challenges_diff,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

ggplot(challenge_aggression, aes(
  x = avg_challenge_rate_diff,
  y = zone,
  color = pitch_type
)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(size = 5) +
  scale_x_continuous(
    labels = percent_format(accuracy = 1)
  ) +
  labs(
    title = "Where Are Hitters Aggressive or Passive?",
    subtitle = "Right of 0 = challenged more than expected",
    x = "Challenge Rate Difference",
    y = "Zone",
    color = "Pitch Type"
  ) +
  theme_minimal()


##############################################################
# 3. Where are hitters actually creating challenge value?
##############################################################

challenge_value <- abs_batting_team %>%
  group_by(zone, pitch_type) %>%
  summarise(
    total_net_value = sum(
      net_chal_gained,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

ggplot(challenge_value, aes(
  x = total_net_value,
  y = zone,
  fill = pitch_type
)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_col(
    position = position_dodge(width = 0.75),
    width = 0.7
  ) +
  labs(
    title = "Hitter Challenge Performance vs Expected",
    subtitle = "Positive = more overturns than expected",
    x = "Net Overturns vs Expected",
    y = "Zone",
    fill = "Pitch Type"
  ) +
  theme_minimal()

##############################################################
# 4. Are hitters overconfident?
##############################################################

pitch_decisions <- abs_batting_team %>%
  group_by(zone, pitch_type) %>%
  summarise(
    challenge_diff = mean(
      exp_rate_challenges_diff,
      na.rm = TRUE
    ),
    
    actual_overturns = sum(
      n_overturns,
      na.rm = TRUE
    ),
    
    total_challenges = sum(
      n_challenges,
      na.rm = TRUE
    ),
    
    expected_overturns = sum(
      exp_rate_overturns * n_challenges,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    overturn_diff =
      (actual_overturns - expected_overturns) /
      total_challenges
  )

ggplot(pitch_decisions, aes(
  x = challenge_diff,
  y = overturn_diff,
  color = pitch_type,
  shape = zone
)) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  
  annotate(
    "text",
    x = .03,
    y = .05,
    label = "Smart\nAggression",
    fontface = "bold"
  ) +
  
  annotate(
    "text",
    x = -.035,
    y = .05,
    label = "Selective\nSuccess",
    fontface = "bold"
  ) +
  
  annotate(
    "text",
    x = .03,
    y = -.08,
    label = "Overconfident",
    fontface = "bold"
  ) +
  
  annotate(
    "text",
    x = -.035,
    y = -.08,
    label = "Missed\nOpportunities",
    fontface = "bold"
  ) +
  
  geom_point(size = 5) +
  
  scale_x_continuous(
    labels = percent_format(accuracy = 1)
  ) +
  
  scale_y_continuous(
    labels = percent_format(accuracy = 1)
  ) +
  
  labs(
    title = "Are Hitters Overconfident?",
    subtitle = "Challenge frequency vs challenge quality",
    x = "Challenge Rate Difference",
    y = "Overturn Rate vs Expected",
    color = "Pitch Type",
    shape = "Zone"
  ) +
  
  theme_minimal()


##############################################################
# 5. Do aggressive hitters benefit their teams?
##############################################################

team_aggression <- abs_batting_team %>%
  group_by(entity_name, team_abbr) %>%
  summarise(
    total_team_value = sum(
      total_vs_expected,
      na.rm = TRUE
    ),
    
    avg_hitter_challenge_rate_diff = mean(
      exp_rate_challenges_diff,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  left_join(team_logos, by = "team_abbr")

ggplot(team_aggression, aes(
  x = avg_hitter_challenge_rate_diff,
  y = total_team_value
)) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  
  annotate("text", x = 0.035, y = 10, label = "Aggressive + Successful", size = 4, fontface = "bold") +
  annotate("text", x = -0.025, y = 10, label = "Passive + Successful", size = 4, fontface = "bold") +
  annotate("text", x = -0.025, y = -14, label = "Passive + Unsuccessful", size = 4, fontface = "bold") +
  annotate("text", x = 0.035, y = -14, label = "Aggressive + Unsuccessful", size = 4, fontface = "bold") +
  
  geom_image(
    aes(image = logo_url),
    size = 0.07
  ) +
  
  scale_x_continuous(
    labels = percent_format(accuracy = 1)
  ) +
  
  labs(
    title = "Do Aggressive Hitters Benefit Their Teams?",
    subtitle = "Includes challenges by and against each team",
    x = "Hitter Challenge Rate Difference",
    y = "Team Net Overturns vs Expected"
  ) + theme_minimal()
