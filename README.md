# MLB ABS Challenge Analysis

This project analyzes early trends from MLB’s new Automatic Ball-Strike (ABS) challenge system during the 2026 season.

Using Baseball Savant’s ABS leaderboard data through April 29th, I looked at how hitters are using challenges by pitch type, zone location, and expected vs actual challenge performance.

## Research Question

Are hitters challenging the right pitches, or just the pitches they think are wrong?

## Data Source

Data comes from Baseball Savant’s ABS Challenge Leaderboard.

This analysis uses hitter challenges within the shadow zone:

- Top: Zones 11–13
- Side: Zones 14 and 16
- Bottom: Zones 17–19

Pitch types were grouped as:

- Fastballs (FB): Fastball, Sinker, Cutter
- Offspeed (OS): Changeup, Splitter, Forkball, Screwball
- Breaking (BR): Curveball, Slider, Sweeper, Slurve, Knuckleball

## Key Findings

- Fastballs account for the majority of hitter challenges across all three zones.
- Hitters are most aggressive challenging pitches at the bottom of the zone.
- Bottom-zone fastballs generate the largest number of successful overturns above expectation.
- Edge pitches, especially breaking balls, remain a major challenge blind spot.
- Aggression alone does not guarantee success. The best hitters do not just challenge more; they challenge smarter.

## Files

- `scripts/abs_analysis.R` — R code used to clean, combine, and visualize the ABS data
- `figures/` — exported plots used in the analysis
- `slides/` — LinkedIn carousel / presentation PDF

## Tools Used

- R
- tidyverse
- ggplot2
- ggimage
- Baseball Savant ABS Leaderboard

## Future Questions

- Is challenge success a true skill or just early-season noise?
- Can hitters be trained to challenge smarter?
- Can challenge decision-making become a measurable skill — ABS+?
