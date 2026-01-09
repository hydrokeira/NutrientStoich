# NutrientStoich
This repo contains code for nutrient regime, coherence, and depletion analysis

It includes the following files, which should be run in this order:

Step 1: “Clean_WRTDS_forAnalysis.R”
This file reads in WRTDS output, applies filtering to only include streams with N, P, and Si data, and shifts southern hemisphere rivers by 6 months to align with northern hemisphere seasonality.

Step 2: “NutrientRegimes_Together.R”
This file clusters nutrient time series into regimes.
