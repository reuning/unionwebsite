[![Netlify Status](https://api.netlify.com/api/v1/badges/df4ca112-b4f3-4113-8938-c638d531bc6f/deploy-status)](https://app.netlify.com/sites/amazing-mayer-87367c/deploys)
[![Data automatically updating](https://github.com/reuning/unionwebsite/actions/workflows/update.yaml/badge.svg)](https://github.com/reuning/unionwebsite/actions/workflows/update.yaml)

# unionwebsite

Github for [unionelections.org](https://unionelections.org).


- Data, plots and markdown pages are generated using files `gen/scripts`
  - Scraping script is adapted from [nlrb-cases](https://github.com/labordata/nlrb-cases)
- The website is built using [Quarto](https://quarto.org/). 

# Note on union name matching

The `National` union affiliation is assigned in `gen/scripts/Scripts.R` by matching free-text `Labor_Union` names against a local dictionary (`gen/data/union_dictionary_exact.csv` and `gen/data/union_dictionary_substrings.csv`). These dictionaries were reconstructed from historical data after the original Google Sheets lookup was accidentally deleted; they can be regenerated with `gen/scripts/rebuild_union_dictionary.R`.

# Note on AI use

Code and data analysis in this repository are developed with the assistance of a large language model (LLM), accessed through [Positron](https://positron.posit.co/)'s Posit Assistant with [OpenRouter](https://openrouter.ai/) as the model provider. AI assistance is used to help write, refactor, and debug code and to help with data analysis and interpretation; specific models may vary by session. All AI-generated output is reviewed by a human before it is committed, and a human remains responsible for the correctness of the data and code.

Commits made with meaningful AI assistance carry a Git trailer for attribution:

    Co-authored-by: Posit Assistant <reuning+posit-assistant@users.noreply.github.com>

The `git co-ai` alias commits with this trailer attached, and `.gitmessage` is the commit message template (see the commented trailer line inside).

# Things to do

- Fix the "NA"s for certification decisions.