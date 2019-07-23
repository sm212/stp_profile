# Mid STP profile code

Code used to generate graphs, tables, and maps for the Mid & South Essex STP profile pack.

The Public Health team at Essex County Council were commissioned to produce a profile for the Mid Essex STP. The profile pulls together a range of publically available indicator data across a range of health topics, and produces a well structured, high level pack for senior officials and comissioners.

This repo contains all code used for downloading and analysing the public indicators. A consistent theme is applied to all outputs.

## Outputs

Run `main.r` to generate all output files.

`main.r` reads from `indicator_list.csv` and pulls data from [Fingertips](https://fingertips.phe.org.uk/). `indicator_list.csv` was made by the Public Health team - `list_data.r` outputs a list of all indicators available at MSOA, District, and CCG level. The Public Health team then looked over this list and selected a list of indicators which would be useful to include in the profile pack.