# SOI Sealog Search (*ROV SuBastian*)

A shiny app developed for searching through sealog record produced by ***ROV SuBastian*** (Schmidt Ocean Insistute) with keywords. 

## App Interface and Capabilities

### Get Started

This shiny app searches through selected fields (see section "Search Fields") from *SuBastian*'s sealog file. On the app interface, all settings and functions are located in the sidebar on right side. The simplest work can be done by putting keywords in the search field and hit "Search". The result will be shown on the left side with some general infos, and url links to the YouTube live stream archive if the event was streamed online. You could also download all information along with other sealog fields in a csv file.

You can set a custom range of cruises and dives in which the keyword will be searched by de-selecting "All cruises" or "All dives". 

### Advance Settings

The app uses an self-developed algorithm to enable ambiguous search that could also find keywords with typos. The default setting is a balanced option that has the ability to find keywords with a range of modification without including too many irrelevant results, and is case insensitive.

This might not be always desired when searching for keywords that are included in other words or have similar doppelgangers (e.g. search for "vent" with default settings will hit a lot of entries with the word "event"). In such case, enable the option **Search only for full-word exact match** to find entries that has the exact match of the keyword (the keyword will be wrapped by `\b` in regex in pattern matching). However, this option does not force the search to be case sensitive. To further enforce case sensitive search, select **Case sensitive**.

Selecting the option **Don't search for potential indel-typo** will make the algorithm less likely to find potential typos that decreases or increases the word length (e.g. search for "Rossellidae" and be able to hit "Roselid" or *vice versa*). **Ambiguous search threshold** controls how much tolerance the algorithm is giving for typos. Lower threshold search the keywords stricter and higher threshold allows larger modification but will also includes more irrelevant results. This option also controls the tolerance while searching for results with indel-typo.

## Build Your Own App

The source code for the app is under CC-BY-NC-SA licence and available for everyone to use, modify, and re-distribute with appropriate credits, and any kind of commercial use of the code is prohibited. 

### Sealog Files

Required files are the event only export (csv) of each dive that is produced by SOI sealog system automatically.

Clone the repository to your local, create the folder `sealog/` at the root directory of the app, then place and rename your `csv` files following the structure:

```
sealog
    |
    +- <cruise_id_1>
    |    |
    |    +- <cruise_id_1>_<dive_no_1>_sealogExport.csv
    |    +- <cruise_id_1>_<dive_no_2>_sealogExport.csv
    |    ...
    |
    +- <cruise_id_2>
    |    |
    |    +- <cruise_id_2>_<dive_no_3>_sealogExport.csv
    |    +- <cruise_id_2>_<dive_no_4>_sealogExport.csv
    |    ...
```

### Divestream Archive Metadata

If the dive was streamed by SOI, the app is able to calculate a time-coded url directly links to the archive video on YouTube, according to the event time. To enable this, the divestream metadata needs to be set manually. 

At the root directory, create a new file `replay-meta.yml`. Then manually set up the metadata from each youtube archive from SOI, as follows:

```yaml
# Dive S0798, live archive part 1
- S0798P1:
  dive: 798
  part: 1
  start_UTC: 2025-03-02 07:32:58
  length: 02:24:35
  url: https://youtu.be/vUyaDVyA6Hw

# Dive S0798, live archive part 2
- S0798P2:
  dive: 798
  part: 2
  start_UTC: 2025-03-02 10:59:07
  length: 02:09:10
  url: https://youtu.be/6HSBtPXBwuE
```

- the key of the list should follow the format `S0xxxPx`.
- `start_UTC` is the time code shown on the livestream at the start of the live archive.
- `length` is the length of the live stream.
- `url` should only contains the video id, without further parameters.

### Initiate and Update the System

After finishing setting up sealog files and live stream metadata, run the function `init_cache()` at the root directory to build cache files and set up the system. The script will generate a new folder `cache/` at the root and put all cache files there. The app relies on the cached sealog to effectively perform the search requests as they are stored in RDS file and therefore much memory-friendly and faster to read in than csv files.

If further dives are added to the app in the future, simply run the function `update_cache()` to update the cache files and the system. 

### Search Fields

Default search fields are "event_value", "event_free_text", "event_option.comment", and "event_option.notes". These are the fields where users normally put free text input while logging the dive. If additional fields are desired to be included in the search, pass the desired field names as a character vector to the argument `search.field.custom` in `init_cache()`, which will re-generates all existing cache files.

### Deployment

You can simply run the shiny app locally by sourcing the script `app.R`, or you can deploy the app on an online host. 