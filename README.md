# Historical Ballot Measures Data for San Francisco

The San Francisco Public Library (SFPL) maintains historical records of ballot propositions from San Francisco elections over the years. That database can be found on the [SFPL website](https://sfpl.org/index.php?pg=2000027201&PropTitle=&Description=&PropLetter=&Month=&Year=&submit=Search). However, it can be difficult to navigate or look at structured data about these elections and results. The Department of Elections maintains some [structured data](https://sfelections.org/tools/election_data/dataset.php?ATAB=d1970-01-01) on past propositions, but the dataset only includes basic information (e.g. the prop letter, what % voted for it, and the title). It does not contain a more detailed description of the propositions, or links to further information.

That's where this repo comes in! The `generate_ballot_measures_csv.py` script scrapes the SFPL's website to build a structured CSV of past propositions - including their full description, and links to individual SFPL pages that describe the proposition in more detail. That CSV can be found [in the data directory of this repo](https://github.com/nrjones8/sf-ballot-measures/blob/master/data/ballot_measure_history.csv).


