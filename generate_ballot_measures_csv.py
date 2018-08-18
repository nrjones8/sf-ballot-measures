import csv
import hashlib
import os.path
import re
import requests
import time

import pandas as pd
from bs4 import BeautifulSoup

BASE_SFPL_URL = 'https://sfpl.org'
ALL_PROPS_URL = '{}/index.php?pg=2000027201&PropTitle=&Description=&PropLetter=&Month=&Year=&submit=Search'.format(
    BASE_SFPL_URL
)

PROP_LETTER_OR_NUM = 'prop_letter_or_num'
PROP_TITLE = 'prop_title'

# Data about when the election happened
ELECTION_DATE = 'election_date'
ELECTION_YEAR = 'election_year'
ELECTION_MONTH = 'election_month'
ELECTION_DAY = 'election_day'

# Data about outcome of prop
VOTE_COUNT_YES = 'num_yes_votes'
VOTE_COUNT_NO = 'num_no_votes'
VOTE_PCT_YES = 'pct_yes_votes'
VOTE_PCT_NO = 'pct_no_votes'

PCT_REQUIRED_TO_PASS = 'pct_required_to_pass'
PASS_OR_FAIL = 'pass_or_fail'

# E.g. Did BoS put it on ballot, or were signatures collected?
SOURCE_OF_PROP = 'source_of_prop'
# The raw data we parsed out; so this field will have different entries for e.g. "Board of Supervisors"
# and "Supervisors", whereas the SOURCE_OF_PROP field is normalized
RAW_SOURCE_OF_PROP = 'raw_source_of_prop'
PROP_KIND = 'prop_kind'
PROP_DESCRIPTION = 'description'

PROP_MORE_INFO_URL = 'prop_url'

ALL_COLUMNS = [
    ELECTION_DATE,
    ELECTION_YEAR,
    ELECTION_MONTH,
    ELECTION_DAY,

    PROP_LETTER_OR_NUM,
    PROP_TITLE,

    VOTE_COUNT_YES,
    VOTE_COUNT_NO,
    VOTE_PCT_YES,
    VOTE_PCT_NO,

    PCT_REQUIRED_TO_PASS,
    PASS_OR_FAIL,

    RAW_SOURCE_OF_PROP,
    SOURCE_OF_PROP,

    PROP_KIND,
    PROP_DESCRIPTION,
    PROP_MORE_INFO_URL
]

class SourceOfMeasure:
    SOURCE_SUPERVISORS = 'supervisors'
    SOURCE_INITIATIVE = 'initiative'
    SOURCE_MAYOR = 'mayor'
    SOURCE_LABOR_DISPUTE = 'labor_dispute'
    SOURCE_REFERENDUM = 'referendum'
    SOURCE_ETHICS_COMM = 'ethics_commission'

    SOURCE_UNKNOWN = 'unknown'
    SOURCE_OTHER = 'other'

def convert_int_with_commas(s):
    return int(s.replace(',', '').strip())

def classify_source_of_measure(raw_source):
    if not raw_source:
        return SourceOfMeasure.SOURCE_UNKNOWN

    lower_source = raw_source.lower()

    if 'supervisors' in lower_source or 'Superviosr' in lower_source:
        # There are a few misspelled entries, handle those
        return SourceOfMeasure.SOURCE_SUPERVISORS
    elif lower_source == 'initiative':
        return SourceOfMeasure.SOURCE_INITIATIVE
    elif lower_source == 'mayor':
        return SourceOfMeasure.SOURCE_MAYOR
    elif lower_source == 'labor dispute':
        return SourceOfMeasure.SOURCE_LABOR_DISPUTE
    elif lower_source == 'referendum':
        return SourceOfMeasure.SOURCE_REFERENDUM
    elif lower_source == 'ethics commission':
        return SourceOfMeasure.SOURCE_ETHICS_COMM
    else:
        return lower_source


def parse_pct_of_votes(raw_pct_of_votes):
    # `raw_pct_of_votes` looks like "Yes: 80.4%  /  No: 19.6%"

    # Thank you https://regex101.com/, you are amazing
    field_regex = 'Yes: ([\d|.]+)(.*No:\s+)([\d|.]+)'

    m = re.match(field_regex, raw_pct_of_votes)
    groups = m.groups()

    return {'yes': groups[0], 'no': groups[2]}

def parse_vote_count(raw_vote_count):
    field_regex = 'Yes: ([\d|,]+)(.*No:\s+)([\d|,]+)'

    m = re.match(field_regex, raw_vote_count)
    groups = m.groups()

    return {
        'yes': convert_int_with_commas(groups[0]),
        'no': convert_int_with_commas(groups[2])
    }

def parse_pct_required_to_pass(raw_pct_required):
    field_regex = '([\d|.]+)(\%)'
    if '66' in raw_pct_required:
        return 66.66
    else:
        field_regex = '([\d]+)(.*)'
        m = re.match(field_regex, raw_pct_required)
        if m is not None:
            match_groups = m.groups()
            if len(match_groups) > 0:
                return m.groups()[0]

        return 'unknown'

def parse_date_field(date_str):
    return [int(part) for part in date_str.split('/')]

def _parse_raw_details(raw_details_dict):
    direct_mappings = {
        'Kind': PROP_KIND,
        'Description': PROP_DESCRIPTION,
        'Pass or Fail': PASS_OR_FAIL,
        'How it was placed on the ballot': RAW_SOURCE_OF_PROP,
    }

    cleaned_details = {
        cleaned_key_name: raw_details_dict[key_from_website]
        for key_from_website, cleaned_key_name in direct_mappings.iteritems()
    }
    cleaned_details[SOURCE_OF_PROP] = classify_source_of_measure(
        cleaned_details[RAW_SOURCE_OF_PROP]
    )

    # TODO - parse these fields too:
    # Percentage of votes required to pass 50%+1
    pct_of_votes_values = parse_pct_of_votes(raw_details_dict['Percentage of votes'])
    count_of_votes_values = parse_vote_count(raw_details_dict['Vote Count'])
    pct_votes_required_to_pass = parse_pct_required_to_pass(raw_details_dict['Percentage of votes required to pass'])

    voting_results_data = {
        VOTE_PCT_YES: pct_of_votes_values['yes'],
        VOTE_PCT_NO: pct_of_votes_values['no'],

        VOTE_COUNT_YES: count_of_votes_values['yes'],
        VOTE_COUNT_NO: count_of_votes_values['no'],

        PCT_REQUIRED_TO_PASS: pct_votes_required_to_pass,
    }
    cleaned_details.update(voting_results_data)

    # Add in extra data here
    # 1. Clean up Supervisors thing
    # 2. Clean up prop_kind (e.g. "Bond Issue" and "Bond issue")
    # 2. was it a presidential year?

    return cleaned_details

def process_detailed_prop_page(url):
    detailed_content = get_page_content(url)
    soup = BeautifulSoup(detailed_content, 'html.parser')
    table = soup.find('table', {'class': 'standard'})

    prop_details = {}
    for row in table.findAll('tr'):
        field_name = row.find('th').text.strip()
        field_value = row.find('td').text.strip()
        prop_details[field_name] = field_value

        # Last row we care about is "Description" - stop after that
        if field_name == 'Description':
            break

    return _parse_raw_details(prop_details)

def write_csv(dataset, csv_path, column_names):
    """
    dataset
    csv_path - path to save data to
    column_names - the names of the columns for the CSV, in the desired order of columns in the CSV
    """
    with open(csv_path, 'w') as f:
        writer = csv.DictWriter(f, column_names)
        writer.writeheader()
        for row in dataset:
            encoded_row = {}
            # TODO - this is janky, fix it
            for k, v in row.iteritems():
                value_for_writing = v.encode('ascii', 'ignore') if type(v) != int else v
                encoded_row[k] = value_for_writing

            writer.writerow(encoded_row)


def write_file(path, content):
    with open(path, 'w') as f:
        f.write(content)


def read_file(path):
    with open(path, 'r') as f:
        content = f.read()

    return content

def get_page_content(url, check_cache=True):
    hashed_url = hashlib.md5(url).hexdigest()
    cached_path = 'cached_pages/{}'.format(hashed_url)

    if check_cache and os.path.isfile(cached_path):
        print 'Reading from cached path {}'.format(cached_path)
        raw_content = read_file(cached_path)
    else:
        print 'Not found locally, requesting from {} and writing to {}'.format(
            url, cached_path
        )
        resp = requests.get(url)
        raw_content = resp.content
        write_file(cached_path, raw_content)

    return raw_content

def main():
    all_props_content = get_page_content(ALL_PROPS_URL)
    soup = BeautifulSoup(all_props_content, 'html.parser')
    table = soup.find('table', {'class': 'standard'})
    all_dicts = []

    for i, row in enumerate(table.findAll('tr')):
        if i == 0:
            print 'Skipping first row, it is a header:', row
            continue

        try:
            cell_objects = row.findAll('td')
            link_to_more_info = cell_objects[0].find('a').attrs['href']
            full_url_to_more_info = '{}/{}'.format(BASE_SFPL_URL, link_to_more_info)

            cells = [c.text for c in cell_objects]
            prop_letter_or_num, prop_title, prop_date, prop_outcome = cells
            prop_month, prop_day, prop_year = parse_date_field(prop_date)

            prop_details_dict = process_detailed_prop_page(full_url_to_more_info)
            prop_details_dict.update({
                PROP_LETTER_OR_NUM: prop_letter_or_num,
                PROP_TITLE: prop_title,

                ELECTION_DATE: prop_date,
                ELECTION_YEAR: prop_year,
                ELECTION_MONTH: prop_month,
                ELECTION_DAY: prop_day,

                PASS_OR_FAIL: prop_outcome,
                PROP_MORE_INFO_URL: full_url_to_more_info,
            })
            all_dicts.append(prop_details_dict)
        except Exception as e:
            print 'Got an exception, but will continue - happened on:', full_url_to_more_info, row
            print e


        print 'Done with', cells

    df = pd.DataFrame(all_dicts)
    df.to_csv('data/ballot_measure_history.csv', columns=ALL_COLUMNS, index=False, encoding='utf-8')
    df.to_json('data/ballot_measure_history.json', orient='records')

if __name__ == '__main__':
    main()