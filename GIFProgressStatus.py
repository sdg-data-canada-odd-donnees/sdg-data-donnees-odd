# import packages used in this script
import os
import pandas as pd
import ProgressMeasure as pm
import yaml
from datetime import datetime as dt


def read_meta(indicator_id):
    meta_file = indicator_id + '.yml'
    filepath = os.path.join('indicator-config', meta_file)
    if not os.path.isfile(filepath):
        return None
    with open(filepath, 'r') as stream:
        meta = yaml.safe_load(stream)
    return meta


def read_data(indicator_id):
    data_file = 'indicator_' + indicator_id + '.csv'
    filepath = os.path.join('data', data_file)
    if not os.path.isfile(filepath):
        return None
    data = pd.read_csv(filepath)
    return data


def get_indicator_ids():
    ids = []
    for file in os.listdir('meta'):
        # take just the file name (remove file extension) to get indicator id and add to list
        if file.endswith('.yml'):
            id_name = file[:-4]
            ids.append(id_name)
    return ids


def merge_indicator(indicator_id):
    test_meta = read_meta(indicator_id)
    test_data = read_data(indicator_id)
    if test_data is None or test_meta is None:
        return None
    indicator = {'meta': test_meta, 'data': test_data}
    return indicator


def update_progress_status_meta(progress_dict, indicator_id):
    meta_file = indicator_id + '.yml'
    filepath = os.path.join('indicator-config', meta_file)
    with open(filepath, 'r') as stream:
        meta = yaml.safe_load(stream)
    meta.update(progress_dict)
    with open(filepath, 'w') as file:
        outputs = yaml.dump(meta, file)


def diff_note(old, new):
    now = dt.now().strftime("%d/%m/%Y %H:%M:%S")
    return 'progress status has changed from ' + old + ' to ' + new + ' (' + now + ')'
def update_progress_diff(diff):
    filepath = os.path.join('progress_diff.yml')
    with open(filepath, 'r') as stream:
        diff_file = yaml.safe_load(stream)
    diff_file.update(diff)
    with open(filepath, 'w') as file:
        outputs = yaml.dump(diff_file, file)


def update_progress_status(indicator_ids):
    all_progress_statuses = {}
    progress_diff = {}

    for ind_id in indicator_ids:

        print(ind_id)
        # Get data + metadata for calculation
        indicator = merge_indicator(ind_id)
        if indicator is not None:

            # get the old progress measure
            if indicator['meta'].get('progress_status'):
                old_pm = indicator['meta'].get('progress_status')
            else:
                old_pm = None

            # Run data + metadata through calculation to get progress
            progress = pm.progress_measure(indicator)
            all_progress_statuses[ind_id] = progress

            if progress is not None:

                # check if the newly calculate progress measure is different from the old one
                if old_pm and progress != old_pm:
                    progress_diff[ind_id] = diff_note(old_pm, progress)

                # Update progress status field in meta
                progress_dict = {'progress_status': progress}
                # Uncomment to update metadata files
                update_progress_status_meta(progress_dict, ind_id)
    # return the progress differences
    return progress_diff


def get_goal_progress(indicator_ids):

    scores = {}

    for ind_id in indicator_ids:

        indicator = merge_indicator(ind_id)
        score = pm.get_indicator_score(indicator)

        scores[ind_id] = score

    # manual_input = {'8-2-1': -5, '9-2-1': 4.273685, '15-1-2': 0.02720645}
    # scores.update(manual_input)

    filepath = os.path.join('indicator_scores.yml')
    with open(filepath, 'w') as file:
        outputs = yaml.dump(scores, file)

    return scores


def output_calculation_components(indicator_ids):

    components_dict = {}

    for ind_id in indicator_ids:

        print(ind_id)

        indicator = merge_indicator(ind_id)

        if indicator is None:
            components_dict[ind_id] = {
                'progress_status': 'not_available'
            }

        else:
            data = pm.data_progress_measure(indicator['data'])
            meta = indicator['meta']

            if data is None or meta is None or data.shape[0] < 2:
                components_dict[ind_id] = {
                    'progress_status': 'not_available'
                }

            else:

                if indicator['meta'].get('progress_calculation_options'):
                    opts = dict(indicator['meta'].get('progress_calculation_options')[0])
                else:
                    opts = {}

                years = data["Year"]
                current_year = years.max()

                if not opts.get('base_year'):
                    opts['base_year'] = 2015
                if not opts.get('target_year'):
                    opts['target_year'] = 2030

                if opts['base_year'] not in years.values:
                    opts['base_year'] = years[years > opts['base_year']].min()

                current_value = data.Value[data.Year == float(current_year)].values[0]
                base_value = data.Value[data.Year == float(opts['base_year'])].values[0]

                if pm.measure_indicator_progress(data, meta) is None:
                    progress_calculation_value = None
                else:
                    progress_calculation_value = str(pm.measure_indicator_progress(data, meta).get('value'))

                components_dict[ind_id] = {
                    'base_year': str(opts.get('base_year')),
                    'current_year': str(current_year),
                    'base_value': str(base_value),
                    'current_value': str(current_value),
                    'direction': str(opts.get('direction')),
                    'target_year': str(opts.get('target_year')),
                    'target': str(opts.get('target')),
                    'progress_calculation_value': progress_calculation_value,
                    'progress_status': str(pm.progress_measure(indicator))
                }

        filepath = os.path.join('indicator_calculation_components.yml')
        with open(filepath, 'w') as file:
            outputs = yaml.dump(components_dict, file)

    return components_dict


# get all indicator ids from listed data files
indicator_ids = get_indicator_ids()

# calculate & update progress measures
diffs = update_progress_status(indicator_ids)

# output all components of progress calculation
output_calculation_components(indicator_ids)

# calculate indicator scores & output
scores = get_goal_progress(indicator_ids)

# if there have been changes to any progress measure, update the difference file
if diffs:
    update_progress_diff(diffs)
    