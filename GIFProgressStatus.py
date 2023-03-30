import yamlmd
import yaml
import os
import pandas as pd
import ProgressMeasure as pm
from datetime import datetime as dt


def read_meta(indicator_id):
    meta_file = indicator_id + '.yml'
    filepath = os.path.join('indicator-config', meta_file)
    if not os.path.isfile(filepath):
        return None
    with open(filepath, 'r') as stream:
        meta = yaml.safe_load(stream)
    return meta


# def write_meta_md(meta, indicator_id):
#     meta_file = indicator_id + '.yml'
#     filepath = os.path.join('meta', meta_file)
#     yamlmd.write_yamlmd(meta, filepath)
#     # with open(r'output.yaml', 'w') as file:
#     #     outputs = yaml.dump(days, file)


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


def update_progress_diff(diff):
    filepath = os.path.join('progress_diff.yml')
    with open(filepath, 'r') as stream:
        diff_file = yaml.safe_load(stream)

    if diff_file:
        diff_file.update(diff)

    else:
        diff_file = diff

    with open(filepath, 'w') as file:
        outputs = yaml.dump(diff_file, file)


def diff_note(old, new):
    now = dt.now().strftime("%d/%m/%Y %H:%M:%S")
    return 'progress status has changed from ' + old + ' to ' + new + ' (' + now + ')'


def remove_progress_configs(indicator_id):
    ind_str = indicator_id.split('-')
    policy_ind = ind_str[1] in ['a', 'b', 'c', 'd', 'e', 'f']

    if policy_ind:
        meta = read_meta(indicator_id)

        meta['auto_progress_calculation'] = False
        meta['progress_status'] = 'not_available'
        meta['progress_calculation_options'] = []

        update_progress_status_meta(meta, indicator_id)
        print(indicator_id + ': meta updated')


def update_progress_status(indicator_ids):
    all_progress_statuses = {}
    progress_diff = {}

    for ind_id in indicator_ids:
        # Uncomment to turn on ALL indicator calculation
        # turn_on_progress_calc(ind_id)

        # Get data + metadata for calculation
        indicator = merge_indicator(ind_id)
        if indicator is not None:

            if indicator['meta'].get('progress_status'):
                old_pm = indicator['meta'].get('progress_status')
            else:
                old_pm = None

            # Run data + metadata through calculation to get progress
            progress = pm.measure_indicator_progress(indicator)
            all_progress_statuses[ind_id] = progress

            if progress is not None:

                if old_pm and progress != old_pm:
                    progress_diff[ind_id] = diff_note(old_pm, progress)

                print(ind_id + ': ' + progress)
                # Update progress status field in meta
                progress_dict = {'progress_status': progress}
                # Uncomment to update metadata files
                update_progress_status_meta(progress_dict, ind_id)
    return progress_diff


indicator_ids = get_indicator_ids()
diffs = update_progress_status(indicator_ids)
update_progress_diff(diffs)


# individal calculations result ----
# test_ind = merge_indicator('8-2-1')
# test_data = pm.data_progress_measure(test_ind['data'])
# print(test_data)
# print(pm.measure_indicator_progress(test_ind))
