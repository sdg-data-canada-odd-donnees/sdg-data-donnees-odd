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


def turn_on_progress_calc(indicator_id):
    """
    Force turns on progress calculation in meta markdown file for whatever indicator is passed.
    :param indicator_id: id for indicator (e.g. 1-1-1)
    :type indicator_id: str
    """

    meta = read_meta(indicator_id)

    if meta.get('reporting_status') == 'complete':
        if not meta.get('auto_progress_calculation'):
            meta['auto_progress_calculation'] = True

        update_progress_status_meta(meta, indicator_id)


def remove_progress_configs(indicator_id):
    """
    Function to mass remove all progress statuses for policy indicators (i.e. any indicator with an alphabetic target)
    Args:
        indicator_id: str. Identifier of indicator (e.g. 1-1-1).

    """
    ind_str = indicator_id.split('-')
    policy_ind = ind_str[1] in ['a', 'b', 'c', 'd', 'e', 'f']

    if policy_ind:
        meta = read_meta(indicator_id)

        meta['auto_progress_calculation'] = False
        meta['progress_status'] = 'not_available'
        meta['progress_calculation_options'] = []

        update_progress_status_meta(meta, indicator_id)
        print(indicator_id + ': meta updated')


def update_progress_direction(indicator_id, direction):

    meta = read_meta(indicator_id)
    new_ind_direction = direction.get(indicator_id)
    prog_calc_options = meta.get('progress_calculation_options')

    if prog_calc_options:
        prog_calc_options = prog_calc_options[0]
        if prog_calc_options.get('direction'):
            old_ind_direction = prog_calc_options.get('direction')
        else:
            old_ind_direction = ''
    else:
        old_ind_direction = ''

    if old_ind_direction == 'down':
        old_ind_direction = 'negative'
    elif old_ind_direction == 'up':
        old_ind_direction = 'positive'

    if indicator_id in direction.keys():
        if new_ind_direction != old_ind_direction:
            print('changing ' + indicator_id + ' progress direction')
            print('old_ind_direction: ' + old_ind_direction)
            print('new_ind_direction: ' + new_ind_direction)

            if new_ind_direction == 'binary':
                if meta.get('reporting_status') == 'complete':
                    meta['progress_status'] = 'target_achieved'

            else:
                if meta.get('progress_calculation_options'):
                    meta['progress_calculation_options'][0]['direction'] = new_ind_direction
                else:
                    meta['progress_calculation_options'] = [{'direction': new_ind_direction}]

            update_progress_status_meta(meta, indicator_id)


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
            progress = pm.measure_indicator_progress(indicator, False)
            all_progress_statuses[ind_id] = progress

            if progress is not None:

                if old_pm and progress != old_pm:
                    progress_diff[ind_id] = diff_note(old_pm, progress)

                print(ind_id + ': ' + progress)
                # Update progress status field in meta
                progress_dict = {'progress_status': progress}
                # Uncomment to update metadata files
                # update_progress_status_meta(progress_dict, ind_id)
    return progress_diff


indicator_ids = get_indicator_ids()

# read in progress directions
# filepath = 'indicator_progress_direction.yml'
# with open(filepath, 'r') as stream:
#     direction = yaml.safe_load(stream)
#
# for ind_id in indicator_ids:
#     update_progress_direction(ind_id, direction)
#     turn_on_progress_calc(ind_id)
#     remove_progress_configs(ind_id)

diffs = update_progress_status(indicator_ids)
# update_progress_diff(diffs)

# individal calculations result ----
# test_ind = merge_indicator('4-2-2')
# test_data = pm.data_progress_measure(test_ind['data'])
# print(test_data)
# print(pm.measure_indicator_progress(test_ind))
