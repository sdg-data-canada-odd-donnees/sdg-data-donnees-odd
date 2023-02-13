import yamlmd
import yaml
import os
import pandas as pd
import ProgressMeasure as pm


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


def update_progress_status(progress_dict, indicator_id):
    meta_file = indicator_id + '.yml'
    filepath = os.path.join('indicator-config', meta_file)
    with open(filepath, 'r') as stream:
        meta = yaml.safe_load(stream)
    meta.update(progress_dict)
    with open(filepath, 'w') as file:
        outputs = yaml.dump(meta, file)
    

indicator_ids = get_indicator_ids()

for ind_id in indicator_ids:
    print(ind_id)
    # Uncomment to turn on ALL indicator calculation
    # turn_on_progress_calc(ind_id)

    # Get data + metadata for calculation
    indicator = merge_indicator(ind_id)
    if indicator is not None:
        # Run data + metadata through calculation to get progress
        progress = pm.measure_indicator_progress(indicator)
        if progress is not None:
            print(ind_id + ': ' + progress)
            # Update progress status field in meta
            progress_dict = {'progress_status': progress}
            # Uncomment to update metadata files
            update_progress_status(progress_dict, ind_id)

# individal calculations result ----
# test_ind = merge_indicator('8-2-1')
# test_data = pm.data_progress_measure(test_ind['data'])
# print(test_data)
# print(pm.measure_indicator_progress(test_ind))
