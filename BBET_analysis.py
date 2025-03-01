import eyekit
import json
import statistics
import pandas as pd
import matplotlib.pyplot as plt
import seaborn
import numpy as np
import os
import csv

px_per_character = 16
font_height = 26.66666672
screen_height = 1080

participant_ids = [3,4]

data_path = 'C:/Users/annal/OneDrive/Documents/GitHub/bilingualboundary/data'
stimuli_path = 'C:/Users/annal/OneDrive/Documents/GitHub/bilingualboundary/stimuli'

#############################################################
# run separately for each participant data file to check it #
id = 3

# INPUT PARTICIPANT SCORES
lextale_score = 86.25
morph_score = 95
#############################################################

sbj_data_path = f'{data_path}/sbj_{id}'

file_asc = eyekit.io.import_asc(file_path=f'{sbj_data_path}/{id}.asc')
# this is now a list of trials
# each trial is a dictionary with one key ('fixations'),
# which has as a value a list of FixationSequences
# each FixationSequence is a list of Fixations

# import raw ASC file
with open(f'{sbj_data_path}/{id}.asc', 'r') as file:
        asc_data = file.read()
        asc_data = asc_data.split("\n") # split text file into list
        asc_data = asc_data[:-1] # remove last \n from text file

# export as txt file
with open(f'{sbj_data_path}/{id}.txt', 'w') as f:
    for line in asc_data:
        f.write(f"{line}\n")

# read created txt file
with open(f"{sbj_data_path}/{id}.txt", "r", encoding='utf-8') as file_txt:
        file_txt = file_txt.read()
        file_txt = file_txt.split("\n") # split text file into list
        file_txt = file_txt[:-1] # remove last \n from text file

# create list of just the messages sent to the tracker
msgs = []
for line in file_txt:
    if line[:3] == 'MSG':
        if 'trial' in line:
            line = line.split()
            line = line[2:]
            line2 = ' '.join(e for e in line)
            msgs.append(line2)
        elif 'RECORD' in line:
            msgs.append('split')
            msgs.append('RECORD')

# separate the messages according to each recording session
def split(sequence, sep):
    chunk = []
    for val in sequence:
        if val == sep:
            yield chunk
            chunk = []
        else:
            chunk.append(val)
    yield chunk

msgs2 = split(msgs, 'split')
msgs_per_rec = []
for value in msgs2:
    if len(value) > 0:
        msgs_per_rec.append(value)

# make into dict with the key being the trial index
recording_dict = []
n = 1
for recording in msgs_per_rec:
    if len(recording) == 1:
        m = 0
    else:
        m = recording[1:]
    trial_dict = {'rec_id': n, 'msgs': m}
    recording_dict.append(trial_dict)
    n += 1

# combine FixationSequences and trial messages
for n in range(len(file_asc)):
    recording_dict[n]['fixations'] = file_asc[n]['fixations']

# combine with sentences/target info
with open(f'{sbj_data_path}/sbj_{id}.json', 'r') as file:
    participant_data = json.load(file)

# get just initial information
stimuli_data = participant_data[0]

boundarytrial_data = []
for block in stimuli_data: # for each of the 4 blocks
    for entry in block: # for each trial in a block
        if entry['type'] == 'trial': # if it's a boundary trial
            boundarytrial_data.append(entry)

# get just boundary trials from recording_dict
boundary_recordings = []
for trial in recording_dict:
    trial_msgs = trial['msgs']
    if trial_msgs == 0:
        continue
    if len(trial_msgs) == 1:
        if 'trial' in trial_msgs:
            boundary_recordings.append(trial)
            break
    else:
        for message in trial_msgs:
            if 'trial' in message  and 'practice trial' not in trial_msgs:
                boundary_recordings.append(trial)
                break

# sanity checks for the number of trials in the stimuli and ET file
if len(boundary_recordings) != len(boundarytrial_data):
    print(f'Check 1: Careful! {len(boundary_recordings) - len(boundarytrial_data)} trials of difference')

if boundary_recordings[-1]['msgs'][-1] != 'trial 128 end':
    end_trialn = int("".join([ele for ele in boundary_recordings[-1]['msgs'][-1] if ele.isdigit()]))
    print(f'Check 1: Careful! Last trial number is {end_trialn} instead of 128')

# check trial numbering
for trial in boundary_recordings:
    print(trial['msgs'])

# renumber trials if one was abandonned
displacement_index = 0
for trial in boundary_recordings:
    current_trialn = int("".join([ele for ele in trial['msgs'][0] if ele.isdigit()]))
    if trial['msgs'][0][-7:] == 'restart':
        trial['msgs'][0] = f'trial {current_trialn + displacement_index} restart'
    else:
        trial['msgs'][0] = f'trial {current_trialn + displacement_index} start'
    if trial['msgs'][-1] == 'trial_abandoned':
        displacement_index =+ 1
    else:
        if trial['msgs'][1][-27:] == 'interrupted for calibration':
            trial['msgs'][-1] = f'trial {current_trialn + displacement_index} interrupted for calibration'
        else:
            trial['msgs'][-1] = f'trial {current_trialn + displacement_index} end'

if len(boundary_recordings) != len(boundarytrial_data):
    print(f'Check 2: Careful! {len(boundary_recordings) - len(boundarytrial_data)} trials of difference')

# if trial was abandonned for calibration, then re-started
# delete aborted trial
for trial in boundary_recordings:
    current_trialn = int("".join([ele for ele in trial['msgs'][0] if ele.isdigit()]))
    if trial['msgs'][-1] == f'trial {current_trialn} interrupted for calibration':
        boundary_recordings.remove(trial)

if len(boundary_recordings) != len(boundarytrial_data):
    print(f'Check 3: Careful! {len(boundary_recordings) - len(boundarytrial_data)} trials of difference')
else:
    print('Check 3: no trial n difference')

# join the boundary trial stimuli info with the recording data
for trial_rec in boundary_recordings:
    trial_data = boundarytrial_data.pop(0)
    for key in trial_data.keys():
        value = trial_data[key]
        trial_rec[key] = value

# remove fixs before fix on fix dot
fix_dot_x = []
fix_dot_fixs = []
n = 1
for trial_rec in boundary_recordings:
    fixs = trial_rec['fixations']
    trial_fix_fixs = []
    for fix in fixs:
        if fix.duration > 200 and fix.x > 300 and fix.x < 370:
            trial_fix_fixs.append(fix)
    if type(trial_fix_fixs) == list: # if more that 1
        if len(trial_fix_fixs) > 0: # if there is data
            interest = trial_fix_fixs[0]
            trial_start = interest.start
            fix_dot_x.append(interest.x)
        else:
            print(f'No fixations for trial {n}.')
            trial_start = 0 # don't discard any fixs
    else: # if just 1 fixation
        trial_start = trial_fix_fixs.start
        fix_dot_x.append(trial_fix_fixs.x)
    for fix in fixs: # discard any fixs before fix dot
            if fix.start < trial_start:
                fix.discard()
    n += 1

# x of fixation dot = x of start of sentence
sentence_start_x = round(statistics.mean(fix_dot_x))

# add sentence len in px
for trial_rec in boundary_recordings:
    target = trial_rec['target']
    pre_target = trial_rec['pre_target']
    post_target = trial_rec['post_target']
    textblock_sentence = pre_target + '[' + target + ']' + '{target}' + post_target

    len_sentence = len(pre_target) + len(target) + len(post_target) # len in characters
    len_sentence_px = len_sentence * px_per_character
    sentence_end_x = sentence_start_x + len_sentence_px

    sentence_start_y = screen_height/2
    
    # create TextBlock objects
    txt = eyekit.TextBlock(textblock_sentence, position = (sentence_start_x,sentence_start_y), font_face='Courier New', font_size = font_height)
    trial_rec['TextBlock'] = txt

### FONT_SIZE MAKES INPUT INTO INT ###
### POSITION IS (SENTENCE_START_X, SENTENCE_START_Y) ###
### FONT SIZE IS IN PX ###
### IA LENGTH INCLUDES 0.5 CHARACTERS OF PADDING ON EITHER SIDE ###

# visualisation of trials with eye tracking trace
vis_folder_path = f'{sbj_data_path}/vis' 
if not os.path.exists(vis_folder_path):
    os.makedirs(vis_folder_path)

for trial_rec in boundary_recordings:
    trial_id = int("".join([ele for ele in trial_rec['msgs'][0] if ele.isdigit()]))
    
    img = eyekit.vis.Image(1920, 1080)
    img.draw_text_block(trial_rec['TextBlock'])
    img.draw_fixation_sequence(trial_rec['fixations'])
    img.draw_rectangle(trial_rec['TextBlock']['target'], fill_color='slateblue',opacity=0.5)
    img.save(f'{sbj_data_path}/vis/trial_{trial_id}.png', crop_margin=30)

### STOP HERE, CHECK DATA VISUALISATIONS ###

# trial visualisations with trace snapped to lines
for trial_rec in boundary_recordings:
    trial_id = int("".join([ele for ele in trial_rec['msgs'][0] if ele.isdigit()]))
    trial_rec['adjusted_seq'] = trial_rec['fixations'].copy()
    if not 'trial_abandoned' in trial_rec['msgs']: # don't adjust if trial was abandoned
        trial_rec['adjusted_seq'].snap_to_lines(trial_rec['TextBlock'])

    img = eyekit.vis.Image(1920, 1080)
    img.draw_text_block(trial_rec['TextBlock'])
    img.draw_fixation_sequence(trial_rec['adjusted_seq'])
    img.draw_rectangle(trial_rec['TextBlock']['target'], fill_color='slateblue',opacity=0.5)
    img.save(f'{sbj_data_path}/vis/trial_{trial_id}_snapped.png', crop_margin=30)


# add gaze metrics
GDs = []
FFDs = []
GPDs = []
FoMs = []
for trial_rec in boundary_recordings:
    trial_id = trial_rec['trial_id'] = int("".join([ele for ele in trial_rec['msgs'][0] if ele.isdigit()]))

    GD = eyekit.measure.gaze_duration(trial_rec['TextBlock']['target'], trial_rec['adjusted_seq'])
    trial_rec['GD'] = GD
    GDs.append(GD)

    FFD = eyekit.measure.initial_fixation_duration(trial_rec['TextBlock']['target'], trial_rec['adjusted_seq'])
    trial_rec['FFD'] = FFD
    FFDs.append(FFD)
    
    GPD = eyekit.measure.go_past_duration(trial_rec['TextBlock']['target'], trial_rec['adjusted_seq'])
    trial_rec['GPD'] = GPD
    GPDs.append(GPD)

    if FFD != GD:
        FoM = FFD
        trial_rec['FoM'] = FoM
    else:
        FoM = 'n/a'
    trial_rec['FoM'] = FoM
    FoMs.append(FoM)

    print(f'Trial {trial_id}: GD = {GD}; FFD = {FFD}; GPD = {GPD}; FoM = {FoM}')

FoMs_noNAs = [x for x in FoMs if type(x) == int]

#print(sorted(GDs))
#print(sorted(FFDs))
print(len(GDs))
print(len(FFDs))
print(len(GPDs))
print(len(FoMs))
print(len(FoMs_noNAs))

diffs = []
for x in range(len(GDs)):
    GD = GDs[x]
    FFD = FFDs[x]
    diffs.append(GD-FFD)

seaborn.set_theme()

# matplotlib histogram
plt_1 = plt.subplot(2, 2, 1)
plt_1.hist(GDs, color = 'peru', bins = range(0,1700,50),alpha = 0.5, edgecolor='peru', linewidth=1.2)
plt_1.set_title('GDs')
plt_2 = plt.subplot(2, 2, 2)
plt_2.hist(FFDs, color = 'orchid', bins = range(0,1700,50), alpha = 0.5, edgecolor='orchid', linewidth=1.2)
plt_2.set_title('FFDs')
plt_3 = plt.subplot(2, 2, 3)
plt_3.hist(GPDs, color = 'firebrick', bins = range(0,1700,50), alpha = 0.5, edgecolor='firebrick', linewidth=1.2)
plt_3.set_title('GPDs')
plt_4 = plt.subplot(2, 2, 4)
plt_4.hist(FoMs_noNAs, color = 'deepskyblue', bins = range(0,1700,50), alpha = 0.5, edgecolor='deepskyblue', linewidth=1.2)
plt_4.set_title('FoMs')
plt.tight_layout()
#plt.show()
plt.savefig(f"{sbj_data_path}/sbj_{id}_histogram.png")

# seaborn density plots
fig, axes = plt.subplots(2,2,figsize=(10,8))
axes = axes.flatten()
seaborn.kdeplot(FFDs,ax=axes[0],shade=True,color='orchid')
axes[0].set_title('FFDs')
seaborn.kdeplot(GDs,ax=axes[1],shade=True,color='peru')
axes[1].set_title('GDs')
seaborn.kdeplot(GPDs,ax=axes[2],shade=True,color='firebrick')
axes[2].set_title('GPDs')
seaborn.kdeplot(FoMs_noNAs,ax=axes[3],shade=True,color='deepskyblue')
axes[3].set_title('FoMs')
axes[0].set_xlim(0,2000)
#axes[0].set_xticks(range(1,32))
axes[1].set_xlim(0,2000)
#axes[1].set_xticks(range(1,32))
axes[2].set_xlim(0,2000)
#axes[2].set_xticks(range(1,32))
axes[3].set_xlim(0,2000)
#axes[3].set_xticks(range(1,32))
plt.tight_layout()
#plt.show()
plt.savefig(f"{sbj_data_path}/sbj_{id}_density.png")


# get boundary trial type (identical, cognate, etc.)
stimuli_key = pd.read_excel(f'{stimuli_path}/stim_key.xlsx')

for trial_rec in boundary_recordings:
    target = trial_rec['target']
    print(target)
    preview = trial_rec['preview']

    temp = stimuli_key.loc[stimuli_key['target'] == target]
    temp2 = temp.loc[stimuli_key['preview'] == preview]
    trial_type = temp2['trial_type'].iloc[0]
    trial_rec['trial_type'] = trial_type

identicals_GD = []
cognates_GD = []
leg_nons_GD = []
illeg_nons_GD = []
simples_GD = []
complexes_GD = []
identicals_FFD = []
cognates_FFD = []
leg_nons_FFD = []
illeg_nons_FFD = []
simples_FFD = []
complexes_FFD = []
identicals_GPD = []
cognates_GPD = []
leg_nons_GPD = []
illeg_nons_GPD = []
simples_GPD = []
complexes_GPD = []
identicals_FoM = []
cognates_FoM = []
leg_nons_FoM = []
illeg_nons_FoM = []
simples_FoM = []
complexes_FoM = []
for trial_rec in boundary_recordings:
    trial_type = trial_rec['trial_type']
    if trial_type == 'identical':
        identicals_GD.append(trial_rec['GD'])
        identicals_FFD.append(trial_rec['FFD'])
        identicals_GPD.append(trial_rec['GPD'])
        identicals_FoM.append(trial_rec['FoM'])
    
    elif trial_type == 'cognate':
        cognates_GD.append(trial_rec['GD'])
        cognates_FFD.append(trial_rec['FFD'])
        cognates_GPD.append(trial_rec['GPD'])
        cognates_FoM.append(trial_rec['FoM'])
        
    elif trial_type == 'legal_nonword':
        leg_nons_GD.append(trial_rec['GD'])
        leg_nons_FFD.append(trial_rec['FFD'])
        leg_nons_GPD.append(trial_rec['GPD'])
        leg_nons_FoM.append(trial_rec['FoM'])
        
    elif trial_type == 'illegal_nonword':
        illeg_nons_GD.append(trial_rec['GD'])
        illeg_nons_FFD.append(trial_rec['FFD'])
        illeg_nons_GPD.append(trial_rec['GPD'])
        illeg_nons_FoM.append(trial_rec['FoM'])
    
    morph_type = trial_rec['morph_type']
    if morph_type == 'simple':
        simples_GD.append(trial_rec['GD'])
        simples_FFD.append(trial_rec['FFD'])
        simples_GPD.append(trial_rec['GPD'])
        simples_FoM.append(trial_rec['FoM'])
        
    elif morph_type == 'complex':
        complexes_GD.append(trial_rec['GD'])
        complexes_FFD.append(trial_rec['FFD'])
        complexes_GPD.append(trial_rec['GPD'])
        complexes_FoM.append(trial_rec['FoM'])

### exploring metrics ###
# GD
#print(f'Simple morphs GD mean: {statistics.mean(simples_GD)}') # mean = 386ms
#print(f'Complex morphs GD mean: {statistics.mean(complexes_GD)}') # mean = 374ms

#print(f'Identical GD mean: {statistics.mean(identicals_GD)}') # mean = 281ms
#print(f'Cognate GD mean: {statistics.mean(cognates_GD)}') # mean = 413ms
#print(f'Legal nonwords GD mean: {statistics.mean(leg_nons_GD)}') # mean = 371
#print(f'Illegal nonwords GD mean: {statistics.mean(illeg_nons_GD)}') # mean = 455

# FFD
#print(f'Simple morphs FFD mean: {statistics.mean(simples_FFD)}') # mean = 297ms
#print(f'Complex morphs FFD mean: {statistics.mean(complexes_FFD)}') # mean = 294ms

#print(f'Identical FFD mean: {statistics.mean(identicals_FFD)}') # mean = 230ms
#print(f'Cognate FFD mean: {statistics.mean(cognates_FFD)}') # mean = 309ms
#print(f'Legal nonwords FFD mean: {statistics.mean(leg_nons_FFD)}') # mean = 290
#print(f'Illegal nonwords FFD mean: {statistics.mean(illeg_nons_FFD)}') # mean = 354

# GPD
#print(f'Simple morphs GPD mean: {statistics.mean(simples_GPD)}') # mean = 425ms
#print(f'Complex morphs GPD mean: {statistics.mean(complexes_GPD)}') # mean = 418ms

#print(f'Identical GPD mean: {statistics.mean(identicals_GPD)}') # mean = 321ms
#print(f'Cognate GPD mean: {statistics.mean(cognates_GPD)}') # mean = 438ms
#print(f'Legal nonwords GPD mean: {statistics.mean(leg_nons_GPD)}') # mean = 410
#print(f'Illegal nonwords GPD mean: {statistics.mean(illeg_nons_GPD)}') # mean = 517



# get participant responses
participant_responses = participant_data[1:]

# exclude practice check & final completion time stamp
participant_responses = participant_responses[1:-1]

# get all comprehension checks
comp_checks = []
for trial in participant_responses:
    if 'question' in trial.keys():
        comp_checks.append(trial)

for trial in boundary_recordings:
    print(trial)

# check that we have the right number of checks
if len(comp_checks) != 40:
    print(f'Wrong number of comprehension check responses: {len(comp_checks)}/40')

# just get responses to the comp checks
responses = []
for check in comp_checks:
    responses.append(check['correct'])

# get comp check score
comp_score = responses.count('YES')/len(comp_checks) * 100

# input all scores
participant_scores = [f'comprehension_score: {comp_score}',f'lextale_score: {lextale_score}', f'morph_score: {morph_score}']
participant_scores = {'comp_score': comp_score, 'lextale_score': lextale_score, 'morph_score': morph_score}

# export as json
#fl = f'{sbj_data_path}/sbj_{id}_boundaryrecording.json'
#with open(fl, 'w', encoding='utf-8') as file:
#    json.dump(sbj_data_path, file, indent='\t')

# export as csv
keys = boundary_recordings[0].keys()
with open(f'{sbj_data_path}/sbj_{id}_boundaryrecording.csv', 'w', newline='') as output_file:
    dict_writer = csv.DictWriter(output_file, keys)
    dict_writer.writeheader()
    dict_writer.writerows(boundary_recordings)

keys = participant_scores.keys()
with open(f'{sbj_data_path}/sbj_{id}_scores.csv', 'w') as csv_file:  
    writer = csv.writer(csv_file)
    for key, value in participant_scores.items():
       writer.writerow([key, value])

print(f'Participant {id} pre-processing finished')



# combine all participant datafiles into large dataframe
all_ET = pd.DataFrame()
all_scores = pd.DataFrame()

for id in participant_ids:
    sbj_data_path = f'{data_path}/sbj_{id}'

    sbj_ET = pd.read_csv(f'{sbj_data_path}/sbj_{id}_boundaryrecording.csv',encoding = "ISO-8859-1")
    sbj_ET['sbjID'] = id # add sbj_id column
    all_ET = pd.concat([all_ET, sbj_ET]) # add to big DF

    sbj_scores = pd.read_csv(f'{sbj_data_path}/sbj_{id}_scores.csv')
    sbj_scores['sbjID'] = id # add sbj_id column
    all_scores = pd.concat([all_scores, sbj_scores]) # add to big DF
    
# export big dataframe
all_ET.to_csv(f'{data_path}/all_ET.csv')
all_scores.to_csv(f'{data_path}/all_scores.csv')