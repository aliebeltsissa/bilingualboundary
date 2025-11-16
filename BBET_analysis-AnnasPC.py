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

data_path = 'C:/Users/annal/OneDrive/Documents/GitHub/bilingualboundary/data'
stimuli_path = 'C:/Users/annal/OneDrive/Documents/GitHub/bilingualboundary/stimuli'


def split(sequence, sep):
        chunk = []
        for val in sequence:
            if val == sep:
                yield chunk
                chunk = []
            else:
                chunk.append(val)
        yield chunk

def participant_preprocessing(id, lextale_score, morph_score, changes, excluded_fixs=None, gender=None, age=None):
    '''
    Runs the pre-processing for each participant ASC datafile.
    '''
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
        print(f'Initial check: Careful! {len(boundary_recordings) - len(boundarytrial_data)} trials of difference')

    if boundary_recordings[-1]['msgs'][-1] != 'trial 128 end' and boundary_recordings[-1]['msgs'][-1] != 'trial_abandoned':
        end_trialn = int("".join([ele for ele in boundary_recordings[-1]['msgs'][-1] if ele.isdigit()]))
        print(f'Initial check: Careful! Last trial number is {end_trialn} instead of 128')

    # check trial numbering
    #for trial in boundary_recordings:
    #    print(trial['msgs'])

    # renumber trials if one was abandonned
    displacement_index = 0
    deleted_trials = 0
    for trial in boundary_recordings:
        current_trialn = int("".join([ele for ele in trial['msgs'][0] if ele.isdigit()]))
        if trial['msgs'][0][-7:] == 'restart':
            trial['msgs'][0] = f'trial {current_trialn + displacement_index} restart'
        else:
            trial['msgs'][0] = f'trial {current_trialn + displacement_index} start'
        
        if trial['msgs'][0][-7:] == 'restart' and trial['msgs'][-1] == 'trial_abandoned':
            boundary_recordings.remove(trial)
            deleted_trials += 1

        if trial['msgs'][-1] == 'trial_abandoned' and trial['msgs'][0][-7:] != 'restart':
            displacement_index =+ 1
        else:
            if trial['msgs'][1][-27:] == 'interrupted for calibration':
                trial['msgs'][-1] = f'trial {current_trialn + displacement_index} interrupted for calibration'
            else:
                trial['msgs'][-1] = f'trial {current_trialn + displacement_index} end'

    if len(boundary_recordings)+deleted_trials != len(boundarytrial_data):
        print(f'After removing abandonned trials: Careful! {len(boundary_recordings) - len(boundarytrial_data)} trials of difference')

    # if trial was abandonned for calibration, then re-started
    # delete aborted trial
    for trial in boundary_recordings:
        current_trialn = int("".join([ele for ele in trial['msgs'][0] if ele.isdigit()]))
        if trial['msgs'][-1] == f'trial {current_trialn} interrupted for calibration':
            boundary_recordings.remove(trial)

    if len(boundary_recordings)+deleted_trials != len(boundarytrial_data):
        print(f'After removing aborted trials: Careful! {len(boundary_recordings) - len(boundarytrial_data)} trials of difference')
    else:
        print('After checks: no trial n difference')
    if current_trialn != 128:
        print(f'Careful! Final trialn is {current_trialn} instead of 128.')
    if deleted_trials > 0:
        print(f'Deleted trials: {deleted_trials}')

    # check trial numbering
    for trial in boundary_recordings:
        print(trial['msgs'])

    # join the boundary trial stimuli info with the recording data
    for trial_rec in boundary_recordings:
        trial_data = boundarytrial_data.pop(0)
        for key in trial_data.keys():
            value = trial_data[key]
            trial_rec[key] = value

    # remove fixs before fix on fix dot
    fix_dot_x = []
    for trial_rec in boundary_recordings:
        current_trialn = int("".join([ele for ele in trial_rec['msgs'][0] if ele.isdigit()]))
        fixs = trial_rec['fixations']
        trial_fix_fixs = []
        for fix in fixs:
            if fix.duration > 200 and fix.x > 290 and fix.x < 370:
                trial_fix_fixs.append(fix)
        if type(trial_fix_fixs) == list: # if more that 1
            if len(trial_fix_fixs) > 0: # if there is data
                interest = trial_fix_fixs[0]
                trial_start = interest.start
                fix_dot_x.append(interest.x)
            else:
                print(f'No cross fixations for trial {current_trialn}.')
                trial_start = 0 # don't discard any fixs
        else: # if just 1 fixation
            trial_start = trial_fix_fixs.start
            fix_dot_x.append(trial_fix_fixs.x)
        for fix in fixs: # discard any fixs before fix dot
                if fix.start < trial_start:
                    fix.discard()
                    fixs.purge()

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

    vis_sbjID = str(id)
    if len(vis_sbjID) == 1:
        vis_sbjID = '0'+str(id)

    for trial_rec in boundary_recordings:
        trial_id = "".join([ele for ele in trial_rec['msgs'][0] if ele.isdigit()])
        # make all trial IDs of same length for correct sorting
        if len(trial_id) == 1:
            trial_id = '00'+trial_id
        if len(trial_id) == 2:
            trial_id = '0'+trial_id
        
        # colour fixations if on target
        color = lambda fxn: 'slateblue' if fxn in trial_rec['TextBlock']['target'] else 'black'

        img = eyekit.vis.Image(1920, 1080)
        img.draw_text_block(trial_rec['TextBlock'])
        img.draw_fixation_sequence(trial_rec['fixations'],color=color,number_fixations=True)
        img.draw_rectangle(trial_rec['TextBlock']['target'], fill_color='slateblue',opacity=0.5)
        img.save(f'{sbj_data_path}/vis/sbj{vis_sbjID}_trial{trial_id}.png', crop_margin=50)

    # trial visualisations with trace snapped to lines
    for trial_rec in boundary_recordings:
        trial_id = "".join([ele for ele in trial_rec['msgs'][0] if ele.isdigit()])
        if len(trial_id) == 1:
            trial_id = '00'+trial_id
        if len(trial_id) == 2:
            trial_id = '0'+trial_id
        trial_rec['adjusted_seq'] = trial_rec['fixations'].copy()
        if not 'trial_abandoned' in trial_rec['msgs']: # don't adjust if trial was abandoned
            trial_rec['adjusted_seq'].snap_to_lines(trial_rec['TextBlock'])

        # colour fixations if on target
        color = lambda fxn: 'slateblue' if fxn in trial_rec['TextBlock']['target'] else 'black'

        img = eyekit.vis.Image(1920, 1080)
        img.draw_text_block(trial_rec['TextBlock'])
        img.draw_fixation_sequence(trial_rec['adjusted_seq'],color=color,number_fixations=True)
        img.draw_rectangle(trial_rec['TextBlock']['target'], fill_color='slateblue',opacity=0.5)
        img.save(f'{sbj_data_path}/vis/sbj{vis_sbjID}_trial{trial_id}_snapped.png', crop_margin=30)


    # exclude problematic fixations
    if excluded_fixs != None:
        for trial in excluded_fixs:
            trial_n = trial[0]
            fix_n = trial[1] -1
            
            for trial_rec in boundary_recordings:
                trial_id = int("".join([ele for ele in trial_rec['msgs'][0] if ele.isdigit()]))
                if trial_id == trial_n:
                    count = 0
                    for fixation in trial_rec['adjusted_seq']:
                        if fixation in trial_rec['TextBlock']['target']:
                            if count == fix_n:
                                fixation.discard()
                                trial_rec['adjusted_seq'].purge()
                                print(f'Discarded trial {trial_id}, fixation {count+1}')
                            count += 1

    # exclude problematic trials
    #if excluded_trials != None:
    #    for trial_n in excluded_trials:
    #        for trial_rec in boundary_recordings:
    #            trial_id = int("".join([ele for ele in trial_rec['msgs'][0] if ele.isdigit()]))
    #            if trial_id == trial_n:
    #                boundary_recordings.remove(trial_rec)

    # get participant bad trials log
    badtrials = pd.read_excel("C:/Users/annal/OneDrive/Documents/Me/SISSA/BBET/BBET_analysis/BBET_preprocessing.xlsx")
    sbj_badtrials = badtrials[badtrials['sbj_ID']==id]

    # make into a dict
    sbj_badtrials_dict = []
    for n in range(1,129):
        trial_issue = sbj_badtrials[n].iloc[0]
        sbj_badtrials_dict.append({'trial_id':n,'trial_issue':trial_issue})

    # combine with boundary_recordings
    for trial_rec in boundary_recordings:
        trial_id = int("".join([ele for ele in trial_rec['msgs'][0] if ele.isdigit()]))
        for trial in sbj_badtrials_dict:
            if trial['trial_id'] == trial_id:
                trial_rec['trial_issue'] = trial['trial_issue']

    # create Fixation Sequences without short fixations (<80ms)
    for trial_rec in boundary_recordings:
        trial_rec['noshorts_seq'] = trial_rec['adjusted_seq'].copy()
        for fixation in trial_rec['noshorts_seq']:
            if fixation.duration < 80:
                fixation.discard()
        trial_rec['noshorts_seq'].purge()

    # add gaze metrics
    GDs_all = []
    FFDs_all = []
    GPDs_all = []
    FoMs_all = []

    GDs_noshortfixs = []
    FFDs_noshortfixs = []
    GPDs_noshortfixs = []
    FoMs_noshortfixs = []
    for trial_rec in boundary_recordings:
        # count how many target fixs in this trial
        #count = 0
        #print(trial_rec['TextBlock']['target'].onset)
        #for fixation in trial_rec['fixations']:
        #    print(fixation.x, fixation.y)
        #for fixation in trial_rec['adjusted_seq']:
        #    if fixation in trial_rec['TextBlock']['target']:
        #        print(fixation.x)
        #        count += 1

        if trial_rec['trial_issue'] == 'N': # exclude trials with skip on target or N-1 word
            trial_id = trial_rec['trial_id'] = int("".join([ele for ele in trial_rec['msgs'][0] if ele.isdigit()]))

            # all data
            GD_all = eyekit.measure.gaze_duration(trial_rec['TextBlock']['target'], trial_rec['adjusted_seq'])
            trial_rec['GD_all'] = GD_all
            GDs_all.append(GD_all)

            FFD_all = eyekit.measure.initial_fixation_duration(trial_rec['TextBlock']['target'], trial_rec['adjusted_seq'])
            trial_rec['FFD_all'] = FFD_all
            FFDs_all.append(FFD_all)
            
            GPD_all = eyekit.measure.go_past_duration(trial_rec['TextBlock']['target'], trial_rec['adjusted_seq'])
            trial_rec['GPD_all'] = GPD_all
            GPDs_all.append(GPD_all)

            if FFD_all != GD_all:
                FoM_all = FFD_all
                trial_rec['FoM_all'] = FoM_all
            else:
                FoM_all = 'n/a'
            trial_rec['FoM_all'] = FoM_all
            FoMs_all.append(FoM_all)

            # no short fixations
            GD_noshortfixs = eyekit.measure.gaze_duration(trial_rec['TextBlock']['target'], trial_rec['noshorts_seq'])
            trial_rec['GD_noshortfixs'] = GD_noshortfixs
            GDs_noshortfixs.append(GD_noshortfixs)

            FFD_noshortfixs = eyekit.measure.initial_fixation_duration(trial_rec['TextBlock']['target'], trial_rec['noshorts_seq'])
            trial_rec['FFD_noshortfixs'] = FFD_noshortfixs
            FFDs_noshortfixs.append(FFD_noshortfixs)
            
            GPD_noshortfixs = eyekit.measure.go_past_duration(trial_rec['TextBlock']['target'], trial_rec['noshorts_seq'])
            trial_rec['GPD_noshortfixs'] = GPD_noshortfixs
            GPDs_noshortfixs.append(GPD_noshortfixs)

            if FFD_noshortfixs != GD_noshortfixs:
                FoM_noshortfixs = FFD_noshortfixs
                trial_rec['FoM_noshortfixs'] = FoM_noshortfixs
            else:
                FoM_noshortfixs = 'n/a'
            trial_rec['FoM_noshortfixs'] = FoM_noshortfixs
            FoMs_noshortfixs.append(FoM_noshortfixs)

    FoMs_noNAs = [x for x in FoMs_all if type(x) == int]
    
    diffs = []
    for x in range(len(GDs_all)):
        GD = GDs_all[x]
        FFD = FFDs_all[x]
        diffs.append(GD-FFD)

    seaborn.set_theme()

    # matplotlib histogram
    plt_1 = plt.subplot(2, 2, 1)
    plt_1.hist(GDs_all, color = 'peru', bins = range(0,1700,50),alpha = 0.5, edgecolor='peru', linewidth=1.2)
    plt_1.set_title('GDs')
    plt_2 = plt.subplot(2, 2, 2)
    plt_2.hist(FFDs_all, color = 'orchid', bins = range(0,1700,50), alpha = 0.5, edgecolor='orchid', linewidth=1.2)
    plt_2.set_title('FFDs')
    plt_3 = plt.subplot(2, 2, 3)
    plt_3.hist(GPDs_all, color = 'firebrick', bins = range(0,1700,50), alpha = 0.5, edgecolor='firebrick', linewidth=1.2)
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
    seaborn.kdeplot(FFDs_all,ax=axes[0],shade=True,color='orchid')
    axes[0].set_title('FFDs')
    seaborn.kdeplot(GDs_all,ax=axes[1],shade=True,color='peru')
    axes[1].set_title('GDs')
    seaborn.kdeplot(GPDs_all,ax=axes[2],shade=True,color='firebrick')
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
        preview = trial_rec['preview']

        temp = stimuli_key.loc[stimuli_key['target'] == target]
        temp2 = temp.loc[stimuli_key['preview'] == preview]
        trial_type = temp2['trial_type'].iloc[0]
        trial_rec['trial_type'] = trial_type

    
    # calculate number of skips
    n1_skips = 0
    target_skips = 0
    for trial_rec in boundary_recordings:
        if trial_rec['trial_issue'] == "N-1_SKIP":
            n1_skips += 1
        elif trial_rec['trial_issue'] == "T_SKIP":
            target_skips += 1

    # get participant responses
    participant_responses = participant_data[1:]

    # exclude practice check & final completion time stamp
    participant_responses = participant_responses[1:-1]

    # get all comprehension checks
    comp_checks = []
    for trial in participant_responses:
        if 'question' in trial.keys():
            comp_checks.append(trial)

    # check that we have the right number of checks
    if len(comp_checks) != 40:
        print(f'Wrong number of comprehension check responses: {len(comp_checks)}/40')

    # just get responses to the comp checks
    responses = []
    for check in comp_checks:
        responses.append(check['correct'])

    # get comp check score
    comp_score = round(responses.count('YES')/len(comp_checks) * 100,2)
    print(f'Comprehension score: {comp_score}')

    # input all scores
    participant_scores = {'gender': gender,'age': age,'comp_score': comp_score, 'lextale_score': lextale_score, 'morph_score': morph_score, 'changes_seen': changes,'N-1_skips':n1_skips,'target_skips':target_skips}

    # export as csv
    if boundary_recordings[0]['trial_issue'] == "N":
        keys = boundary_recordings[0].keys()
    elif boundary_recordings[1]['trial_issue'] == "N":
        keys = boundary_recordings[1].keys()
    else:
        keys = boundary_recordings[2].keys()
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



participant_ids = [3,4,8,9,10,11,12,13,14,15,16,19,21,22,24,25,26,29,31,32,33,34,35,36,38,39,40,41,42,43,45,46,47,48,49,51,52,53,54,55,58,59,60]

# by-participant pre-processing
participant_preprocessing(id=3,lextale_score=86.25,morph_score=95,changes=15,gender='M',age=22)
participant_preprocessing(id=4,lextale_score=95.5,morph_score=100,changes=7.5,excluded_fixs=[[111,2]],gender='M',age=24)
participant_preprocessing(id=8,lextale_score=80,morph_score=75,changes=5,gender='F',age=25)
participant_preprocessing(id=9,lextale_score=75,morph_score=75,changes=15,gender='M',age=26)
participant_preprocessing(id=10,lextale_score=85,morph_score=95,changes=2,gender='F',age=23)
participant_preprocessing(id=11,lextale_score=92.5,morph_score=90,changes=10)
participant_preprocessing(id=12,lextale_score=92.5,morph_score=95,changes=35,gender='M',age=26)
participant_preprocessing(id=13,lextale_score=86.25,morph_score=90,changes=30,gender='M',age=20)
participant_preprocessing(id=14,lextale_score=87.5,morph_score=85,changes=30,excluded_fixs=[[68,2]],gender='M',age=25)
participant_preprocessing(id=15,lextale_score=77.5,morph_score=95,changes=17.5,gender='F',age=21)
participant_preprocessing(id=16,lextale_score=91.25,morph_score=80,changes=10,gender='F',age=25)
participant_preprocessing(id=19,lextale_score=85,morph_score=95,changes=5,gender='F',age=25)
participant_preprocessing(id=21,lextale_score=82.5,morph_score=85,changes=5,gender='F',age=20)
participant_preprocessing(id=22,lextale_score=82.5,morph_score=95,changes=22.5,gender='F',age=22)
participant_preprocessing(id=24,lextale_score=75,morph_score=85,changes=17,gender='M',age=32)
participant_preprocessing(id=25,lextale_score=78.75,morph_score=95,changes=20)
participant_preprocessing(id=26,lextale_score=80,morph_score=85,changes=20,gender='F',age=19)
participant_preprocessing(id=29,lextale_score=75,morph_score=75,changes=5,gender='M',age=25)
participant_preprocessing(id=31,lextale_score=88.75,morph_score=85,changes=20,gender='F',age=24)
participant_preprocessing(id=32,lextale_score=93.75,morph_score=100,changes=7.5,gender='F',age=22)
participant_preprocessing(id=33,lextale_score=88.75,morph_score=90,changes=12.5,gender='F',age=22)
participant_preprocessing(id=34,lextale_score=80,morph_score=85,changes=25,gender='F',age=20)
participant_preprocessing(id=35,lextale_score=81.25,morph_score=80,changes=17.5,gender='M',age=20)
participant_preprocessing(id=36,lextale_score=86.25,morph_score=90,changes=12.5,gender='F',age=25)
participant_preprocessing(id=38,lextale_score=78.75,morph_score=95,changes=22.5,gender='M',age=20)
participant_preprocessing(id=39,lextale_score=90,morph_score=95,changes=30,gender='F',age=20)
participant_preprocessing(id=40,lextale_score=90,morph_score=95,changes=0,gender='F',age=23)
participant_preprocessing(id=41,lextale_score=80,morph_score=85,changes=3,gender='F',age=23)
participant_preprocessing(id=42,lextale_score=98.75,morph_score=100,changes=30,excluded_fixs=[[9,2]])
participant_preprocessing(id=43,lextale_score=86.25,morph_score=95,changes=3)
participant_preprocessing(id=45,lextale_score=93.75,morph_score=100,changes=40)
participant_preprocessing(id=46,lextale_score=81.25,morph_score=75,changes=30,excluded_fixs=[[124,0]])
participant_preprocessing(id=47,lextale_score=77.5,morph_score=90,changes=33,excluded_fixs=[[91,0],[95,0],[110,0]])
participant_preprocessing(id=48,lextale_score=81.25,morph_score=90,changes=7.5)
participant_preprocessing(id=49,lextale_score=75,morph_score=100,changes=33)
participant_preprocessing(id=51,lextale_score=93.75,morph_score=85,changes=20)
participant_preprocessing(id=52,lextale_score=80,morph_score=95,changes=5)
participant_preprocessing(id=53,lextale_score=93.75,morph_score=85,changes=40)
participant_preprocessing(id=54,lextale_score=80,morph_score=100,changes=6)
participant_preprocessing(id=55,lextale_score=83.75,morph_score=95,changes=17)
participant_preprocessing(id=58,lextale_score=81.25,morph_score=70,changes=3)
participant_preprocessing(id=59,lextale_score=78.75,morph_score=90,changes=5)
participant_preprocessing(id=60,lextale_score=96.25,morph_score=85,changes=7.5)

# combine all participant datafiles into large dataframe
all_ET = pd.DataFrame()
all_scores = pd.DataFrame()

for id in participant_ids:
    sbj_data_path = f'{data_path}/sbj_{id}'

    sbj_ET = pd.read_csv(f'{sbj_data_path}/sbj_{id}_boundaryrecording.csv',encoding = "ISO-8859-1")
    sbj_ET.insert(0,"sbj_ID",id)
    all_ET = pd.concat([all_ET, sbj_ET]) # add to big DF

    sbj_scores = pd.read_csv(f'{sbj_data_path}/sbj_{id}_scores.csv',header=None)
    sbj_scores = sbj_scores.transpose()
    new_header = sbj_scores.iloc[0] #grab the first row for the header
    sbj_scores = sbj_scores[1:] #take the data less the header row
    sbj_scores.columns = new_header #set the header row as the df header
    sbj_scores.insert(0,"sbj_ID",id)

    all_scores = pd.concat([all_scores, sbj_scores]) # add to big DF
    
all_scores = all_scores.dropna(axis=1, how='all')

# export big dataframe
all_ET.to_csv(f'{data_path}/all_ET.csv')
all_scores.to_csv(f'{data_path}/all_scores.csv')