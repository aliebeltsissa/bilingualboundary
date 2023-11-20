import random
import pandas as pd
from os import chdir

chdir("C:\\Users\\annal\\OneDrive\\Documents\\GitHub\\bilingualboundary\\stimuli")

SCREEN_WIDTH_PX = 1280
SCREEN_WIDTH_MM = 312

char_width_mm = 2
px_per_mm = SCREEN_WIDTH_PX / SCREEN_WIDTH_MM
char_width = int(round(char_width_mm * px_per_mm))

# defining things
stimuli_filename = 'example_stim.csv'

def import_stimuli(stimuli_filename):
    '''
    Import the list of stimuli for the experiment.
    '''
    column_names = ['morph_type','target','cognate','legal_non','illegal_non','sentence1','sentence2']
    # import file as dataframe
    df = pd.read_csv(f"{stimuli_filename}", index_col=0)
    # make dataframe into dictionary
    temp = df.to_dict("split")
    temp = dict(zip(temp["index"], temp["data"]))
    temp_length = len(temp.keys())
    # replace data by column:data dictionary
    stimuli = {}
    for x in range(temp_length):
        values = [i for i in temp.values()]
        value = values[x]
        value2 = {column_names[x]: value[x] for x in range(len(column_names))}
        stimuli[x] = value2
    return stimuli

def pick_sentence_set(stimuli):
    '''
    Choose which of the 4 lists the current participant will receive. 
    This determines which preview-target combination the participant
    will be given for each sentence.
    '''
    choices = []
    options = [1,2,3,4]
    for x in range(len(stimuli.keys())):
        choice = random.choice(options)
        choices.append(choice)
    return choices

def stimuli_exp_extraction(stimuli, choices):
    '''
    This forms the sentences each participant is going to see, based
    on their randomly allocated list for each item.
    '''
    stimuli_exp = []
    stimuli_copy = stimuli.copy()
    sentencen = len(stimuli_copy.keys())
    indices = [x for x in range(sentencen)]
    for x in range(len(choices)):
        list_type = choices[x]
        indice = random.choice(indices)
        indices.remove(indice)
        item = stimuli_copy[indice]
        morph_type = item['morph_type']
        target = item['target']
        cognate = item['cognate']
        legal_non = item['legal_non']
        illegal_non = item['illegal_non']
        sentence1 = item['sentence1']
        sentence2 = item['sentence2']
        if list_type == 1:
            preview1 = target
            preview2 = legal_non
        if list_type == 2:
            preview1 = target
            preview2 = illegal_non
        if list_type == 3:
            preview1 = cognate
            preview2 = legal_non
        if list_type == 4:
            preview1 = cognate
            preview2 = illegal_non
        stimuli_exp.append({'indice':indice,'list_index':list_type,
                        'morph_type':morph_type,'target':target,
                        'preview1':preview1,'preview2':preview2,
                        'sentence1':sentence1,'sentence2':sentence2})
    return stimuli_exp

def boundary(stimuli_exp):
    '''
    Generate info on boundary.
    '''
    for x in range(len(stimuli_exp)): # for each trial
        # get stimuli for trial    
        trial_stimuli = stimuli_exp[x] 
        # extract sentence 1
        sentence1 = trial_stimuli['sentence1'] 
        # find the index of 'X' (to be replaced by the preview/target)
        X1_pos = sentence1.index('X')
        # get the sentence up to the target word
        pre_target1 = sentence1[:X1_pos]
        # get the sentence after the target word
        post_target1 = sentence1[X1_pos+1:]
        # find the index of the last letter in the pre-target word
        boundary1_index = X1_pos - 1
        # get preview
        preview1 = trial_stimuli['preview1']
        # get total sentence length
        full_sentence1 = pre_target1 + preview1 + post_target1
        sentence1_len = len(full_sentence1)
        # get boundary location relative to left of screen
        boundary1_shift = -SCREEN_HEIGHT_PX/2 + 50 + boundary1_index * char_width
        trial_stimuli['boundary1_index'] = boundary1_index
        trial_stimuli['sentence1_len'] = sentence1_len
        trial_stimuli['boundary1_shift'] = boundary1_shift
        trial_stimuli['pre_target1'] = pre_target1
        trial_stimuli['post_target1'] = post_target1
         
        # extract sentence 2
        sentence2 = trial_stimuli['sentence2'] 
        # find the index of 'X' (to be replaced by the preview/target)
        X2_pos = sentence2.index('X')
        # get the sentence up to the target word
        pre_target2 = sentence2[:X2_pos]
        # get the sentence after the target word
        post_target2 = sentence2[X2_pos+1:]
        # find the index of the last letter in the pre-target word
        boundary2_index = X2_pos - 1
        # get preview
        preview2 = trial_stimuli['preview2']
        # get total sentence length
        full_sentence2 = pre_target2 + preview2 + post_target2
        sentence2_len = len(full_sentence2)
        # get boundary location relative to left of screen
        boundary2_shift = -SCREEN_HEIGHT_PX/2 + 50 + boundary2_index * char_width
        trial_stimuli['boundary2_index'] = boundary2_index
        trial_stimuli['sentence2_len'] = sentence2_len
        trial_stimuli['boundary2_shift'] = boundary2_shift
        trial_stimuli['pre_target2'] = pre_target2
        trial_stimuli['post_target2'] = post_target2
        stimuli_exp[x] = trial_stimuli
    return stimuli_exp

def split_separate_trials(stimuli_exp):
    '''
    Splits the 2 sentences for each word into separate trial stimuli.
    '''
    stimuli_split = []
    for x in range(len(stimuli_exp)):
        trial_stimuli = stimuli_exp[x]
        morph_type = trial_stimuli['morph_type']
        list_index = trial_stimuli['list_index']
        indice = trial_stimuli['indice']
        target = trial_stimuli['target']
        preview = trial_stimuli['preview1']
        sentence = trial_stimuli['sentence1']
        sentence_len = trial_stimuli['sentence1_len']
        pre_target = trial_stimuli['pre_target1']
        post_target = trial_stimuli['post_target1']
        boundary_index = trial_stimuli['boundary1_index']
        boundary_shift = trial_stimuli['boundary1_shift']
        stimuli_split.append({'morph_type':morph_type, 'list_index':list_index,
                              'indice':indice,'target':target,'preview':preview,
                              'sentence':sentence,'sentence_len':sentence_len,
                              'pre_target':pre_target,'post_target':post_target,
                              'boundary_index':boundary_index,
                              'boundary_shift':boundary_shift})
        
        preview = trial_stimuli['preview2']
        sentence = trial_stimuli['sentence2']
        sentence_len = trial_stimuli['sentence2_len']
        pre_target = trial_stimuli['pre_target2']
        post_target = trial_stimuli['post_target2']
        boundary_index = trial_stimuli['boundary2_index']
        boundary_shift = trial_stimuli['boundary2_shift']
        stimuli_split.append({'morph_type':morph_type, 'list_index':list_index,
                              'indice':indice,'target':target,'preview':preview,
                              'sentence':sentence,'sentence_len':sentence_len,
                              'pre_target':pre_target,'post_target':post_target,
                              'boundary_index':boundary_index,
                              'boundary_shift':boundary_shift})
    return stimuli_split
        

stimuli = import_stimuli(stimuli_filename)
choices = pick_sentence_set(stimuli)
stimuli_exp = stimuli_exp_extraction(stimuli, choices)
stimuli_exp = boundary(stimuli_exp)
random.shuffle(stimuli_exp)