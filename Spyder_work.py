import random
import pandas as pd
from os import chdir

chdir("C:\\Users\\annal\\OneDrive\\Documents\\GitHub\\bilingualboundary\\stimuli")

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

def pick_sentence_set():
    '''
    Choose which of the 4 lists the current participant will receive. 
    This determines which preview-target combination the participant
    will be given for each sentence.
    '''
    choices = []
    options = [1,2,3,4]
    for x in range(40):
        choice = random.choice(options)
        choices.append(choice)
    return choices
    
stimuli = import_stimuli(stimuli_filename)
choices = pick_sentence_set()