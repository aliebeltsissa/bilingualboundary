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

stimuli = import_stimuli(stimuli_filename)
choices = pick_sentence_set(stimuli)
stimuli_exp = stimuli_exp_extraction(stimuli, choices)