import random
from os import chdir
import pandas as pd

chdir("C:\\Users\\annal\\OneDrive\\Documents\\GitHub\\bilingualboundary\\stimuli")

def import_stimuli(stimuli_filename):
    '''
    Import the list of stimuli for the experiment.
    '''
    column_names = ['morph_type','affix_onset_indice','LD','Italian','English','legal_non','illegal_non','sentence1','sentence2']
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


def extract_targets(stimuli):
    '''
    Extract the target words from the stimuli file.
    '''
    alphabet = ['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o',
                'p','q','r','s','t','u','v','w','x','y','z']
    # exclude characters too strange to work if randomly placed in middle of word
    to_exclude = ['j','k','q','v','w','x','y','z']
    characters = [x for x in alphabet if x not in to_exclude]
    consonants = ['b','c','d','f','g','h','j','k','l','m','n','p','q','r','s','t','v','w','x','y','z']
    
    full_output = []
    nonwords_to_generate = 10
    
    for x in range(len(stimuli)):
        line = stimuli[x]
        morph_type = line['morph_type']
        target = line['English']
        italian = line['Italian']
        LD = line['LD']
        nonwords = []
        
        target_len = len(target)
        target_characters = [*target]
        italian_characters = [*italian]
        characters_toavoid = target_characters + italian_characters
        consonants_touse = [character for character in consonants if character not in characters_toavoid]
        sampled_con = random.sample(consonants_touse, target_len)
        illegal_nonword = ''.join(sampled_con)
        
        while len(nonwords) < nonwords_to_generate:
            if morph_type == 'simple':
                reps = 0
                nonword = ''
                indices = list(range(0,len(target)))
                while reps < LD: # extract as many indices as required by the LD
                    pick = indices.pop(random.randrange(len(indices)))
                    new_letter = random.choice(characters)
                    if len(nonword) < 1:
                        nonword = target
                    nonword = nonword[:pick] + new_letter + nonword[pick + 1:]
                                    
                    reps += 1
                
            elif morph_type == 'complex':
                if LD == 1: # can't function well for LD of 1
                    LD = 2
                    
                boundary = int(line['affix_onset_indice'])
                # this is the indice for the 1st character after the boudnary 
                # between stem & affix
                part1 = target[:boundary]
                part2 = target[boundary:]
                extractn_1 = LD//2 # half of LD, rounded down
                if (LD % 2) != 0: # if the LD is odd
                    extractn_2 = (LD//2)+1 # half of LD, rounded up
                else:
                    extractn_2 = LD//2
                
                choices = [extractn_1,extractn_2]
                choice_1 = choices.pop(random.randrange(len(choices)))
                choice_2 = choices[0]
                
                reps = 0
                nonword_part1 = ''
                indices_1 = list(range(0,len(part1)))
                while reps < choice_1:
                    pick = indices_1.pop(random.randrange(len(indices_1)))
                    new_letter = random.choice(characters)
                    if len(nonword_part1) < 1:
                        nonword_part1 = part1
                    nonword_part1 = nonword_part1[:pick] + new_letter + nonword_part1[pick + 1:]
                                    
                    reps += 1
                    
                reps = 0
                nonword_part2 = ''
                indices_2 = list(range(0,len(part2)))
                while reps < choice_2:
                    pick = indices_2.pop(random.randrange(len(indices_2)))
                    new_letter = random.choice(characters)
                    if len(nonword_part2) < 1:
                        nonword_part2 = part2
                    nonword_part2 = nonword_part2[:pick] + new_letter + nonword_part2[pick + 1:]
                                    
                    reps += 1
                
                nonword = nonword_part1 + nonword_part2
            nonwords.append(nonword)
            
        full_output.append([target,LD,illegal_nonword,nonwords])
        nonwords = pd.DataFrame(full_output)
        nonwords.columns = ['Italian', 'LD','illegal_nonword','options']
        nonwords[['op1','op2','op3','op4','op5','op6','op7','op8','op9','op10']] = pd.DataFrame(nonwords.options.tolist(), index= nonwords.index)
        del nonwords['options']
    return nonwords

stimuli = import_stimuli('experiment_stim_withLDs.csv')
nonwords = extract_targets(stimuli)
nonwords.to_csv('nonword_options.csv', index=True, header=True)
