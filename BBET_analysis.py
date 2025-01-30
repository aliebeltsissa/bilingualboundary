import eyekit
import json
import statistics

px_per_character = 19
font_pt = 23.7525
screen_height = 1080

data_path = 'C:/Users/annal/OneDrive/Documents/GitHub/bilingualboundary/data/sbj_2'

file_asc = eyekit.io.import_asc(file_path=f'{data_path}/2.asc')
# this is now a list of trials
# each trial is a dictionary with one key ('fixations'),
# which has as a value a list of FixationSequences
# each FixationSequence is a list of Fixations

print(len(file_asc))
# 265 recorded trials

# read created txt file
with open(f"{data_path}/2.txt", "r", encoding='utf-8') as file_txt:
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
with open(f'{data_path}/sbj_2.json', 'r') as file:
    stimuli_data = json.load(file)

# get just initial information
stimuli_data = stimuli_data[0]

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

if len(boundary_recordings) != len(boundarytrial_data):
    print(f'Careful! {len(boundary_recordings) - len(boundarytrial_data)} trials of difference')

if boundary_recordings[-1]['msgs'][-1] != 'trial 128 end':
    end_trialn = int("".join([ele for ele in boundary_recordings[-1]['msgs'][-1] if ele.isdigit()]))
    print(f'Careful! Last trial number is {end_trialn} instead of 128')

for trial in boundary_recordings:
    print(trial['msgs'])
# careful: trial 58 abandonned, so went to the next word

# renumber trials if one was abandonned
displacement_index = 0
for trial in boundary_recordings:
    current_trialn = int("".join([ele for ele in trial['msgs'][0] if ele.isdigit()]))
    trial['msgs'][0] = f'trial {current_trialn + displacement_index} start'
    if trial['msgs'][-1] == 'trial_abandoned':
        displacement_index =+ 1
    else:
        trial['msgs'][-1] = f'trial {current_trialn + displacement_index} end'
    
# join the boundary trial stimuli info with the recording data
for trial_rec in boundary_recordings:
    trial_data = boundarytrial_data.pop(0)
    for key in trial_data.keys():
        value = trial_data[key]
        trial_rec[key] = value
 
fix_dot_fixs = []
for trial_rec in boundary_recordings:
    for fix in trial_rec['fixations']:
        if fix.duration > 1000 and fix.x > 300 and fix.x < 400:
            fix_dot_fixs.append(fix.x)

# x of fixation dot = x of start of sentence
sentence_start_x = round(statistics.mean(fix_dot_fixs))

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
    txt = eyekit.TextBlock(textblock_sentence, position = (sentence_start_x,sentence_start_y), font_face='Courier New', font_size = font_pt)
    trial_rec['TextBlock'] = txt

### FONT_SIZE MAKES INPUT INTO INT ###
### POSITION IS (SENTENCE_START_X, SENTENCE_START_Y) ###

print(boundary_recordings[-1]['TextBlock']['target'].text) # get the string represented in this IA
# marathon
print(boundary_recordings[-1]['TextBlock']['target'].width) # get the pixel width of this IA
# 126 so 126/8 characters = 15.75px per character!!!
boundary_recordings[-1]['TextBlock']['target'].width/len(boundary_recordings[-1]['TextBlock']['target'].text)
# 15.75px per character
print(boundary_recordings[-1]['TextBlock']['target'].center) # get the xy coordinates of the center of the IA
# (708, 1680.5)
print(boundary_recordings[-1]['TextBlock']['target'].onset) # get the x coordinate of the IA onset
# 660
print(boundary_recordings[-1]['TextBlock']['target'].location) # get the location of the IA in its parent TextBlock
# (0, 27, 35)
print(boundary_recordings[-1]['TextBlock']['target'].id) # get the ID of the IA
# target

sentence = 'The important thing during [marathon]{target} training is not to run too quickly.'
txt = eyekit.TextBlock(sentence, position=(sentence_start_x, screen_height/2), font_face='Courier New', font_size=23.7525)

# visualisation
img = eyekit.vis.Image(1920, 1080)
img.draw_text_block(txt)
img.draw_fixation_sequence(boundary_recordings[-1]['fixations'])
img.save(f'{data_path}/test_vis.png')
