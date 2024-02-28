'''
This code was written for PsychoPy version 2023.2.3, and is intended to be
used with an EyeLink 1000 eye tracker. It needs PyLink and 
EyeLinkCoreGraphicsPsychoPy. Alternatively, set TEST_MODE to True and use 
the mouse to simulate the gaze position.

To terminate the experiment, press the Q key (for quit) and the experiment
will exit once the current trial has been completed.

During eye tracking trials, you can force calibration by pressing the C key
(for calibrate), which will interrupt the current trial and return to it
after calibration.
'''

# import packages
import collections.abc
collections.Callable = collections.abc.Callable
import os
import random
from pathlib import Path
import pandas as pd
import argparse
from psychopy import event, visual, core
import json

# for own laptop:
#os.chdir("C:\\Users\\annal\\OneDrive\\Documents\GitHub\\bilingualboundary\\stimuli")
DATA_DIR = Path('C:/Users/annal/OneDrive/Documents/GitHub/bilingualboundary')

# for the lab:
# chdir("D:\\ALiebelt\\bilingualboundary\\stimuli")

# screen metrics
SCREEN_WIDTH_PX = 1280
SCREEN_HEIGHT_PX = 720
SCREEN_WIDTH_MM = 312
SCREEN_DISTANCE_MM = 570

# area of the screen that is actually used
PRESENTATION_WIDTH_PX = 960
PRESENTATION_HEIGHT_PX = 540

BUTTON_SIZE_PX = 100 # size of object buttons
FIXATION_TOLERANCE_PX = 18 # permissible distance from the fixation dot
TIME_RESOLUTION_SECONDS = 0.01 # time to wait between gaze position polls
FONT_WIDTH_TO_HEIGHT_RATIO = 1.66666667 # in Courier New, this ratio is 1 : 1 2/3

TEST_MODE = True # if set to True, use mouse to simulate gaze position

INSTRUCTION_CALIBRATION = 'New calibration... Get comfortable...'
INSTRUCTION_END = 'Experiment complete'

# EXPERIMENTAL SETTINGS
task_id = 'boundary_exp'
calibration_freq = 16
char_width_mm = 4
px_per_mm = SCREEN_WIDTH_PX / SCREEN_WIDTH_MM
char_width = int(round(char_width_mm * px_per_mm))
char_height = char_width * FONT_WIDTH_TO_HEIGHT_RATIO
top_of_screen = (0,SCREEN_HEIGHT_PX/2-100)
bottom_of_screen = (0,-SCREEN_HEIGHT_PX/2+100)

# for own laptop:
stimstart_left = 400
stimstart_center = -600

# for lab computer:
# stimstart_left = 200
# stimstart_center = -800

n_completed_trials = 0
n_trials_until_calibration = 0

class InterruptTrialAndRecalibrate(Exception):
    pass

class InterruptTrialAndExit(Exception):
    pass


def import_fillers(stimuli_filename):
    '''
    Import the lists of filler/practice sentences for the experiment.
    '''
    file = open(f"./stimuli/{stimuli_filename}", "r")
    data = file.read()
    data = data.split("\n") # split text file into list
    data = data[:-1] # remove last \n from text file
    file.close()
    return data

def import_stimuli(stimuli_filename):
    '''
    Import the list of stimuli for the experiment.
    '''
    column_names = ['morph_type','target','cognate','legal_non',
                    'illegal_non','sentence1','sentence2']
    # import file as dataframe
    df = pd.read_csv(f'./stimuli/{stimuli_filename}', index_col=0)
    # make dataframe into dictionary
    temp = df.to_dict("split")
    temp = dict(zip(temp["index"], temp["data"]))
    temp_length = len(temp.keys())
    # replace data by column:data dictionary
    stimuli = {}
    for x in range(temp_length):
        values = list(temp.values())
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
    indices = list(range(sentencen))
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
        boundary1_shift = (stimstart_center) + (boundary1_index * char_width)
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
        boundary2_shift = (stimstart_center) + (boundary2_index * char_width)
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
    for trial_stimuli in stimuli_exp:
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

# display setup
win = visual.Window((SCREEN_WIDTH_PX, SCREEN_HEIGHT_PX), fullscr=True,units='pix')

mouse = event.Mouse(visible=True, win=win)
mouse.clickReset()

fixation_dot = visual.Circle(win,
            lineColor='black',
            radius=SCREEN_WIDTH_PX / 256,
            lineWidth=SCREEN_WIDTH_PX / 256,
            pos=(stimstart_center,0)
)

clock = core.Clock()

if not TEST_MODE:
    import pylink
    from EyeLinkCoreGraphicsPsychoPy import EyeLinkCoreGraphicsPsychoPy
    tracker = pylink.EyeLink('100.1.1.1')
    tracker.openDataFile('exp.edf')
    tracker.sendCommand("add_file_preamble_text 'Experiment 1'")
    pylink.openGraphicsEx(EyeLinkCoreGraphicsPsychoPy(tracker, win))
    tracker.setOfflineMode()
    pylink.pumpDelay(100)
    tracker.sendCommand(f'screen_pixel_coords = 0 0 {SCREEN_WIDTH_PX-1} {SCREEN_HEIGHT_PX-1}')
    tracker.sendMessage(f'DISPLAY_COORDS = 0 0 {SCREEN_WIDTH_PX-1} {SCREEN_HEIGHT_PX-1}')
    tracker.sendCommand('sample_rate 1000')
    tracker.sendCommand('recording_parse_type = GAZE')
    tracker.sendCommand('select_parser_configuration 0')
    tracker.sendCommand('calibration_type = HV13')
    proportion_w = PRESENTATION_WIDTH_PX / SCREEN_WIDTH_PX
    proportion_h = PRESENTATION_HEIGHT_PX / SCREEN_HEIGHT_PX
    tracker.sendCommand(f'calibration_area_proportion = {proportion_w} {proportion_h}')
    tracker.sendCommand(f'validation_area_proportion = {proportion_w} {proportion_h}')
    tracker.sendCommand('file_event_filter = LEFT,RIGHT,FIXATION,SACCADE,BLINK,MESSAGE,BUTTON,INPUT')
    tracker.sendCommand('file_sample_data  = LEFT,RIGHT,GAZE,GAZERES,PUPIL,HREF,AREA,STATUS,INPUT')
    tracker.sendCommand('link_event_filter = LEFT,RIGHT,FIXATION,FIXUPDATE,SACCADE,BLINK,BUTTON,INPUT')
    tracker.sendCommand('link_sample_data  = LEFT,RIGHT,GAZE,GAZERES,PUPIL,HREF,AREA,STATUS,INPUT')

def transform_to_center_origin(x, y):
    '''
    Transform xy-coordinates based on a top-left origin into
    xy-coordinates based on a center origin.
    '''
    return int(x - SCREEN_WIDTH_PX // 2), int(SCREEN_HEIGHT_PX // 2 - y)

def transform_to_top_left_origin(x, y):
    '''
    Transform xy-coordinates based on a center origin into xy-coordinates
    based on a top-left origin.
    '''
    return int(x + SCREEN_WIDTH_PX // 2), int(SCREEN_HEIGHT_PX // 2 - y)

def get_gaze_position():
    '''
    Returns the current gaze position from the eye tracker (with a center
    origin). Before requesting the sample, a short pause is performed so
    as not to flood the eye tracker with requests. If in test mode, this
    returns the mouse position instead.
    '''
    core.wait(TIME_RESOLUTION_SECONDS)
    if TEST_MODE:
        return mouse.getPos()
    gaze_sample = tracker.getNewestSample()
    if gaze_sample.isRightSample():
        x, y = gaze_sample.getRightEye().getGaze()
    else:
        x, y = gaze_sample.getLeftEye().getGaze()
    return transform_to_center_origin(x, y)

def perform_calibration(n_trials_until_calibration):
    '''
    Run through the eye tracker calibration sequence. In test mode, this
    is skipped.
    '''
    visual.TextStim(win,
        color='black',
        text=INSTRUCTION_CALIBRATION,
    ).draw()
    win.flip()
    if not TEST_MODE:
        tracker.doTrackerSetup()
    n_trials_until_calibration = calibration_freq


def await_mouse_selection(button):
    '''
    Wait for a mouse click and then check if the click is on one of the
    object buttons; if so, return the selected item.
    '''
    mouse.clickReset()
    while True:
        core.wait(TIME_RESOLUTION_SECONDS)
        if mouse.getPressed()[0]:
            mouse_position = mouse.getPos()
            if button.contains(mouse_position):
                return button.name


def await_fixation_on_fixation_dot():
    '''
    Wait for the participant to fixate the fixation dot for the specified
    time. If the C key is pressed, the trial will be abandoned in order
    to recalibrate.
    '''
    gaze_timer = core.Clock()
    while True:
        keypresses = event.getKeys()
        if 'c' in keypresses:
            raise InterruptTrialAndRecalibrate
        if 'q' in keypresses:
            raise InterruptTrialAndExit
        x, y = get_gaze_position()
        distance_from_origin = ((x+(-stimstart_center)) ** 2 + y ** 2) ** 0.5
        if distance_from_origin < FIXATION_TOLERANCE_PX:
            if gaze_timer.getTime() >= 2:
                return True
        else:
            gaze_timer.reset()


def await_boundary_cross(boundary):
    '''
    Wait for the participant's gaze to cross a boundary. If the C key
    is pressed, the trial will be abandoned in order to recalibrate.
    '''
    while True:
        if event.getKeys(['c']):
            raise InterruptTrialAndRecalibrate
        if event.getKeys(['q']):
            raise InterruptTrialAndExit
        if get_gaze_position()[0] > boundary:
            return True


def save_tracker_recording(convert_to_asc=False):
    '''
    Save the eye tracker recording and close the connection. Ensure that
    the recording does not overwrite a file that already exists.
    '''
    if TEST_MODE:
        return
    tracker.setOfflineMode()
    pylink.pumpDelay(100)
    tracker.closeDataFile()
    pylink.pumpDelay(500)
    edf_data_path = f'/{sbj_ID}.edf' # maybe needs adjustment
    suffix = 1
    while edf_data_path.exists():
        edf_data_path = f'/{sbj_ID}_{suffix}.edf' # maybe needs adjustment
        suffix += 1
    tracker.receiveDataFile('exp.edf', str(edf_data_path))
    tracker.close()
    if convert_to_asc:
        from os import system
        system(f'edf2asc {edf_data_path}')


def abandon_trial():
    '''
    Abandon the current trial. This stops eye tracker recording and writes
    a trial_abandoned message.
    '''
    if TEST_MODE:
        return
    tracker.sendMessage('trial_abandoned')
    tracker.stopRecording()


def execute(user_data):
    '''
    Execute the experiment: Iterate over the trial sequence and run each
    trial. If the Q key is pressed during a trial, the experiment will be
    terminated at the end of the trial. If a trial completes
    successfully, the sequence position is incremented and the current
    user_data is saved. Once the experiment has been completed the eye
    tracker recording is saved.
    '''
    while user_data['sequence_position'] < len(user_data['trial_sequence']):
        if event.getKeys(['q']):
            break
        trial_type, params = user_data['trial_sequence'][user_data['sequence_position']]
        trial_func = getattr(trial_type)
        try:
            trial_func(**params)
        except InterruptTrialAndRecalibrate:
            abandon_trial()
            perform_calibration(n_trials_until_calibration)
        except InterruptTrialAndExit:
            abandon_trial()
            break
        else:
            user_data['sequence_position'] += 1
            save_user_data()
    visual.TextStim(win,
        color='black',
        text='Experiment complete...',
    ).draw()
    win.flip()
    save_tracker_recording(convert_to_asc=True)
    core.quit()


def show_fixation_dot():
    '''
    Display the fixation cross.
    '''
    fixation_dot.draw()
    win.flip()


def gen_stimuli(stimuli):
    '''
    Generate the Psychopy visual components for the stimuli.
    '''
    pre_target = stimuli['pre_target']
    post_target = stimuli['post_target']
    target = stimuli['target']
    preview = stimuli['preview']
    preview_sentence = pre_target + preview + post_target
    target_sentence = pre_target + target + post_target
    prev_stim = visual.TextStim(win,
            color='black',
            font='Courier New',
            alignText='left',
            pos=(stimstart_left,0),
            height=char_height,
            text=preview_sentence,
            wrapWidth=2000
        )
    targ_stim = visual.TextStim(win,
            color='black',
            font='Courier New',
            alignText='left',
            pos=(stimstart_left,0),
            height=char_height,
            text=target_sentence,
            wrapWidth=2000
        )
    return prev_stim, targ_stim


def instructions(image=None, message=None, progression=None):
    '''
    Display an instructional image or message and await a press of the
    space bar to continue.
    '''
    if image:
        visual.ImageStim(win,
            image=Path(f'./images/instructions/{image}')
        ).draw()
    elif message:
        visual.TextStim(win,
            color='black',
            text=message,
            font='Courier New',
            wrapWidth=1000,
            height=char_height,
            pos=(0,0)
        ).draw()
    if progression == 'button':
        next = visual.ImageStim(win,
            image=Path('./images/buttons/next.png',),
            size=BUTTON_SIZE_PX,
            pos=bottom_of_screen,
            name='next'
        )
        next.draw()
    win.flip()

    n_trials_until_calibration = 0
    if progression == 'button':
        selected_button = await_mouse_selection(next)
        if selected_button == 'next':
            win.flip()
    else:
        event.waitKeys(keyList=['space'])
        win.flip()


def textbox_input(prompt=''):
    instruction = visual.TextStim(win,
        text=prompt,
        font='Courier New',
        height=char_height,
        color='black',
        pos=top_of_screen
    )

    progression = visual.TextStim(win,
        text='Press "enter" when done.',
        font='Courier New',
        height=char_height,
        color='black',
        pos=bottom_of_screen
    )

    text_box = visual.TextBox2(win, 
        text='|', # cursor for where you're is typing
        color='black',
        font='Courier New',
        letterHeight=char_height,
        borderColor='black',
    )

    while True:
        keys = event.getKeys()  # Get keyboard events
        if 'escape' in keys:  # Exit the loop if the 'escape' key is pressed
            break
        if len(keys) > 0:  # If any key is pressed
            if keys[0] == 'backspace':  # Handle backspace key
                text_box.text = text_box.text[:-2] + '|'  # Remove the last character
            elif keys[0] == 'return':  # Handle return key (end of input)
                output = text_box.text
                break
            elif keys[0] == 'comma':
                text_box.text = text_box.text[:-1] + ',|'
            elif keys[0] == 'space':
                text_box.text = text_box.text[:-1] + ' |'
            elif keys[0] == 'apostrophe':
                text_box.text = text_box.text[:-1] + "'|"
            elif keys[0] == 'period':
                text_box.text = text_box.text[:-1] + '.|'
            elif len(keys[0]) == 1:
                text_box.text = text_box.text[:-1] + keys[0] + '|' # Add the pressed key to the text box
        
        instruction.draw()
        progression.draw()
        text_box.draw()
        win.flip()
    return output


def practice_trial(trial_stimuli):
    '''
    Practice trial for the boundary experiment.
    '''
    end = (stimstart_center) + len(trial_stimuli) * char_width
    show_fixation_dot()
    await_fixation_on_fixation_dot()
    visual.TextStim(win,
        color='black',
        font='Courier New',
        alignText='left',
        pos=(stimstart_left,0),
        height=char_height,
        text=trial_stimuli,
        wrapWidth=2000
    ).draw()
    win.flip()
    await_boundary_cross(end)
    core.wait(0.5)
    win.flip()
    core.wait(2)


def boundary_trial(trial_stimuli, n_trials_until_calibration, n_completed_trials):
    '''
    Trial where the participant's gaze has to cross a boundary to continue.
    '''
    prev_stim, targ_stim = gen_stimuli(trial_stimuli)
    boundary = trial_stimuli['boundary_shift']
    pre_target = trial_stimuli['pre_target']
    target = trial_stimuli['target']
    post_target = trial_stimuli['post_target']
    sentence = pre_target + target + post_target
    end = (stimstart_center) + len(sentence) * char_width
    # if necessary, perform calibration
    if n_trials_until_calibration == 0:
        perform_calibration(n_trials_until_calibration)
    n_trials_until_calibration -= 1
    if not TEST_MODE:
        tracker.startRecording(1,1,1,1)
        tracker.drawText(
            f'"Trial {n_completed_trials + 1} ({n_trials_until_calibration})"'
        )
        tracker.sendMessage('trial_type boundary_trial')
        tracker.sendMessage(f'target {target}')
    show_fixation_dot()
    await_fixation_on_fixation_dot()
    prev_stim.draw()
    win.flip()
    targ_stim.draw()
    await_boundary_cross(boundary)
    win.flip()
    await_boundary_cross(end)
    core.wait(0.5)
    win.flip()
    core.wait(2)
    n_completed_trials += 1


def save_user_data(sbj_ID, user_data_path, user_data):
    '''
    Write the current state of the user_data dictionary to JSON.
    '''
    #modified_time = int(time())
    path = f'./data/sbj_{sbj_ID}'
    with open(f'{path}/{sbj_ID}.json', 'w') as file:
        json.dump(user_data, file, indent='\t')
    file.close()


user_data = []
practice_stim = import_fillers('practice_stim.txt')
filler_stim = import_fillers('filler_stim.txt')
stimuli = import_stimuli('experiment_stim.csv')
choices = pick_sentence_set(stimuli)
stimuli_exp = stimuli_exp_extraction(stimuli, choices)
stimuli_exp = boundary(stimuli_exp)
stimuli_exp = split_separate_trials(stimuli_exp)
random.shuffle(stimuli_exp)
user_data.append(stimuli_exp)

# get participant ID
parser = argparse.ArgumentParser()
parser.add_argument('user_id', action='store', type=str, help='user ID')
args = parser.parse_args()
sbj_ID = args.user_id

# set up participant output folder
user_path = Path('./data')
user_data_path = user_path / f'sbj_{sbj_ID}'
if not user_data_path.exists():
    user_data_path.mkdir()

# welcome
instructions(image='welcome_instructions.png')

# text input prompt
strangeness_prompt = textbox_input('Did you notice anything strange when reading the sentences?')
user_data.append(f'Noticed strangeness?: {strangeness_prompt}')

# practice trials
for item in practice_stim:
    trial_stimuli = item
    practice_trial(trial_stimuli)

# main experiment
instructions(message="Congratulations! Now onto the main experiment. Press 'Next' when ready",progression='button')
for item in stimuli_exp:
    trial_stimuli = item
    boundary_trial(trial_stimuli, n_trials_until_calibration, n_completed_trials)

# save data
save_user_data(sbj_ID, user_data_path, user_data)


win.close()
