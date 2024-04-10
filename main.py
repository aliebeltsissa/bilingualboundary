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

Must be launched from the command line with the following arguments:
python main.py <sbj_id> <list_number>
For example: python main.py 1 1
'''

# import packages
import collections.abc
collections.Callable = collections.abc.Callable
import os
from time import time
import random
from pathlib import Path
import pandas as pd
import argparse
from psychopy import event, visual, core
import json


SCREEN_DISTANCE_MM = 570

# area of the screen that is actually used
PRESENTATION_WIDTH_PX = 1280
PRESENTATION_HEIGHT_PX = 720

#######
# LAB #
#######
DATA_DIR = Path('D:/ALiebelt/bilingualboundary')
SCREEN_WIDTH_PX = 1920
SCREEN_HEIGHT_PX = 1080
SCREEN_WIDTH_MM = 600

char_width_mm = 5


##########
# LAPTOP #
##########
#DATA_DIR = Path('C:/Users/annal/OneDrive/Documents/GitHub/bilingualboundary')
#SCREEN_WIDTH_PX = 1280
#SCREEN_HEIGHT_PX = 720
#SCREEN_WIDTH_MM = 312

#char_width_mm = 3.5

# DON'T KNOW WHY: the math is correct, but for some reason without the (+40) the sentences appear too far left - (0,0) is 40px off-screen
stimstart_left = (SCREEN_WIDTH_PX-PRESENTATION_WIDTH_PX)/2 + 40 # edge of presentation screen (from left side of screen)
stimstart_center = -PRESENTATION_WIDTH_PX/2 # edge of presentation screen (from center of screen)

BUTTON_SIZE_PX = 100 # size of object buttons
FIXATION_TOLERANCE_PX = 30 # permissible distance from the fixation dot
TIME_RESOLUTION_SECONDS = 0.01 # time to wait between gaze position polls
FONT_WIDTH_TO_HEIGHT_RATIO = 1.66666667 # in Courier New, this ratio is 1 : 1 2/3

TEST_MODE = True # if set to True, use mouse to simulate gaze position

INSTRUCTION_CALIBRATION = 'New calibration... Get comfortable...'
INSTRUCTION_END = 'Experiment complete'

# EXPERIMENTAL SETTINGS
task_id = 'boundary_exp'
calibration_freq = 16 # after how many trials the calibration occurs
px_per_mm = SCREEN_WIDTH_PX / SCREEN_WIDTH_MM
char_width = int(round(char_width_mm * px_per_mm))
char_height = char_width * FONT_WIDTH_TO_HEIGHT_RATIO # font size 17 on laptop, 20 in lab
top_of_screen_y = PRESENTATION_HEIGHT_PX/2
top_of_screen = (0,top_of_screen_y)
bottom_of_screen_y = -PRESENTATION_HEIGHT_PX/2
bottom_of_screen = (0,bottom_of_screen_y)


n_completed_trials = 0
n_trials_until_calibration = 0

class InterruptTrialAndRecalibrate(Exception):
    pass

class InterruptTrialAndExit(Exception):
    pass


def import_txt(stimuli_filename):
    '''
    Import the lists of filler/practice sentences for the experiment.
    '''
    file = open(f"./stimuli/{stimuli_filename}", "r")
    data = file.read()
    data = data.split("\n") # split text file into list
    data = data[:-1] # remove last \n from text file
    file.close()
    return data


def import_stimulilst(stimuli_filename):
    '''
    Import the list of stimuli for the experiment.
    '''
    column_names = ['morph_type','lst_type','target','preview','sentence']
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


def import_fillerlst(stimuli_filename):
    '''
    Import the list of stimuli for the experiment.
    '''
    column_names = ['filler','question','answer']
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


def boundary(stimuli_exp):
    '''
    Generate info on boundary.
    '''
    for x in range(len(stimuli_exp)): # for each trial
        # get stimuli for trial
        trial_stimuli = stimuli_exp[x]
        # extract sentence
        sentence = trial_stimuli['sentence']
        # find the index of 'X' (to be replaced by the preview/target)
        X_pos = sentence.index('X')
        # get the sentence up to the target word
        pre_target = sentence[:X_pos]
        # get the sentence after the target word
        post_target = sentence[X_pos+1:]
        # find the index of the last letter in the pre-target word
        boundary_index = X_pos - 1
        # get preview
        preview = trial_stimuli['preview']
        # get total sentence length
        full_sentence = pre_target + preview + post_target
        sentence_len = len(full_sentence)
        # get boundary location relative to center of screen
        boundary_shift = (stimstart_center) + (boundary_index * char_width)
        trial_stimuli['boundary_index'] = boundary_index
        trial_stimuli['sentence_len'] = sentence_len
        trial_stimuli['boundary_shift'] = boundary_shift
        trial_stimuli['pre_target'] = pre_target
        trial_stimuli['post_target'] = post_target
        stimuli_exp[x] = trial_stimuli
    return stimuli_exp


def blocks(stimuli_exp, filler_stim):
    '''
    Split the stimuli into different experimental blocks, interspersed with fillers.
    '''
    n_blocks = 4 # 4 experimental blocks
    n_boundary_perblock = 128/n_blocks # 32 experimental trials per block
    n_fillers_perblock = 40/n_blocks # 10 filler trials (followed by comprehension check) per block

    for x in stimuli_exp.keys(): # for each experimental trial
        trial = stimuli_exp[x]
        trial['type'] = 'trial' # label this as an experimental trial
        stimuli_exp[x] = trial
    for x in filler_stim.keys(): # for each filler trial
        trial = filler_stim[x]
        trial['type'] = 'filler' # label this as a filler trial
        filler_stim[x] = trial

    blocked_stimuli = []
    for x in range(n_blocks):
        # select next set of exp trials + next set of fillers:
        block = []
        for y in range(int(n_boundary_perblock*x),int(n_boundary_perblock*(x+1))): # for next set of experimental trials
            exp_trial = stimuli_exp[y] # extract each trial
            block.append(exp_trial) # append to current block
        for y in range(int(n_fillers_perblock*x),int(n_fillers_perblock*(x+1))): # for next set of filler trials
            filler_trial = filler_stim[y] # extract each trial
            block.append(filler_trial) # append to current block
        
        random.shuffle(block) # randomly intersperse exp trials & fillers
        blocked_stimuli.append(block) # add current block to blocked_stimuli list
    return blocked_stimuli


def import_comprehension(comprehension_filename):
    '''
    Import the list of stimuli for the experiment.
    '''
    column_names = ['English','sentence','pre_target','comprehension_statement','TRUE/FALSE']
    # import file as dataframe
    df = pd.read_csv(f'./stimuli/{comprehension_filename}', index_col=0)
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

def blocks(stimuli_exp, filler_stim):
    '''
    Split the stimuli into different experimental blocks, interspersed with fillers.
    '''
    n_blocks = 4 # 4 experimental blocks
    n_boundary_perblock = 128/n_blocks # 32 experimental trials per block
    n_fillers_perblock = 40/n_blocks # 10 filler trials (followed by comprehension check) per block

    for x in stimuli_exp.keys(): # for each experimental trial
        trial = stimuli_exp[x]
        trial['type'] = 'trial' # label this as an experimental trial
        stimuli_exp[x] = trial
    for x in filler_stim.keys(): # for each filler trial
        trial = filler_stim[x]
        trial['type'] = 'filler' # label this as a filler trial
        filler_stim[x] = trial

    blocked_stimuli = []
    for x in range(n_blocks):
        # select next set of exp trials + next set of fillers:
        block = []
        for y in range(int(n_boundary_perblock*x),int(n_boundary_perblock*(x+1))): # for next set of experimental trials
            exp_trial = stimuli_exp[y] # extract each trial
            block.append(exp_trial) # append to current block
        for y in range(int(n_fillers_perblock*x),int(n_fillers_perblock*(x+1))): # for next set of filler trials
            filler_trial = filler_stim[y] # extract each trial
            block.append(filler_trial) # append to current block
        
        random.shuffle(block) # randomly intersperse exp trials & fillers
        blocked_stimuli.append(block) # add current block to blocked_stimuli list
    return blocked_stimuli


# display setup
win = visual.Window((SCREEN_WIDTH_PX, SCREEN_HEIGHT_PX), fullscr=True,units='pix')

mouse = event.Mouse(visible=True, win=win)
mouse.clickReset()

fixation_dot = visual.Circle(
            win,
            lineColor='black',
            radius=SCREEN_WIDTH_PX / 256,
            lineWidth=SCREEN_WIDTH_PX / 256,
            pos=(stimstart_center,0),
)

clock = core.Clock()

if not TEST_MODE:
    import pylink
    from EyeLinkCoreGraphicsPsychoPy import EyeLinkCoreGraphicsPsychoPy

if not TEST_MODE:
    # Set up eye tracker connection
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
    tracker.sendCommand('calibration_type = HV9') # 9-point calibration
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
    return n_trials_until_calibration


def await_gaze_selection(buttons):
    '''
    Wait for the participant to fixate an object for the specified time
    and return the selected item. If the C key is pressed, the trial will
    be abandoned in order to recalibrate.
    '''
    fixated_button = None
    gaze_timer = core.Clock()
    while True:
        if event.getKeys(['c']):
            raise InterruptTrialAndRecalibrate
        gaze_position = get_gaze_position()
        for button in buttons:
            if button.contains(gaze_position):
                if button == fixated_button:
                    # still looking at the same button
                    if gaze_timer.getTime() >= 1:
                        # and gaze has been on button for sufficient time
                        return button.name
                else: # gaze has moved to different button, reset timer
                    fixated_button = button
                    gaze_timer.reset()
                break
        else: # gaze is not on any button, reset timer
            fixated_button = None
            gaze_timer.reset()


def await_mouse_selection(buttons):
    '''
    Wait for a mouse click and then check if the click is on one of the
    object buttons; if so, return the selected item.
    '''
    mouse.clickReset()
    while True:
        core.wait(TIME_RESOLUTION_SECONDS)
        if mouse.getPressed()[0]:
            mouse_position = mouse.getPos()
            for button in buttons:
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
            if gaze_timer.getTime() >= 1: # have to stay on fixation dot for 1s before advancing
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
    edf_data_path = DATA_DIR / 'data' / f'sbj_{sbj_ID}' / f'{sbj_ID}.edf' # maybe needs adjustment
    suffix = 1
    while edf_data_path.exists():
        edf_data_path = DATA_DIR / 'data' / f'sbj_{sbj_ID}' / f'{sbj_ID}_{suffix}.edf' # maybe needs adjustment
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


def show_fixation_dot():
    '''
    Display the fixation cross.
    '''
    fixation_dot.draw()
    win.flip()


def render_experimenter_screen():
    '''
    Render an outline of the screen on the host computer. In test mode,
    this is skipped.
    '''
    if TEST_MODE:
        return
    tracker.clearScreen(color=0)
    tracker.drawLine(
        (PRESENTATION_WIDTH_PX // 2 + stimstart_center, 0), # line start
        (PRESENTATION_WIDTH_PX // 2 + stimstart_center, SCREEN_HEIGHT_PX), # line end
        color=1
    )
    tracker.drawLine(
        (0, PRESENTATION_WIDTH_PX // 2),
        (PRESENTATION_WIDTH_PX, PRESENTATION_HEIGHT_PX // 2),
        color=1
    )


def render_experimenter_screen_comprehension():
    '''
    Render an outline of the screen on the host computer. In test mode,
    this is skipped.
    '''
    if TEST_MODE:
        return
    tracker.clearScreen(color=0)
    tracker.drawLine(
        (0, PRESENTATION_HEIGHT_PX // 2),
        (PRESENTATION_WIDTH_PX, PRESENTATION_HEIGHT_PX // 2),
        color=1
    )
    tracker.drawBox( # for 'True' button
        -200,
        -PRESENTATION_HEIGHT_PX/2+100,
        BUTTON_SIZE_PX,
        BUTTON_SIZE_PX,
        color=1
    )
    tracker.drawBox( # for 'False' button
        200,
        -PRESENTATION_HEIGHT_PX/2+100,
        BUTTON_SIZE_PX,
        BUTTON_SIZE_PX,
        color=1
    )


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
    if not TEST_MODE:
        tracker.startRecording(1, 1, 1, 1)
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
        mouse.visible = True
        selected_button = await_gaze_selection([next])
        if selected_button == 'next':
            win.flip()
    else:
        event.waitKeys(keyList=['space'])
        win.flip()
    if not TEST_MODE:
        tracker.stopRecording()
    mouse.setVisible(TEST_MODE)
    return n_trials_until_calibration


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
        text='|', # cursor for where you're typing
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


def comprehension_check(sentence,expected):
    '''
    Comprehension check for certain sentences.
    '''
    render_experimenter_screen_comprehension()
    if not TEST_MODE:
        tracker.startRecording(1, 1, 1, 1)

    instructions = visual.TextStim(win,
        text="Thinking about what you've just read, is this true or false? (Look at your answer)",
        color='black',
        height=char_height,
        font='Courier New',
        pos=top_of_screen
    )
    comprehension = visual.TextStim(win,
        text=sentence,
        color='darkblue',
        height=char_height,
        font='Courier New'
    )
    true = visual.ImageStim(win,
        image=Path('./images/buttons/true.png',),
        size=BUTTON_SIZE_PX,
        pos=(-200,bottom_of_screen_y),
        name='true'
    )
    false = visual.ImageStim(win,
        image=Path('./images/buttons/false.png',),
        size=BUTTON_SIZE_PX,
        pos=(200,bottom_of_screen_y),
        name='false'
    )
    instructions.draw()
    comprehension.draw()
    true.draw()
    false.draw()
    win.flip()
    if not TEST_MODE:
        selected_button = await_gaze_selection([true,false])
    else:
        mouse.setVisible(True)
        selected_button = await_mouse_selection([true,false])
    if selected_button == 'true' or selected_button == 'false':
        win.flip()
    if expected == 'T': # if they were supposed to say TRUE
        if selected_button == 'true': # if the participant said TRUE
            correct = 'YES' # correct response (hit)
        if selected_button == 'false': # if theparticipant said FALSE
            correct = 'NO' # incorrect response (miss)
    if expected == 'F': # if they were supposed to say FALSE
        if selected_button == 'true': # if the participant said TRUE
            correct = 'NO' # incorrect response (false alarm)
        if selected_button == 'false': # if the participant said FALSE
            correct = 'YES' # correct response (correct rejection)
    mouse.setVisible(TEST_MODE)
    if not TEST_MODE:
        tracker.stopRecording()
    return selected_button, correct


def practice_trial(trial_stimuli,n_trials_until_calibration):
    '''
    Practice trial for the boundary experiment.
    '''
    render_experimenter_screen()
    if not TEST_MODE:
        tracker.startRecording(1, 1, 1, 1)
    end = (stimstart_center) + len(trial_stimuli) * char_width

    if n_trials_until_calibration == 0:
        n_trials_until_calibration = perform_calibration(n_trials_until_calibration)
    n_trials_until_calibration -= 1

    show_fixation_dot()
    try:
        await_fixation_on_fixation_dot()
    except InterruptTrialAndRecalibrate:
        n_trials_until_calibration = perform_calibration(0)
    except InterruptTrialAndExit:
        core.quit()
    visual.TextStim(win,
        text=trial_stimuli,
        color='black',
        font='Courier New',
        alignText='left',
        pos=(stimstart_left,0),
        height=char_height,
        wrapWidth=2000
    ).draw()
    win.flip()
    if not TEST_MODE:
        tracker.sendMessage('trigger_timer')
    await_boundary_cross(end)
    core.wait(0.5)
    win.flip()
    if not TEST_MODE:
        tracker.stopRecording()
    core.wait(2)
    if trial_stimuli == 'Fifteen men and women were at the market that day.':
        attention_sentence = trial_stimuli
    else:
        attention_sentence = '0' # else don't do an attention check
    return attention_sentence, n_trials_until_calibration



def boundary_trial(trial_stimuli, n_trials_until_calibration, n_completed_trials):
    '''
    Trial where the participant's gaze has to cross a boundary to continue.
    '''
    render_experimenter_screen()

    prev_stim, targ_stim = gen_stimuli(trial_stimuli)
    boundary = trial_stimuli['boundary_shift']
    pre_target = trial_stimuli['pre_target']
    target = trial_stimuli['target']
    post_target = trial_stimuli['post_target']
    sentence = pre_target + target + post_target
    end = (stimstart_center) + len(sentence) * char_width

    # if necessary, perform calibration
    if n_trials_until_calibration == 0:
        n_trials_until_calibration = perform_calibration(n_trials_until_calibration)
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

    if not TEST_MODE:
        tracker.sendMessage('trigger_timer')
    await_boundary_cross(end)
    core.wait(0.5)
    win.flip()

    if not TEST_MODE:
        tracker.stopRecording()
    core.wait(2)
    n_completed_trials += 1
    return n_completed_trials, n_trials_until_calibration



user_data = []
practice_stim = import_txt('practice_stim.txt')
practice_stim = [{'sentence':sentence} for sentence in practice_stim]

# get participant ID
parser = argparse.ArgumentParser()
parser.add_argument('user_id', action='store', type=str, help='user ID')
parser.add_argument('lst_number', action='store', type=str, help='stimuli list number')
args = parser.parse_args()
sbj_ID = args.user_id
sbj_lst = args.lst_number

if sbj_lst == '1':
    file = 'version1.csv'
elif sbj_lst == '2':
    file = 'version2.csv'
elif sbj_lst == '3':
    file = 'version3.csv'
elif sbj_lst == '4':
    file = 'version4.csv'
else:
    print('Error: wrong participant stimuli list number.')

stimuli = import_stimulilst(file)
stimuli_exp = boundary(stimuli)
random.shuffle(stimuli_exp)
filler_stim = import_fillerlst('filler_sentences.csv')
blocked_stimuli = blocks(stimuli_exp,filler_stim)
user_data.append(blocked_stimuli)


# set up participant output folder
user_path = Path('./data')
user_data_path = user_path / f'sbj_{sbj_ID}'
if not user_data_path.exists():
    user_data_path.mkdir()

# welcome
n_trials_until_calibration = instructions(image='welcome_instructions.png')

# calibration
n_trials_until_calibration = perform_calibration(n_trials_until_calibration=0)

# text input prompt
#strangeness_prompt = textbox_input('Did you notice anything strange when reading the sentences?')
#user_data.append(f'Noticed strangeness?: {strangeness_prompt}')

# practice trials
#for item in practice_stim:
#    trial_stimuli = item
#    attention_sentence, n_trials_until_calibration = practice_trial(trial_stimuli['sentence'],n_trials_until_calibration)
#    if attention_sentence != '0':
#        response,correct = comprehension_check('There were men at the market.','T')
#        user_data.append({attention_sentence: 'There were men at the market','response':response,'correct':correct})


# main experiment
n_trials_until_calibration = instructions(message="Congratulations! Now onto the main experiment. You'll now be given 4 blocks of sentences to read. Look at 'Next' when ready to start block 1.",progression='button')

for x in range(4): # 4 blocks
    block_stim = blocked_stimuli[x] # extract stimuli for one block
    n_trials_until_calibration = perform_calibration(0) # calibrate before the start of each block
    
    for trial in block_stim: # for each trial in that block
        if trial['type'] == 'trial': # if experimental trial, do boundary trial
            try:
                n_completed_trials, n_trials_until_calibration = boundary_trial(trial, n_trials_until_calibration, n_completed_trials)
            except InterruptTrialAndRecalibrate:
                abandon_trial()
                n_trials_until_calibration = perform_calibration(n_trials_until_calibration=0)
            except InterruptTrialAndExit:
                abandon_trial()
                core.quit()
            # get x position of boundary, from left of screen, in pixels:
            boundary_x = (SCREEN_WIDTH_PX-PRESENTATION_WIDTH_PX)/2 + trial['boundary_index']*char_width
            user_data.append({'target':trial['target'],'boundary_x':boundary_x}) # info to be recorded for each trial
            
        elif trial['type'] == 'filler': # if filler trial, do filler trial, then comprehension check
            sentence, n_trials_until_calibration = practice_trial(trial['filler'], n_trials_until_calibration) # filler trial
            response,correct = comprehension_check(trial['question'],trial['answer']) # comprehension check
            user_data.append({'question': trial['question'], 'response': response, 'correct':correct}) # add response to data
    
    # after each block, display inter-block break:
    instructions(message=f"You've finished reading block {x+1} out of 4! PLease take a short break, and press the spacebar when you're ready to continue.")


# save data
user_data.append(int(time()))

fl = user_data_path/f'sbj_{sbj_ID}.json'
with open(fl, 'w') as file:
    json.dump(user_data, file, indent='\t')

save_tracker_recording()


core.quit()
