'''
This code was written for PsychoPy version 2020.2.3 and Python 3.6.6, and is intended to be
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
UPDATE: Now launchable from PsychoPy Coder window with the "Run" button. 
IMPORTANT: Change participant ID & number directly in the script below 
(ln. 38-39).
'''

import collections.abc
collections.Callable = collections.abc.Callable
import psychopy
#psychopy.useVersion('2024.2.3')
from os import system
from time import time
import random
from pathlib import Path
import argparse
import json
import pandas as pd
from psychopy import event, visual, core, monitors
import pylink
from EyeLinkCoreGraphicsPsychoPy import EyeLinkCoreGraphicsPsychoPy


sbj_ID = 4
sbj_lst = '4'

TEST_MODE = False # if set to True, use mouse to simulate gaze position


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

#char_width_mm = 3

# number of pixels to adjust display of things on screen
# no idea why this works, just know that it does
magic_number = 40

# DON'T KNOW WHY: the math is correct
# but for some reason without the magic number the sentences appear too far left
stimstart_left = (SCREEN_WIDTH_PX-PRESENTATION_WIDTH_PX)/2 + magic_number
# edge of presentation screen (from left side of screen)
stimstart_center = -PRESENTATION_WIDTH_PX/2
# edge of presentation screen (from center of screen)

BUTTON_SIZE_PX = 100 # size of object buttons
FIXATION_TOLERANCE_PX = 32 # permissible distance from the fixation dot
TIME_RESOLUTION_SECONDS = 0.002 # time to wait between gaze position polls
FONT_WIDTH_TO_HEIGHT_RATIO = 1.66666667 # in Courier New, this ratio is 1 : 1 2/3

INSTRUCTION_CALIBRATION = 'New calibration... Get comfortable...'
INSTRUCTION_END = 'Experiment complete'

# EXPERIMENTAL SETTINGS
task_id = 'boundary_exp'
px_per_mm = SCREEN_WIDTH_PX / SCREEN_WIDTH_MM # 3.2
char_width = int(round(char_width_mm * px_per_mm)) # 16 px per character
char_height = char_width * FONT_WIDTH_TO_HEIGHT_RATIO # 26.66666672 px
# 20 pt font size
top_of_screen_y = PRESENTATION_HEIGHT_PX/2
top_of_screen = (0,top_of_screen_y)
bottom_of_screen_y = -PRESENTATION_HEIGHT_PX/2
bottom_of_screen = (0,bottom_of_screen_y)
# 1 deg of visual angle = 32 px
# 1.6 characters per deg of visual angle
# max sentence len = 77 characters = 1232 px = 38.5 deg


n_completed_trials = 0

class InterruptTrialAndRecalibrate(Exception):
    '''
    Stop the trial in progress and do a calibration.
    '''
    pass

class InterruptTrialAndExit(Exception):
    '''
    Stop the trial in progress and exit the experiment.
    '''
    pass

class InterruptTrialAndContinue(Exception):
    '''
    Stop the trial in progress and force continue.
    Designed in case participant can't look to end of 
    sentence to progress.
    '''
    pass


def import_txt(stimuli_filename):
    '''
    Import the lists of filler/practice sentences for the experiment.
    '''
    with open(f"./stimuli/{stimuli_filename}", "r", encoding='utf-8') as stimuli_file:
        data = stimuli_file.read()
        data = data.split("\n") # split text file into list
        data = data[:-1] # remove last \n from text file
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
        for y in range(int(n_boundary_perblock*x),int(n_boundary_perblock*(x+1))):
            # for next set of experimental trials
            exp_trial = stimuli_exp[y] # extract each trial
            block.append(exp_trial) # append to current block
        for y in range(int(n_fillers_perblock*x),int(n_fillers_perblock*(x+1))):
            # for next set of filler trials
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
    with open(f"./stimuli/{stimuli_filename}", "r", encoding="utf-8") as file:
        data = file.read()
        data = data.split("\n") # split text file into list
        data = data[:-1] # remove last \n from text file
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
        for y in range(int(n_boundary_perblock*x),int(n_boundary_perblock*(x+1))):
            # for next set of experimental trials
            exp_trial = stimuli_exp[y] # extract each trial
            block.append(exp_trial) # append to current block
        for y in range(int(n_fillers_perblock*x),int(n_fillers_perblock*(x+1))):
            # for next set of filler trials
            filler_trial = filler_stim[y] # extract each trial
            block.append(filler_trial) # append to current block

        random.shuffle(block) # randomly intersperse exp trials & fillers
        blocked_stimuli.append(block) # add current block to blocked_stimuli list
    return blocked_stimuli


# display setup
monitor = monitors.Monitor('monitor', width=SCREEN_WIDTH_PX, distance = SCREEN_DISTANCE_MM)
monitor.setSizePix = ((SCREEN_WIDTH_PX, SCREEN_HEIGHT_PX))
win = visual.Window((SCREEN_WIDTH_PX, SCREEN_HEIGHT_PX),
                    monitor=monitor, fullscr=True, winType='pyglet',
                    units='pix', allowStencil=True, waitBlanking=True, screen = 1)

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
    tracker.sendCommand('calibration_type = HV13') # 13-point calibration
    proportion_w = PRESENTATION_WIDTH_PX / SCREEN_WIDTH_PX
    proportion_h = PRESENTATION_HEIGHT_PX / SCREEN_HEIGHT_PX / 2 # want calib/valid targets closer to the centre line
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
    if gaze_sample:
        if gaze_sample.isRightSample():
            x, y = gaze_sample.getRightEye().getGaze()
        else:
            x, y = gaze_sample.getLeftEye().getGaze()
    return transform_to_center_origin(x, y)


def perform_calibration():
    '''
    Run through the eye tracker calibration sequence. In test mode, this
    is skipped.
    '''
    visual.TextStim(win,
        color='black',
        text=INSTRUCTION_CALIBRATION,
    ).draw()
    win.flip()
    time = clock.getTime(applyZero=True)
    print(f'{time}: calibration')

    if not TEST_MODE:
        tracker.doTrackerSetup()


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
        if event.getKeys(['n']):
            raise InterruptTrialAndContinue
        if event.getKeys(['q']):
            raise InterruptTrialAndExit
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
        if event.getKeys(['c']):
            raise InterruptTrialAndRecalibrate
        if event.getKeys(['n']):
            raise InterruptTrialAndContinue
        if event.getKeys(['q']):
            raise InterruptTrialAndExit
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
        if 'n' in keypresses:
            raise InterruptTrialAndContinue
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
        if event.getKeys(['n']):
            raise InterruptTrialAndContinue

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
        edf_data_path = DATA_DIR / 'data' / f'sbj_{sbj_ID}' / f'{sbj_ID}_{suffix}.edf'
        # maybe needs adjustment
        suffix += 1
    tracker.receiveDataFile('exp.edf', str(edf_data_path))
    tracker.close()
    if convert_to_asc:
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
    win.flip()
    win.flip()


def show_fixation_dot():
    '''
    Display the fixation cross.
    '''
    fixation_dot.draw()
    win.flip()
    time = clock.getTime(applyZero=True)
    print(f'{time}: fixation')


def render_experimenter_screen(boundary=0):
    '''
    Render an outline of the screen on the host computer. In test mode,
    this is skipped.
    '''
    if TEST_MODE:
        return
    tracker.clearScreen(color=0)

    # centre horizontal line
    tracker.drawLine(
        (stimstart_left - magic_number, SCREEN_HEIGHT_PX // 2),
        (SCREEN_WIDTH_PX, SCREEN_HEIGHT_PX // 2),
        color=1
    )

    # vertical line at fixation dot
    tracker.drawLine(
        (stimstart_left - magic_number, SCREEN_HEIGHT_PX),
        (stimstart_left - magic_number, 0),
        color=1
    )

    # why is this correct?
    if boundary > 0:
        tracker.drawLine(
            (boundary + SCREEN_WIDTH_PX // 2, SCREEN_HEIGHT_PX),
            (boundary + SCREEN_WIDTH_PX // 2, -SCREEN_HEIGHT_PX),
            color=2
        )

    # to try:
    #tracker.drawLine(
    #    (stimstart_left - magic_number + boundary, SCREEN_HEIGHT_PX),
    #    (stimstart_left - magic_number + boundary, -SCREEN_HEIGHT_PX),
    #    color=2
    #)

    tracker.drawText(
        f'"Trial {n_completed_trials + 1}"'
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
        next_button = visual.ImageStim(win,
            image=Path('./images/buttons/next.png',),
            size=BUTTON_SIZE_PX,
            pos=(0,bottom_of_screen_y + 200),
            name='next_button'
        )
        next_button.draw()
    win.flip()

    time = clock.getTime(applyZero=True)
    print(f'{time}: instructions')

    if progression == 'button':
        mouse.visible = True
        selected_button = await_gaze_selection([next_button])
        if selected_button == 'next':
            win.flip()
    else:
        event.waitKeys(keyList=['space'])
        win.flip()
    mouse.setVisible(TEST_MODE)
    if not TEST_MODE:
        tracker.stopRecording()


def textbox_input(prompt=''):
    '''
    Create a textbox in which a participant can type.
    '''
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
                text_box.text = text_box.text[:-1] + keys[0] + '|'
                # Add the pressed key to the text box

        instruction.draw()
        progression.draw()
        text_box.draw()
        win.flip()
    return output


def comprehension_check(sentence,expected):
    '''
    Comprehension check for certain sentences.
    '''
    if not TEST_MODE:
        tracker.startRecording(1, 1, 1, 1)

    instructions = visual.TextStim(win,
        text="Thinking about what you've just read, is this true or false? (Look at your answer)",
        color='black',
        height=char_height,
        font='Courier New',
        pos=(0,top_of_screen_y - 200)
    )
    comprehension = visual.TextStim(win,
        text=sentence,
        color='darkblue',
        height=char_height,
        font='Courier New',
        wrapWidth=1000,
    )
    true = visual.ImageStim(win,
        image=Path('./images/buttons/true.png',),
        size=BUTTON_SIZE_PX,
        pos=(-200,bottom_of_screen_y + 300),
        name='true'
    )
    false = visual.ImageStim(win,
        image=Path('./images/buttons/false.png',),
        size=BUTTON_SIZE_PX,
        pos=(200,bottom_of_screen_y + 300),
        name='false'
    )

    instructions.draw()
    comprehension.draw()
    true.draw()
    false.draw()
    win.flip()

    time = clock.getTime(applyZero=True)
    print(f'{time}: comprehension check')

    if not TEST_MODE:
        try:
            selected_button = await_gaze_selection([true,false])
        except InterruptTrialAndContinue:
            abandon_trial()
            return 'aborted', 'ABORTED'
    else:
        mouse.setVisible(True)
        try:
            selected_button = await_mouse_selection([true,false])
        except InterruptTrialAndContinue:
            abandon_trial()
            return 'aborted', 'ABORTED'
    if selected_button == 'true' or selected_button == 'false':
        win.flip()
    if expected == 'T' and selected_button == 'true':
        correct = 'YES' # correct response (hit)
    elif expected == 'T' and selected_button == 'false':
        correct = 'NO' # incorrect response (miss)
    elif expected == 'F' and selected_button == 'true':
        correct = 'NO' # incorrect response (false alarm)
    elif expected == 'F' and selected_button == 'false':
            correct = 'YES' # correct response (correct rejection)
    mouse.setVisible(TEST_MODE)
    if not TEST_MODE:
        tracker.stopRecording()
    return selected_button, correct


def practice_trial(trial_stimuli):
    '''
    Practice trial for the boundary experiment.
    '''
    end = (stimstart_center) + (len(trial_stimuli) * char_width)

    if trial_stimuli == 'Fifteen men and women were at the market that day.':
        attention_sentence = trial_stimuli
    else:
        attention_sentence = '0' # else don't do an attention check

    show_fixation_dot()

    if not TEST_MODE:
        tracker.startRecording(1, 1, 1, 1)
        render_experimenter_screen()
        tracker.startRecording(1,1,1,1)
        tracker.sendMessage(f'practice trial')

    try:
        await_fixation_on_fixation_dot()
    except InterruptTrialAndRecalibrate:
        if not TEST_MODE:
            tracker.stopRecording()
        win.flip()
        win.flip()
        perform_calibration()

        show_fixation_dot()
        if not TEST_MODE:
                tracker.startRecording(1, 1, 1, 1)
    except InterruptTrialAndContinue:
        abandon_trial()
        return attention_sentence
    except InterruptTrialAndExit:
        abandon_trial()
        save_output()
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
    time = clock.getTime(applyZero=True)
    print(f'{time}: practice text')

    try:
        await_boundary_cross(end)
    except InterruptTrialAndContinue:
        abandon_trial()
        return attention_sentence
    core.wait(0.25)

    win.flip()

    if not TEST_MODE:
        tracker.stopRecording()
    core.wait(2)
    
    return attention_sentence


def boundary_trial(trial_stimuli, n_completed_trials):
    '''
    Trial where the participant's gaze has to cross a boundary to continue.
    '''
    prev_stim, targ_stim = gen_stimuli(trial_stimuli)
    # -4 added since the boundary was otherwise
    # in the space instead of immediately at the end of the previous word
    boundary = trial_stimuli['boundary_shift'] -4
    pre_target = trial_stimuli['pre_target']
    target = trial_stimuli['target']
    target_len = (len(target) +1) * char_width
    after_target = boundary + target_len
    post_target = trial_stimuli['post_target']
    sentence_len = trial_stimuli['sentence_len']
    sentence = pre_target + target + post_target
    # -6 added since the boundary was otherwise
    # in the space instead of immediately at the end of the final word
    end = (stimstart_center) + (sentence_len * char_width)

    show_fixation_dot()
    prev_stim.draw()

    if not TEST_MODE:
        render_experimenter_screen(boundary)
        tracker.startRecording(1,1,1,1)
        tracker.sendMessage(f'trial {n_completed_trials+1} start')
        tracker.sendMessage(f'target {target}')

    try:
        await_fixation_on_fixation_dot()
    except InterruptTrialAndRecalibrate:
        if not TEST_MODE:
            tracker.sendMessage(f'trial {n_completed_trials+1} interrupted for calibration')
            tracker.stopRecording()
        win.flip()
        win.flip()
        perform_calibration()

        show_fixation_dot()
        prev_stim.draw()
        if not TEST_MODE:
            render_experimenter_screen(boundary)
            tracker.startRecording(1,1,1,1)
            tracker.sendMessage(f'trial {n_completed_trials+1} restart')
            tracker.sendMessage(f'target {target}')
        try:
            await_fixation_on_fixation_dot()
        except InterruptTrialAndContinue:
            if not TEST_MODE:
                tracker.sendMessage(f'trial {n_completed_trials+1} fix aborted')
    except InterruptTrialAndContinue:
        win.flip()
    except InterruptTrialAndExit:
        abandon_trial()
        save_output()
        core.quit()

    win.flip()
    time = clock.getTime(applyZero=True)
    print(f'{time}: boundary trial')

    targ_stim.draw()
    await_boundary_cross(boundary)
    win.flip()
    print('on_target')
    if not TEST_MODE:
        tracker.sendMessage('on_target')
        
    await_boundary_cross(after_target)
    if not TEST_MODE:
        tracker.sendMessage('off_target')
    print('off_target')
    
    try:
        await_boundary_cross(end)
    except InterruptTrialAndContinue:
        abandon_trial()
        return n_completed_trials
    core.wait(0.25)
    print('end')
    win.flip()

    if not TEST_MODE:
        tracker.sendMessage(f'trial {n_completed_trials+1} end')
        tracker.stopRecording()
    core.wait(2)

    n_completed_trials += 1
    return n_completed_trials


def save_output():
    user_data.append(int(time()))

    fl = user_data_path/f'sbj_{sbj_ID}.json'
    with open(fl, 'w', encoding='utf-8') as file:
        json.dump(user_data, file, indent='\t')

    save_tracker_recording()


user_data = []
practice_stim = import_txt('practice_stim.txt')
practice_stim = [{'sentence':sentence} for sentence in practice_stim]

# get participant ID
#parser = argparse.ArgumentParser()
#parser.add_argument('user_id', action='store', type=str, help='user ID')
#parser.add_argument('lst_number', action='store', type=str, help='stimuli list number')
#args = parser.parse_args()
#sbj_ID = args.user_id
#sbj_lst = args.lst_number

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
instructions(image='welcome_instructions.png')

# calibration
perform_calibration()

# practice trials
attention_sentence = '0'
for item in practice_stim:
    trial_stimuli = item
    try:
        attention_sentence = practice_trial(trial_stimuli['sentence'])
    except InterruptTrialAndRecalibrate:
        abandon_trial()
        perform_calibration()
    except InterruptTrialAndExit:
        abandon_trial()
        save_output()
        core.quit()
    except InterruptTrialAndContinue:
        abandon_trial()
    if attention_sentence != '0':
        response,correct = comprehension_check('There were men at the market.','T')
        user_data.append({attention_sentence: 'There were men at the market',
           'response':response,'correct':correct})


# main experiment
instructions(
    message="Congratulations! Now onto the main experiment. You'll now be given 4 blocks of sentences to read. Look at 'Next' when ready to start block 1.",progression='button'
)

for x in range(4): # 4 blocks
    block_stim = blocked_stimuli[x] # extract stimuli for one block
    if x>0:
        perform_calibration()
        # calibrate before the start of each block,
        # except the first (when there's anyway a calibration)

    n_trial_in_block = 0
    for trial in block_stim: # for each trial in that block
        print(f'Trial {n_trial_in_block+1}/42')
        if trial['type'] == 'trial': # if experimental trial, do boundary trial
            try:
                n_completed_trials = boundary_trial(trial, n_completed_trials)
            except InterruptTrialAndRecalibrate:
                abandon_trial()
                n_completed_trials += 1
                perform_calibration()
            except InterruptTrialAndExit:
                abandon_trial()
                save_output()
                core.quit()
            except InterruptTrialAndContinue:
                abandon_trial()
            # get x position of boundary, from left of screen, in pixels:
            boundary_x=(SCREEN_WIDTH_PX-PRESENTATION_WIDTH_PX)/2+(trial['boundary_index']-4)*char_width
            # info to be recorded for each trial:
            user_data.append({'target':trial['target'],'boundary_x':boundary_x})


        elif trial['type'] == 'filler': # if filler trial, do filler trial, then comprehension check
            try:
                # filler trial:
                sentence = practice_trial(trial['filler'])
                # comprehension check:
                response,correct = comprehension_check(trial['question'],trial['answer'])
                # add response to data:
                user_data.append(
                    {'question': trial['question'], 'response': response, 'correct':correct}
                )
            except InterruptTrialAndRecalibrate:
                abandon_trial()
                perform_calibration()
            except InterruptTrialAndExit:
                abandon_trial()
                save_output()
                core.quit()
            except InterruptTrialAndContinue:
                abandon_trial()
        n_trial_in_block += 1

    # after each block, display inter-block break:
    if x+1 < 4:
        instructions(
            message=f"You've finished reading block {x+1} out of 4! PLease take a short break, and press the spacebar when you're ready to continue."
        )


# debrief
instructions(
    message="You've finished the experiment! Thank you very much."
)

save_output()

core.quit()
