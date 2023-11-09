'''
This code was written for PsychoPy version 2023.2.3, and is intended to be
used with an EyeLink 1000 eye tracker. To run the script for real, you first
need to obtain PyLink and EyeLinkCoreGraphicsPsychoPy. Alternatively, set
TEST_MODE to True and use the mouse to simulate the gaze position.

The experiment is run with a command like

    python main.py exp1 01

where exp1 is a task ID and 01 is a participant ID. The script expects to
find task config files in the specified DATA_DIR.

To terminate the experiment, press the Q key (for quit) and the experiment
will exit once the current trial has been completed.

During eye tracking trials, you can force calibration by pressing the C key
(for calibrate), which will interrupt the current trial and return to it
after calibration.
'''

# import packages
import collections
collections.Callable = collections.abc.Callable
from psychopy import core, event, monitors, visual
from pathlib import Path
from time import time
import random
import json
import csv

DATA_DIR = Path('./data/experiments/')

# screen metrics
SCREEN_WIDTH_PX = 1920
SCREEN_HEIGHT_PX = 1080
SCREEN_WIDTH_MM = 587
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

if not TEST_MODE:
    import pylink
    from EyeLinkCoreGraphicsPsychoPy import EyeLinkCoreGraphicsPsychoPy


class InterruptTrialAndRecalibrate(Exception):
    pass


class InterruptTrialAndExit(Exception):
    pass

class Experiment:

    def __init__(self, task_id, user_id):

        # Load task data
        self.task_data_path = DATA_DIR / f'{task_id}.json'
        with open(self.task_data_path) as file:
            self.task_data = json.load(file)

        # Calculate screen and font metrics
        px_per_mm = SCREEN_WIDTH_PX / SCREEN_WIDTH_MM
        self.char_width = int(round(self.task_data['char_width_mm'] * px_per_mm))
        self.char_height = self.char_width * FONT_WIDTH_TO_HEIGHT_RATIO
        self.horizontal_margin = (SCREEN_WIDTH_PX - PRESENTATION_WIDTH_PX) // 2
        self.vertical_margin = (SCREEN_HEIGHT_PX - PRESENTATION_HEIGHT_PX) // 2

        # Calculate button positions
        first_button_position = self.horizontal_margin + BUTTON_SIZE_PX // 2
        inter_button_distance = (SCREEN_WIDTH_PX - self.horizontal_margin * 2 - BUTTON_SIZE_PX * self.task_data['n_items']) / (self.task_data['n_items'] - 1)
        button_positions = [
            self.transform_to_center_origin(
                x=int(round(first_button_position + button_i * inter_button_distance + button_i * BUTTON_SIZE_PX)),
                y=SCREEN_HEIGHT_PX - self.vertical_margin - BUTTON_SIZE_PX // 2
            ) for button_i in range(self.task_data['n_items'])
        ]
        self.button_rects = [
            (
                *self.transform_to_top_left_origin(x - BUTTON_SIZE_PX // 2, y + BUTTON_SIZE_PX // 2),
                BUTTON_SIZE_PX,
                BUTTON_SIZE_PX
            ) for x, y in button_positions
        ]

        # Load user_data or create new user
        self.user_data_path = DATA_DIR / task_id / f'{user_id}.json'
        if self.user_data_path.exists():
            with open(self.user_data_path) as file:
                self.user_data = json.load(file)
        else:
            if not self.user_data_path.parent.exists():
                self.user_data_path.parent.mkdir()
            if self.include_predictors:
                random.shuffle(self.task_data['predictor_forms'])
            object_mapping = list(range(self.task_data['n_items']))
            random.shuffle(object_mapping)
            self.user_data = {
                'user_id': user_id,
                'task_id': task_id,
                'creation_time': int(time()),
                'modified_time': None,
                'screen_width_px': SCREEN_WIDTH_PX,
                'screen_height_px': SCREEN_HEIGHT_PX,
                'screen_width_mm': SCREEN_WIDTH_MM,
                'screen_distance_mm': SCREEN_DISTANCE_MM,
                'presentation_area': [
                    self.horizontal_margin,
                    self.vertical_margin,
                    PRESENTATION_WIDTH_PX,
                    PRESENTATION_HEIGHT_PX,
                ],
                'buttons': self.button_rects,
                'font_size': self.char_height,
                'responses': {
                    'sentence_boundary_test': [],
                },
                'sequence_position': 0,
            }
            self.save_user_data()
        
        # 

    def import_stimuli():
        '''
        Import the list of stimuli for the experiment.
        '''

    def pick_sentence_set():
        '''
        Choose which of the 4 lists the current participant will receive. 
        This determines which preview-target combination the participant
        will be given for each sentence.
        '''
        choices = [1,2,3,4]*40
        random.shuffle(choices)
    
    def transform_to_center_origin(self, x, y):
        '''
        Transform xy-coordinates based on a top-left origin into
        xy-coordinates based on a center origin.
        '''
        return int(x - SCREEN_WIDTH_PX // 2), int(SCREEN_HEIGHT_PX // 2 - y)
    
    def get_gaze_position(self):
        '''
        Returns the current gaze position from the eye tracker (with a center
        origin). Before requesting the sample, a short pause is performed so
        as not to flood the eye tracker with requests. If in test mode, this
        returns the mouse position instead.
        '''
        core.wait(TIME_RESOLUTION_SECONDS)
        if TEST_MODE:
            return self.mouse.getPos()
        gaze_sample = self.tracker.getNewestSample()
        if gaze_sample.isRightSample():
            x, y = gaze_sample.getRightEye().getGaze()
        else:
            x, y = gaze_sample.getLeftEye().getGaze()
        return self.transform_to_center_origin(x, y)

    def perform_calibration(self):
        '''
        Run through the eye tracker calibration sequence. In test mode, this
        is skipped.
        '''
        visual.TextStim(self.window,
            color='black',
            text=INSTRUCTION_CALIBRATION[LANGUAGE],
        ).draw()
        self.window.flip()
        if not TEST_MODE:
            self.tracker.doTrackerSetup()
        self.n_trials_until_calibration = self.task_data['calibration_freq']

    def abandon_trial(self):
        '''
        Abandon the current trial. This stops eye tracker recording and writes
        a trial_abandoned message.
        '''
        if TEST_MODE:
            return
        self.tracker.sendMessage('trial_abandoned')
        self.tracker.stopRecording()

    def render_experimenter_screen(self, stims=[]):
        '''
        Render an outline of the screen on the host computer. In test mode,
        this is skipped.
        '''
        if TEST_MODE:
            return
        self.tracker.clearScreen(color=0)
        self.tracker.drawLine(
            (SCREEN_WIDTH_PX // 2, 0),
            (SCREEN_WIDTH_PX // 2, SCREEN_HEIGHT_PX),
            color=1
        )
        self.tracker.drawLine(
            (0, SCREEN_HEIGHT_PX // 2),
            (SCREEN_WIDTH_PX, SCREEN_HEIGHT_PX // 2),
            color=1
        )
        self.tracker.drawBox(
            SCREEN_WIDTH_PX // 2 - FIXATION_TOLERANCE_PX,
            SCREEN_HEIGHT_PX // 2 - FIXATION_TOLERANCE_PX,
            FIXATION_TOLERANCE_PX * 2 - 1,
            FIXATION_TOLERANCE_PX * 2 - 1,
            color=1
        )
        for x, y, width, height in self.button_rects:
            self.tracker.drawBox(
                x,
                y,
                width - 1,
                height - 1,
                color=1
            )
        for color_i, stim in enumerate(stims, 2):
            width = len(stim.text) * self.char_width
            height = self.char_height
            x, y = self.transform_to_top_left_origin(*stim.pos)
            self.tracker.drawBox(
                x - width // 2,
                y - height // 2,
                width - 1,
                height - 1,
                color=color_i
            )
    
    def instructions(self, image=None, message=None):
        '''
        Display an instructional image or message and await a press of the
        space bar to continue.
        '''
        if image:
            visual.ImageStim(self.window,
                image=Path(f'images/instructions/{image}'),
                size=(1000, 600),
            ).draw()
        elif message:
            visual.TextStim(self.window,
                color='black',
                text=message,
            ).draw()
        self.window.flip()
        event.waitKeys(keyList=['space'])
        self.n_trials_until_calibration = 0
    
    def await_boundary_cross(self):
        '''
        Wait for the participant's gaze to cross a boundary. If the C key
        is pressed, the trial will be abandoned in order to recalibrate.
        '''
        while True:
            if event.getKeys(['c']):
                raise InterruptTrialAndRecalibrate
            if self.get_gaze_position()[0] > self.boundary_x:
                return True
            
    def show_fixation_dot(self):
        '''
        Show the fixation dot in the center of the screen.
        '''
        self.fixation_dot.draw()
        self.window.flip()

    def await_mouse_selection(self, buttons):
        '''
        Wait for a mouse click and then check if the click is on one of the
        object buttons; if so, return the selected item.
        '''
        self.mouse.clickReset()
        while True:
            core.wait(TIME_RESOLUTION_SECONDS)
            if self.mouse.getPressed()[0]:
                mouse_position = self.mouse.getPos()
                for button_i, button in enumerate(buttons):
                    if button.contains(mouse_position):
                        return button_i
            
    def await_fixation_on_fixation_dot(self):
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
            elif 'q' in keypresses:
                raise InterruptTrialAndExit
            x, y = self.get_gaze_position()
            distance_from_origin = (x ** 2 + y ** 2) ** 0.5
            if distance_from_origin < FIXATION_TOLERANCE_PX:
                if gaze_timer.getTime() >= self.gaze_time:
                    return True
            else:
                gaze_timer.reset()

    def await_gaze_selection(self, buttons):
        '''
        Wait for the participant to fixate an object for the specified time
        and return the selected item. If the C key is pressed, the trial will
        be abandoned in order to recalibrate.
        '''
        fixated_button_i = None
        gaze_timer = core.Clock()
        while True:
            if event.getKeys(['c']):
                raise InterruptTrialAndRecalibrate
            gaze_position = self.get_gaze_position()
            for button_i, button in enumerate(buttons):
                if button.contains(gaze_position):
                    if button_i == fixated_button_i:
                        # still looking at the same button
                        if gaze_timer.getTime() >= self.gaze_time:
                            # and gaze has been on button for sufficient time
                            return button_i
                    else: # gaze has moved to different button, reset timer
                        fixated_button_i = button_i
                        gaze_timer.reset()
                    break
            else: # gaze is not on any button, reset timer
                fixated_button_i = None
                gaze_timer.reset()

    def sentence_boundary_test(self, test_sentence, masked=True):
        '''
        Run a sentence boundary trial. Participant must fixate a dot for 2 
        seconds, after which the sentence appears. Once the participant's
        gaze crosses the boundary, the preview word switches to the target 
        word.
        '''
        if not TEST_MODE:
            self.mouse.setVisible(False)    

    def generate_trial_sequence(task):
        '''
        Generates the main sequence of trials.
        '''
        trial_sequence # []
        # INSTRUCTIONS
        
        # SENTENCE BOUNDARY TEST
        if task['sentence_boundary_reps']:
            trial_sequence.append(('instructions', {
                'image': 'sentence_boundary_instructions.png',
            }))
            for i in range(self.user_data['n_sentences']):
                test_sentence = 

    def execute(self):
        '''
        Execute the experiment: Iterate over the trial sequence and run each
        trial. If the Q key is pressed during a trial, the experiment will be
        terminated at the end of the trial. If a trial completes
        successfully, the sequence position is incremented and the current
        user_data is saved. Once the experiment has been completed the eye
        tracker recording is saved.
        '''
        while self.user_data['sequence_position'] < len(self.user_data['trial_sequence']):
            if event.getKeys(['q']):
                break
            trial_type, params = self.user_data['trial_sequence'][self.user_data['sequence_position']]
            trial_func = getattr(self, trial_type)
            try:
                trial_func(**params)
            except InterruptTrialAndRecalibrate:
                self.abandon_trial()
                self.perform_calibration()
            except InterruptTrialAndExit:
                self.abandon_trial()
                break
            else:
                self.user_data['sequence_position'] += 1
                self.save_user_data()
        visual.TextStim(self.window,
            color='black',
            text=INSTRUCTION_END[LANGUAGE],
        ).draw()
        self.window.flip()
        self.save_tracker_recording(convert_to_asc=True)
        core.quit()

    def save_user_data(self):
        '''
        Write the current state of the user_data dictionary to JSON.
        '''
        self.user_data['modified_time'] = int(time())
        with open(self.user_data_path, 'w') as file:
            json.dump(self.user_data, file, indent='\t')

    def store_response(self, trial_type, response_data):
        '''
        Store response data and save current state of user_data.
        '''
        response_data['time'] = int(time())
        self.user_data['responses'][trial_type].append(response_data)

    def save_screenshot(self, filename):
        '''
        Save a screenshot of the state of the current window (mostly used for
        testing purposes).
        '''
        image = self.window.getMovieFrame()
        image.save(filename)

    def save_tracker_recording(self, convert_to_asc=False):
        '''
        Save the eye tracker recording and close the connection. Ensure that
        the recording does not overwrite a file that already exists.
        '''
        if TEST_MODE:
            return
        self.tracker.setOfflineMode()
        pylink.pumpDelay(100)
        self.tracker.closeDataFile()
        pylink.pumpDelay(500)
        edf_data_path = DATA_DIR / self.user_data['task_id'] / f'{self.user_data["user_id"]}.edf'
        suffix = 1
        while edf_data_path.exists():
            edf_data_path = DATA_DIR / self.user_data['task_id'] / f'{self.user_data["user_id"]}_{suffix}.edf'
            suffix += 1
        self.tracker.receiveDataFile('exp.edf', str(edf_data_path))
        self.tracker.close()
        if convert_to_asc:
            from os import system
            system(f'edf2asc {edf_data_path}')