# This script attempts to reformat audio files using the information stored in the voice note at the beginning of the file.
# A speech-to-text recognizer and fuzzy logic are used to extract the meaningful information from the voice note
# and a limited amplitude filter clips the recordings to length.
# This is part of the digital point count protocol; a collaboration between the Bioacoustic Unit / ABMI, ECCC, Birds Canada,
# and the 3rd Ontario Breeding Bird Atlas (2021 - 2025)

# Import libraries
import os, re, pydub, speech_recognition as sr, wave, csv, pandas as pd
from datetime import datetime
from thefuzz import fuzz
from pydub import AudioSegment
from pydub.utils import make_chunks
from datetime import timedelta

# Choose working directory
my_dir = ''
while True:
    print('')
    print('This Python script attempts to reformat audio files using the information stored in the voice note. \n'
          'A speech-to-text recognizer and fuzzy logic are used to extract the meaningful information from the voice note \n'
          'and a limited amplitude filter clips the recordings to length \n\n'
          'This is part of the digital point count protocol; a collaboration between the Bioacoustic Unit / ABMI, ECCC, Birds Canada, \n'
          'and the 3rd Ontario Breeding Bird Atlas (2021 - 2025) \n\n'
          'Please choose a root directory that holds your audio files: \n')
    my_dir = input()
    if os.path.exists(my_dir):
        print('')
        print('Scanning for files ... this may take a moment ... ')
        print('')
    else:
        print('Sorry, could not find this directory.')
        print('')
        continue
    break

# Need to define location for ffmpeg in order to run the converter
pydub.AudioSegment.converter = '/usr/local/bin/ffmpeg'
ext = ['.WAV','.mp3'] # Modify only Zoom file types (WAV)
pref = ['ABMI','ABMP'] # To deal with Riverforks
standard = ['.wav'] # Standard wav file
# Add more exceptions here and handle in loop below

# Change the timestamps and append the interim LOCATION prefix
for root, dirs, files in os.walk(my_dir):
    for file in files:
        if file.endswith(tuple(ext)) and not file.startswith(tuple(pref)):
            try:
                old = os.path.splitext(file)[0]
                new = str('LOCATION_' + datetime.strftime(datetime.strptime(old, "%y%m%d-%H%M%S"), "%Y%m%d_%H%M%S") + '.wav')
                old_type = str(os.path.join(root, old) + '.wav')
                os.rename(old_type, os.path.join(root, new))
                print('Appended location prefix and fixing timestamps for', old)
            except Exception as e:
                print('Could not read', file, '\n', e)
        elif file.startswith(tuple(pref)) and file.endswith(tuple(ext)):
            pass
        elif file.endswith(tuple(standard)) and not file.startswith(tuple(pref)):
            try:
                old_s = os.path.splitext(file)[0]
                new_s = str(re.sub('^(.*?)_','LOCATION_',old_s) + '.wav')
                old_s_type = str(os.path.join(root, old_s) + '.wav')
                os.rename(old_s_type, os.path.join(root, new_s))
                print('Appended location prefix and fixing timestamps for', old_s)
            except Exception as e1:
                print('Could not read', file, '\n', e1)
        else:
            print('No rename for', file)
            pass

# Create the dictionaries
RF = {}
RF_merge = {}
RF_match = {}

# Scan the directory again with the updated paths
for path, dirs, files in os.walk(my_dir):
    for file in files:
        if file.lower().endswith(".wav"):
            RF.update({(os.path.join(path, file)): ''})
            RF_merge.update({(os.path.join(path, file)): ''})
        elif file.lower().endswith(".mp3"):
            fp = os.path.join(path, os.path.splitext(os.path.basename(os.path.join(path, file)))[0] + '.wav')
            try:
                sound = pydub.AudioSegment.from_mp3(os.path.join(path, file))
                sound.export(fp, format='wav')
                print('Success for ', file)
                RF.update({fp: ''})
                RF_merge.update({fp: ''})
            except Exception as e2:
                print(e2, file)
        else:
            pass

# Double check file time duration (Google SR with only work on files =< 1 min for free)
for paths in RF:
    try:
        f = wave.open(paths, 'rb')
        frames = f.getnframes()
        rate = f.getframerate()
        duration = int(frames / float(rate))
        RF.update({paths: duration})
    except wave.Error as e:
        print('File could not be read for: ', paths)
    except EOFError as e:
        print('File could not be read for: ', paths)

# Create the dictionary of speech results
RF = {paths: duration for paths, duration in RF.items() if duration is not None}

# Speech recognition services - save to RF dictionary
for paths in RF:
    audio_file = paths
    with sr.AudioFile(audio_file) as source:  # Use the first audio files as the audio source
        audio = sr.Recognizer().record(source, duration=60)  # Read the first 60 seconds of audio file
        try:
            location = re.search('[^_]*', os.path.basename(paths))
            results = sr.Recognizer().recognize_google(audio, language='en-US', show_all=True)  # Record the tests
            if not results:
                print('error') ######HANDLE ERRORS
            else:
                RF.update({paths: results['alternative'][0]['transcript']})  # Update the dictionaries
                RF_match.update({location.group(0): results['alternative'][0]['transcript']})
                print('Speech extracted for', os.path.basename(paths))
        except sr.UnknownValueError:
            print('Could not understand the audio from ' + os.path.basename(paths) + ' or it is not a test recording')
        except sr.RequestError as e:
            print('Could not request results from Google Speech Recognition Service; {0}'.format(e) + ' or recording length is too long')

sitelist = []
spat_list = []
#
# Import the sitelist
my_file = ''
while True:
    print('')
    print('Choose how you want to match the voice note? 1) By location name (enter: name), \n'
          'By spatial coordinates (spat), or \n'
          'By date and time (dttm)?')
    my_file = input()
    if (my_file == str("spat")) or (my_file == str("dttm")):
        print('Specify a filepath to use to match spatial coordinates: ')
        input_file = input()
        spat_list = pd.read_csv(input_file,delimiter=',',index_col=False)
        spat_list["spats"] = spat_list['latitude'].apply(lambda x: f'{x:.5f}').apply(str) + ',' + spat_list['longitude'].apply(lambda x: f'{x:.5f}').apply(str)
        spat_list["times"] = spat_list["date_time"]
        spat_list["times"] = pd.to_datetime(spat_list['times'])
        spat_list["times"] = spat_list['times'].dt.strftime("%Y%m%d_%H%M%S")
    elif my_file == str("name"):
        while True:
            print("Specify a filepath to use to match location names: ")
            my_choice = input()
            if os.path.exists(my_choice):
                with open(my_choice, 'r') as file:
                    sitelist = file.read().split('\n')
                    print('')
                    print('Reading location file... ')
                    print('')
            else:
                print('Not a file, try again...')
                continue
            break
    else:
        print('Sorry, could not find this directory.')
        print('')
        continue
    break

# Create a list of the speech results
vls = list(RF.values())

# Create an empty list for the locations
locs = []

#######################################################################################################
# Define the function that will compare site names to speech (e.g. OBBA)
#######################################################################################################
def match_names(site, speech, min_score=0):
    max_score = -1
    max_site = ''
    for x in speech:
        fuzz_score = fuzz.ratio(x, site)
        if (fuzz_score > min_score) & (fuzz_score > max_score):
            max_site = x
            max_score = fuzz_score
    return (max_site, max_score)
#######################################################################################################

#######################################################################################################
# Define the function that will compare spatiotemporal information to speech (e.g. BU)
#######################################################################################################
def match_spatiotemporal(spat, speech, min_score=0):
    max_score = -1
    max_spat = ''
    for x in speech:
        fuzz_score = fuzz.ratio(x, spat)
        if (fuzz_score > min_score) & (fuzz_score > max_score):
            max_spat = x
            max_score = fuzz_score
    return (max_spat, max_score)
#######################################################################################################

# Append a list together of the matching results. Choose a minimum score threshold
while True:
   minscore = 10 #int(input("Choose a minimum score threshold between 0 and 100:"))
   if 0 <= minscore <= 100:
       break
   else:
       print('Try again')

#Create a new dictionary for results
RF2 = {}

# Conduct the renaming of files and update the dictionaries
for key, value in RF.items():
    if my_file == 'spat' or my_file == 'dttm' or my_file == 'name':
        try:
            print('Checking for lats / north...')
            str_result = re.search('(?:(?<=north)|(?<=latitude)|(?<=999)).*$',value.lower()).group()
            str_result = re.sub('coordinates','',str_result)
            str_result = re.sub('longitude|west', ',', str_result)
            str_result = re.sub('latitude|north', '', str_result)
            str_result = re.sub('four|for', '4', str_result)
            str_result = re.sub('two|too|to', '2', str_result)
            str_result = re.sub('point', '.', str_result)
            str_result = re.sub('start|and|of', '', str_result)
            str_result = re.sub(' ', '', str_result)
            str_result = re.sub('-', '', str_result)
            str_result = re.sub('/', '', str_result)
            tm = spat_list['times'].to_list()
            su = spat_list['spats'].to_list()
            key_short = re.sub('^(.*?)_', '', os.path.splitext(os.path.basename(key))[0])
            # Run the function
            if my_file == 'spat':
                mst = match_spatiotemporal(str_result, su, minscore)
                if mst[1] >= minscore:
                    RF2.update({key: [value, mst[0], mst[1]]})
                    print(key, mst[0], mst[1])
                else:
                    RF2.update({key: [value, str('Score threshold not met')]})
            elif my_file == 'dttm':
                mst = match_spatiotemporal(key_short, tm, minscore)
                if mst[1] >= minscore:
                    print(value, mst[0], mst[1])
                    RF2.update({key: [value, mst[0], mst[1]]})
                else:
                    RF2.update({key: [value, str('Score threshold not met')]})
                    break
            else:
                print('failed')
        except Exception as e:
            print('Cant find a match using current parameters')
            RF2.update({key: [value, str('Cant search string using current parameters')]})
    elif my_file == 'name':
        match = match_names(value, sitelist, minscore)
        if match[1] >= minscore:
            okey = os.path.splitext(os.path.basename(key))[0]
            newkey = str(re.sub('^(.*?)_', str(match[0] + "_"), okey) + '.wav')
            newkeytype = str(os.path.join(os.path.dirname(key) + "/" + newkey))
            RF2.update({key: [value, (str(match[0]), str(match[1])), newkeytype]})
        else:
            print('No match for ', value)
            break
    else:
        print('No lookup table provided')
        break

#Update the file paths
output_val = {}

#Choose a duration
while True:
    print('Choose a number between 60 and 600 seconds (1 and 10 minutes).')
    task_length = int(input())
    if isinstance(task_length, int):
        print('')
        print('Finding start times...')
        print('')
    else:
        print('Choose a number between 0 and 600 seconds.')
        print('')
        continue
    break

#Clip the files
for root, dirs, files in os.walk(my_dir):
    for fpath in files:
        if fpath.endswith('wav') and not fpath.startswith(tuple(pref)):
            print('Not an ABMI file')
            my_audio = AudioSegment.from_file(os.path.join(root, fpath))
            task_length_ms = task_length * 1000
            dur = my_audio.duration_seconds
            if dur < task_length:
                print('File too short for: ', fpath)
                output_val.update({fpath: str('File too short to clip for selected method')})
            elif task_length <= dur <= task_length + 30:
                diff = int(my_audio.duration_seconds - task_length)
                chunk_time = datetime.strptime(str(re.compile('(?:.*?_){1}(.*)').split(fpath)[1].rsplit('.')[0]),"%Y%m%d_%H%M%S") + timedelta(seconds=diff)
                chunk_stamp = datetime.strftime(chunk_time, "%Y%m%d_%H%M%S")
                chunk_name = os.path.join(root, fpath.replace(str(re.compile('_(.*)').split(fpath)[1].rsplit('.')[0]),chunk_stamp))
                try:
                    print('Exporting audio from end ', fpath)
                    my_audio[-task_length_ms:].export(chunk_name, format="wav")
                except:
                    pass
            elif task_length + 30 <= dur <= 900:
                chunk_length_ms = 1000
                chunks = make_chunks(my_audio[:60000], chunk_length_ms)
                file_clipped = False
                for i, chunk in enumerate(chunks):
                    r = sr.Recognizer()
                    source = sr.AudioFile(os.path.join(root, fpath))
                    with source as source:
                        ac = r.record(source, duration=2, offset=i)
                        try:
                            rc = r.recognize_google(ac, language='en-US')
                            if rc in ["start", "shark", "search", "stop"]:
                                print('Success at ', i, 'to', i + 1, ' for ', fpath, '! Exporting...')
                                chunk_time = datetime.strptime(str(re.compile('(?:.*?_){1}(.*)').split(fpath)[1].rsplit('.')[0]),"%Y%m%d_%H%M%S") + timedelta(seconds=i)
                                chunk_stamp = datetime.strftime(chunk_time, "%Y%m%d_%H%M%S")
                                chunk_name = os.path.join(root, fpath.replace(str(re.compile('_(.*)').split(fpath)[1].rsplit('.')[0]), chunk_stamp))
                                try:
                                    my_audio[i * 1000:(i * 1000) + task_length_ms].export(chunk_name, format="wav")
                                    file_clipped = True
                                    break
                                except sr.UnknownValueError:
                                    print("Could not understand audio")
                                except sr.RequestError:
                                    print("Could not request results. check your internet connection")
                        except:
                            pass
                else:
                    if not file_clipped:
                        try:
                            chunk_time = datetime.strptime(str(re.compile('(?:.*?_){1}(.*)').split(fpath)[1].rsplit('.')[0]),"%Y%m%d_%H%M%S") + timedelta(seconds=30)
                            chunk_stamp = datetime.strftime(chunk_time, "%Y%m%d_%H%M%S")
                            chunk_name = os.path.join(root, fpath.replace(str(re.compile('_(.*)').split(fpath)[1].rsplit('.')[0]), chunk_stamp))
                            my_audio[30000:task_length_ms + 30000].export(chunk_name, format="wav")
                        except:
                            pass
            else:
                print('noting happened for ', fpath, 'or duration did not meet criteria')
        else:
            print('This is an ABMI RF file', fpath)
            my_audio = AudioSegment.from_file(os.path.join(root, fpath))
            task_length_ms = task_length * 1000
            dur = my_audio.duration_seconds
            if dur < task_length:
                print('File too short for: ', fpath)
                output_val.update({fpath: str('File too short to clip for selected method')})
            elif task_length <= dur <= task_length + 30:
                print('running splitter')
                print(pydub.silence.detect_silence(my_audio[0:30000], min_silence_len = 2000, silence_thresh = -48))
                print(pydub.silence.detect_nonsilent(my_audio[0:30000], min_silence_len = 2000, silence_thresh = -48))
                print('next', fpath)
            else:
                pass

# Write the csv file
while True:
    print('\nName your output log file:')
    csv_name = input() + str('.csv')
    break
with open(csv_name, 'w') as csv_file:
    writer = csv.writer(csv_file, delimiter = ",")
    for key, value in sorted(RF2.items()):
       writer.writerow([os.path.basename(key), value])
