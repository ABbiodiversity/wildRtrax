# wildRtrax rename tool

# Recursively batch renames audio files using the location folder name as the naming convention for the audio files beneath
# Provides directory flattening and some data cleanup as well

# Import libraries
import os, re, csv

# Choose working directory
my_dir = ''
while True:
    print('')
    print('Welcome to the wildRtrax rename tool! Please choose a root directory that holds your audio files:')
    my_dir = input()
    if os.path.exists(my_dir):
        print('Scanning for files ... this may take a moment ... ')
        print('')
    else:
        print('Sorry, could not find this directory.')
        print('')
        continue
    break

# Modify only audio files
ext = ['.mp3', '.wav', '.wac', '.flac']
# Ignore the rest
other = ['.txt', '.dump']
sub_detect = ['SD A', 'SD B', 'A', 'B', 'Data']
ignore = ['.DS_Store']

# Set up lists and dictionary
wronglist = []
rightlist = []
fixlist = []
otherlist = []
fix_log = []

count_of_audio_files = len(
    [files for root, dirs, files in os.walk(my_dir) for file in files if file.endswith(tuple(ext))])

# Check for subdirectories within the location directories - this will affect naming conventions since audio
# files in a subdirectory will be given the name of its closest parent
def folders_in(path_to_parent):
    for fname in os.listdir(path_to_parent):
        if os.path.isdir(os.path.join(path_to_parent, fname)):
            yield os.path.join(path_to_parent, fname)

# List all the subdirectory paths
sub = [x[0] for x in os.walk(my_dir)]

# Find if any folders are empty and delete
for folder in sub:
    if os.path.exists(folder) and os.path.isdir(folder):
        if not os.listdir(folder):
            print('Directory', folder, 'is empty - would you like to delete it? Y / any key')
            remove_empty = input()
            if remove_empty == "Y":
                os.rmdir(folder)
            elif remove_empty == "N":
                pass
            else:
                break
        else:
            pass
    else:
        break

# Scan again and re-populate sub - this sucks should include it in the loop above
sub = [x[0] for x in os.walk(my_dir)]

# Scan the root directory and append files to each list (right or wrong names)
for root, dirs, files in os.walk(my_dir):
    for file in files:
        if file.endswith(tuple(ext)):
            s = re.search('^[^_]+', file)
            if (os.path.split(os.path.dirname(os.path.join(root, file)))[1]) == s.group(0):
                rightlist.append(file)
            else:
                wronglist.append(os.path.join(root, file))
        elif file.endswith(tuple(other)):
            otherlist.append(file)
        else:
            pass

# Create a set of unique location directories
for file in wronglist:
    u = os.path.basename(os.path.dirname(file))
    fixlist.append(u)
fixlist = set(fixlist)

# Print summary statistics on the scan
[print('Important:', len(list(folders_in(dirs))), 'subdirectories detected in', dirs) for dirs in sorted(sub)]
print('')
print('Number of unique subdirectories:', len(sub))
print('Number of audio files:', count_of_audio_files)
print('Found', len(other), 'other files that are not audio')
print(round((len(fixlist) / len(sub)) * 100, 2),
      '% of directories have unmatched file name conventions. Follow the prompts below to repair file names.')
print('')
# print(*[item for item in wronglist if re.match('[^_]*', os.path.basename(item)).group(0) not in fixlist], sep='\n')

# Approve / deny fixes to each location
for wrong_files in sorted(wronglist):
    filename_to_test = os.path.basename(wrong_files)
    filename_to_test_to_underscore = filename_to_test[0:filename_to_test.find("_")]  # part of filename up to first _
    dir_name_to_test = os.path.dirname(wrong_files)
    dir_name_to_test_to_slash = dir_name_to_test[
                                dir_name_to_test.rfind("/") + 1:]  # search starting at right and go to first slash
    if dir_name_to_test_to_slash == filename_to_test_to_underscore:
        print(wrong_files, 'matches folder name, no action')  # Print a message
        fix_log.append((dir_name_to_test + '/' + filename_to_test, 'No changes filename matches directory'))
    else:
        fix_log.append((dir_name_to_test, dir_name_to_test + '/' + filename_to_test, dir_name_to_test + "/" + dir_name_to_test_to_slash + filename_to_test[filename_to_test.find("_"):]))

if not fix_log:
    sett = {}
else:
    sett = set(list(zip(*fix_log))[0])

for item in sett.copy():
    print('\nDo you want to change', len(os.listdir(item)), 'files in', re.match('[^_]*', item).group(0), '? Y, N or X to cancel all repair operations')
    user_response = input()
    if user_response == 'Y' or user_response == 'y':
        pass
    elif user_response == 'N' or user_response == 'n':
        sett.remove(item)
    elif user_response == 'X' or user_response == 'x':
        sett.clear()
        break
    else:
        print('\nPlease use Y, N, or X')
    continue

fix_log2 = [x for x in fix_log if x[0] in sett]
print('\nFiles with errors: ',len(fix_log))
print('\nFiles being fixed: ',len(fix_log2))

for stuff in fix_log2:
    print(stuff[1],'***',stuff[2])
    os.rename(stuff[1], stuff[2])

# Write the csv file
while True:
    print('\nName your output log file:')
    csv_name = input() + str('.csv')
    break

with open(csv_name, 'w') as csv_file:
    writer = csv.writer(csv_file)
    writer.writerow(['Directory,OldPath,NewPath'])
    for filename in fix_log2:
        writer.writerow([filename])
