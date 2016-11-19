from collections import defaultdict
from errno import EEXIST
from os import listdir, makedirs
from os.path import isfile, join
from shutil import move
from string import capwords

import re
import sys

AUDIO_FILE_PATTERN = re.compile(r'^(?P<file_name>\D+)\s+-\s+.+')

def process_audio_files(files):
    """Processes the audio files and regroups them in a dictionary by their artist's name.
    :param files: The audio files
    :type files: list<str>
    :return: The grouped audio files by their artist's name
    :rtype: defaultdict
    """
    audio_files_by_artist_name = defaultdict(list)
    for audio_file in files:
        file_name_match = AUDIO_FILE_PATTERN.match(audio_file)
        if file_name_match:
            audio_files_by_artist_name[file_name_match.group('file_name').lower().strip()].append(audio_file)

    return audio_files_by_artist_name


def move_audio_files(artist_directory, audio_files):
    """Moves the audio files to their artist's directory.
    :param artist_directory: The artist's directory path
    :type artist_directory: str
    :param audio_files: The audio files to move
    :type audio_files: list<str>
    """
    for audio_file in audio_files:
        move(audio_file, artist_directory)


def create_artist_directory(base_path, artist_name):
    """Creates the artist's directory.
    :param base_path: The path in which the program was called
    :type base_path: str
    :param artist_name: The name of the artist
    :type artist_name: str
    :return: The newly created directory name
    :rtype: str
    """
    artist_directory = join(base_path, capwords(artist_name))
    create_directory(artist_directory)
    return artist_directory


def create_directory(path):
    """Create a directory at the given path if it doesn't exist.
    :param path: The path to the directory
    :type path: str
    """
    try:
        makedirs(path)
    except OSError as exception:
        if exception.errno != EEXIST:
            raise


if __name__ == '__main__':
    directory_path = sys.argv[1]
    files = (audio_file for audio_file in listdir(directory_path) if isfile(join(directory_path, audio_file)))

    audio_files_by_artist_name = process_audio_files(files)
    for artist_name, audio_files in audio_files_by_artist_name.viewitems():
        artist_directory = create_artist_directory(directory_path, artist_name)
        move_audio_files(artist_directory, audio_files)
