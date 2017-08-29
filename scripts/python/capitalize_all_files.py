#! /usr/bin/python3
"""Capitalizes all the files in a given directory."""

from os import listdir, rename
from os.path import isfile, join
from string import capwords

import sys


def capitalize_file_names(files, path):
    """Capitalizes all the given files.
    :param files: The files to capitalize
    :type files: list<str>
    """
    for filename in files:
        rename(
            join(path, filename),
            join(path, capwords(filename))
        )


if __name__ == '__main__':
    DIRECTORY_PATH = sys.argv[1]
    FILES = (single_file for single_file in listdir(DIRECTORY_PATH)
             if isfile(join(DIRECTORY_PATH, single_file)))
    capitalize_file_names(FILES, DIRECTORY_PATH)
