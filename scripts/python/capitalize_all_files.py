#! /usr/bin/python3
"""Capitalizes all the files in a given directory."""

from os import listdir, rename
from os.path import isfile, join
from string import capwords
from typing import List

import sys


def capitalize_file_names(files: List[str]):
    """Capitalizes all the given files.
    :param files: The files to capitalize
    :type files: list<str>
    """
    for filename in files:
        rename(filename, capwords(filename))


if __name__ == '__main__':
    DIRECTORY_PATH = sys.argv[1]
    FILES = (single_file for single_file in listdir(DIRECTORY_PATH)
             if isfile(join(DIRECTORY_PATH, single_file)))
    capitalize_file_names(FILES)
