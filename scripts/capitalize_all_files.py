#! /usr/bin/python3

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
    directory_path = sys.argv[1]
    files = (single_file for single_file in listdir(directory_path) if isfile(join(directory_path, single_file)))
    capitalize_file_names(files)
