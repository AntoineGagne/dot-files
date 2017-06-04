#! /usr/bin/python

"""This module contains code that will call xrandr utility with the correct 
   number of screens.
"""

import re
import subprocess


#: The regex used to fetch the screens' names
SCREEN_REGEX = r'^(.+)\s+connected'


def fetch_screens(xrandr_output):
    """Fetch the screens names from xrandr's output.

    :param xrandr_output: xrandr's output
    :type xrandr_output: str
    :returns: Match objects that correspond to the screens' names
    """
    return re.finditer(SCREEN_REGEX, xrandr_output, flags=re.MULTILINE)


def build_program_string(screens):
    """Build the command line program string.

    :param screens: The screens' match objects
    :returns: A string that corresponds to the program to invoke
    """
    formatted_screens = (screen.expand('--output \g<1> --auto') for screen in screens)
    return 'xrandr {0} --noprimary'.format(' '.join(formatted_screens))


if __name__ == '__main__':
    XRANDR_OUTPUT = subprocess.check_output(['xrandr']).decode('utf-8')
    SCREENS = fetch_screens(XRANDR_OUTPUT)
    subprocess.run(build_program_string(SCREENS), shell=True)
