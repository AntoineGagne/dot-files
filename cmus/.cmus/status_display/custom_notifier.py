"""A custom module to output Cmus notifications to named pipes."""

from glob import iglob
from os.path import expandvars
from textwrap import shorten


DISPLAY_BY_STATUS = {
    'Stopped': '<fn=1></fn> Stopped',
    'Paused': '<fn=1></fn> Paused'
}


def send_notification(arguments, information):
    """Send the notification to the OS with a Python library.

    :param title: The message's title
    :type title: str
    :param text: The message's body
    :type text: str
    """
    status = DISPLAY_BY_STATUS.get(
        information.status,
        arguments['body'].format(**information.__dict__)
    )
    message = shorten(status, width=42, placeholder='…')
    print(message)


def find_all_information_pipes():
    """Find all the pipes to write the song information to.

    :returns: The pipes to write to
    """
    home = expandvars('${HOME}')
    return iglob('{0}/.song-information-*'.format(home))
