#! /usr/bin/env python3

"""Contains code to display current status of Cmus."""

import sys
from collections import defaultdict
from functools import reduce
from typing import List


#: Default text for the display values
DEFAULT_STATUS_DISPLAY = 'N/A'
#: The various field contained in the metadata
FIELDS = (
    'status',
    'file',
    'artist',
    'album',
    'discnumber',
    'tracknumber',
    'title',
    'date',
    'duration'
)


class StatusInformation:
    """The status information of Cmus"""

    def __init__(self, **kwargs) -> None:
        """Initialize a :class:`StatusInformation` object.

        :param kwargs: See below.

        :Keyword Arguments:
            * *album* (``str``) --- The name of the song's album
            * *artist* (``str``) --- The name of the song's artist(s)
            * *date* (``str``) --- The date at which the song was composed
            * *discnumber* (``int``) --- The disc's number
            * *duration* (``int``) --- The duration of the song
            * *file* (``str``) --- The place from which the song is read
            * *status* (``str``) --- Cmus current status
            * *title* (``str``) --- The song's title
            * *tracknumber* (``int``) --- The song's number
        """
        self.album = kwargs.get('album', DEFAULT_STATUS_DISPLAY)
        self.artist = kwargs.get('artist', DEFAULT_STATUS_DISPLAY)
        self.date = kwargs.get('date', DEFAULT_STATUS_DISPLAY)
        self.discnumber = kwargs.get('discnumber', DEFAULT_STATUS_DISPLAY)
        self.duration = kwargs.get('duration', DEFAULT_STATUS_DISPLAY)
        self.file = kwargs.get('file', DEFAULT_STATUS_DISPLAY)
        self.status = kwargs.get('status', DEFAULT_STATUS_DISPLAY)
        self.title = kwargs.get('title', DEFAULT_STATUS_DISPLAY)
        self.tracknumber = kwargs.get('tracknumber', DEFAULT_STATUS_DISPLAY)


def _parse_status_information(informations: List[str]):
    """Parse the status informations from the informations list.

    :param informations: The list containing the various song's information
    :type informations: list<str>
    :returns: :class:`collections.defaultdict`
    """
    fields = set(FIELDS)
    status_information = defaultdict(list)
    current_status = None
    for word in informations:
        if word in FIELDS:
            current_status = word
            fields.remove(word)
        if current_status and word not in FIELDS:
            status_information[current_status].append(word)

    return status_information


def _format_status_information_fields(status_information, *formatters) -> StatusInformation:
    """Format the status informations dictionary.

    :param status_information: The various fields information
    :type status_information: :class:`collections.defaultdict`
    :param formatters: The formatters functions
    :type formatters: tuple<callable>
    :returns: A status information object containing the various information
    :rtype: :class:`StatusInformation`
    """
    return StatusInformation(
        **reduce(
            lambda information, formatter: formatter(information),
            formatters,
            status_information
        )
    )


def _format_artist_field(status_information):
    """Format the *artist* field.

    :param status_information: The various fields information
    :type status_information: :class:`collections.defaultdict`
    :returns: The updated status informations
    :rtype: :class:`collections.defaultdict`
    """
    status_information['artist'] = ' '.join(status_information['artist'])
    return status_information


def _format_duration_field(status_information):
    """Format the *duration* field.

    :param status_information: The various fields information
    :type status_information: :class:`collections.defaultdict`
    :returns: The updated status informations
    :rtype: :class:`collections.defaultdict`
    """
    try:
        duration = int(''.join(status_information['duration']))
        status_information['duration'] = '{0:02d}:{1:02d}'.format(
            duration // 60,
            duration % 60
        )
    except ValueError:
        status_information.pop('duration', None)

    return status_information


def _format_left_fields(status_information):
    """Format the unformatted fields.

    :param status_information: The various fields information
    :type status_information: :class:`collections.defaultdict`
    :returns: The updated status informations
    :rtype: :class:`collections.defaultdict`
    """
    for key, value in status_information.copy().items():
        if isinstance(value, list):
            status_information[key] = ' '.join(value)
    return status_information


if __name__ == '__main__':
    INFORMATION_BY_FIELD_NAME = _parse_status_information(sys.argv[1].split())
    STATUS_INFORMATION = _format_status_information_fields(
        INFORMATION_BY_FIELD_NAME,
        _format_artist_field,
        _format_duration_field,
        _format_left_fields
    )
    print(STATUS_INFORMATION.__dict__)
