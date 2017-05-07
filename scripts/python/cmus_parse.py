#!/usr/bin/env python

"""A script that parses Cmus output."""

from collections import defaultdict
from functools import reduce, partial

import sys


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


def parse_status_information(informations):
    """Parse the status informations from the informations list.

    :param informations: The list containing the various song's information
    :type informations: str
    :returns: :class:`collections.defaultdict`
    """
    fields = set(FIELDS)
    status_information = defaultdict(list)
    current_status = None
    for word in informations.split():
        if word in fields:
            current_status = word
        if current_status and word not in fields:
            status_information[current_status].append(word)

    return _format_status_information_fields(
        status_information,
        _format_artist_field,
        _format_duration_field,
        partial(_format_integer_field, 'tracknumber'),
        partial(_format_integer_field, 'discnumber'),
        _format_status_field,
        _format_left_fields
    )


def _format_status_information_fields(status_information, *formatters):
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


def _format_status_field(status_information):
    """Format the *status* field.

    :param status_information: The various fields information
    :type status_information: :class:`collections.defaultdict`
    :returns: The updated status informations
    :rtype: :class:`collections.defaultdict`
    """
    status_information['status'] = (' '.join(status_information['status'])
                                       .capitalize())
    return status_information


def _format_integer_field(field_name, status_information):
    """Format an integer field.

    :param field_name: The name of the field
    :type field_name: str
    :param status_information: The various fields information
    :type status_information: :class:`collections.defaultdict`
    :returns: The updated status informations
    :rtype: :class:`collections.defaultdict`
    """
    try:
        status_information[field_name] = int(''.join(status_information[field_name]))
    except ValueError:
        status_information.pop(field_name, None)

    return status_information


def _format_left_fields(status_information):
    """Format the unformatted fields.

    :param status_information: The various fields information
    :type status_information: :class:`collections.defaultdict`
    :returns: The updated status informations
    :rtype: :class:`collections.defaultdict`
    """
    for key, value in status_information.copy().items():
        if isinstance(value, list) and value:
            status_information[key] = ' '.join(value)
        elif not value:
            status_information.pop(key, None)
    return status_information


if __name__ == '__main__':
    STATUS_INFORMATION = parse_status_information(sys.argv[2])
    print(sys.argv[1].format(**STATUS_INFORMATION.__dict__))
