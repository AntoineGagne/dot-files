#! /usr/bin/python3
"""Module that fetches .gitignore informations from gitignore.io."""

from argparse import (ArgumentParser,
                      ArgumentDefaultsHelpFormatter,
                      Action,
                      SUPPRESS)

import requests


API_URL = 'https://www.gitignore.io/api/'


def arguments_parser():
    """Parses the command line arguments.

    :return: The argument parser
    """
    parser = ArgumentParser(formatter_class=ArgumentDefaultsHelpFormatter,
                            prog='create_gitignore',
                            description='Calls the API defined by '
                                        'gitignore.io')
    parser.add_argument('-l',
                        '--list',
                        default=SUPPRESS,
                        type=bool,
                        action=ToolListing,
                        dest='api_answer',
                        help='Lists the different options.')
    parser.add_argument('-t',
                        '--tools',
                        type=str,
                        nargs='+',
                        default=SUPPRESS,
                        action=GitignoreListing,
                        dest='api_answer',
                        metavar=('LANGUAGE', 'TOOL'),
                        help='The different languages and tools that will be '
                             'used in the project.')
    return parser.parse_args()


class ToolListing(Action):
    """Action that lists the different tools accepted by the API."""
    def __init__(self, option_strings, dest, nargs=0, **kwargs):
        """Initializes the class"""
        super(ToolListing, self).__init__(option_strings,
                                          dest,
                                          nargs=nargs,
                                          **kwargs)

    def __call__(self, parser, namespace, values, option_string=None):
        """Overloads the call operator."""
        response = requests.get('{0}{1}'.format(API_URL, 'list'))
        setattr(namespace, self.dest, response.text)


class GitignoreListing(Action):
    """Action that fetches the corresponding .gitignore from the API."""
    def __call__(self, parser, namespace, values, option_string=None):
        """Overloads the call operator."""
        response = requests.get('{0}{1}'.format(API_URL, ','.join(values)))
        setattr(namespace, self.dest, response.text)


if __name__ == '__main__':
    results = arguments_parser()
    print(results.api_answer)
