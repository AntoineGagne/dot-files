from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter


def main():
    arguments = _parse_arguments()
    arguments.execute(arguments)


def _parse_arguments():
    parser = ArgumentParser(
        formatter_class=ArgumentDefaultsHelpFormatter, prog="", description=""
    )
    subparser = parser.add_subparsers(
        title="Commands",
        description="These are various commands available for the application:",
        required=True,
    )
    _create_subparser(subparser)

    return parser.parse_args()


def _create_subparser(subparser):
    parser = subparser.add_parser(
        "start", formatter_class=ArgumentDefaultsHelpFormatter
    )
    parser.set_defaults(execute=_start)


def _start(arguments):
    pass


if __name__ == "__main__":
    main()
