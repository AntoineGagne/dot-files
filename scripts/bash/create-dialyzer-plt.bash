#! /usr/bin/env bash

declare -a APPLICATIONS=(erts kernel stdlib mnesia \
                         sasl os_mon otp_mibs \
                         syntax_tools asn1 compiler \
                         snmp diameter eldap \
                         erl_interface inets \
                         jinterface megaco public_key \
                         ssh ssl wx \
                         xmerl common_test eunit)

main() {
    dialyzer --add_to_plt --apps "${APPLICATIONS[@]}"
}

main
