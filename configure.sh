#! /bin/sh

./link_dot_files.sh \
    && ./nvim_setup.sh \
    && ./setup_virtual_envs.sh \
    && ./link_redshift.sh
