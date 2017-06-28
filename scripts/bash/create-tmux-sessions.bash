#! /usr/bin/env bash

music_session_name='cmus'
email_session_name='mutt'
chat_session_name='weechat'
code_session_name='code'
sessions="${music_session_name} ${email_session_name} ${chat_session_name} ${code_session_name}"

if type "tmux" >/dev/null 2>&1; then
    tmux start-server

    for session in ${sessions}; do
        create-tmux-session -c "${session}"
    done
fi
