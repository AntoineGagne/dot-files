#!/usr/bin/python3
# Based on https://github.com/cmus/cmus/wiki/cmus_status.py
# Switches to python3, and adds back, forward, and pause buttons

import gi
from subprocess import call
import sys

d = dict(zip(sys.argv[1::2], sys.argv[2::2]))

if d.get('status') != 'playing':
    sys.exit()

gi.require_version('Notify', '0.7')
gi.require_version('GdkPixbuf', '2.0')
from gi.repository import GdkPixbuf, GLib, Notify

title = '{}'.format(
    d.get('title', os.path.basename(
        os.path.splitext(d.get('file', 'Untitled'))[0])
    )
)
display = d.get('artist', 'Unknown Artist')
if d.get('album'):
    display = '{} ({})'.format(display, d.get('album'))

try:
    # Set a notification icon here
    image = GdkPixbuf.Pixbuf.new_from_file(
        '/path/to/image'
    )
except:
    image = None


def cmus_callback(notification, action, *args, **kwargs):
    if action == 'next':
        call(['cmus-remote', '-n'])
    elif action == 'prev':
        call(['cmus-remote', '-r'])
    elif action == 'pause':
        call(['cmus-remote', '-u'])
    GLib.MainLoop().quit()


Notify.init("cmus-status")
n = Notify.Notification.new(title, display)
n.set_timeout(3000)
n.set_urgency(0)
if image:
    n.set_icon_from_pixbuf(image)
    n.set_image_from_pixbuf(image)
n.set_hint('transient', GLib.Variant.new_boolean(True))
n.set_hint('suppress-sound', GLib.Variant.new_boolean(True))
n.add_action('prev', 'Previous', cmus_callback, None)
n.add_action('pause', 'Pause', cmus_callback, None)
n.add_action('next', 'Next', cmus_callback, None)
n.show()
GLib.MainLoop().run()
