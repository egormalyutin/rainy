#!/bin/bash
echo '{ "command": ["cycle", "pause"] }' | socat - /tmp/mpv-bg-socket
