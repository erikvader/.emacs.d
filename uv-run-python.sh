#!/bin/sh

# Emacs can't handle `python-shell-interpreter` being "uv run python", so this script is
# used as a workaround for that.

exec uv run python "$@"
