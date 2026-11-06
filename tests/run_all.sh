#!/bin/sh
# Runs every test file. test_media.scm needs ffmpeg/ffprobe on the PATH;
# set SKIP_MEDIA=1 to leave it out.
cd "$(dirname "$0")/.." || exit 1
status=0
for t in tests/test_pure_*.scm; do
  scm "$t" || status=1
done
if [ -z "$SKIP_MEDIA" ]; then
  scm tests/test_media.scm || status=1
fi
exit $status
