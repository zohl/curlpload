#!/bin/sh
#
# This script uploads given file to the server.
# Usage:
# $ upload.sh [...script options] filename [...curl options]
#   where script options are:
#   --inline           Set disposition type to inline
#   --attachment       Set disposition type to attachment

DISPOSITION_TYPE=x-default

while true; do
  case $1 in
  --inline)
    DISPOSITION_TYPE="inline"
    ;;
  --attachment)
    DISPOSITION_TYPE="attachment"
    ;;
  *) break;
  esac
  shift
done

FILE=$1

curl --data-binary "@$FILE" \
  -H "Content-Type: `file -bi "$FILE"`" \
  -H 'Content-Disposition: '$DISPOSITION_TYPE'; filename="'`basename "$FILE"`'"' \
  "$@" \
