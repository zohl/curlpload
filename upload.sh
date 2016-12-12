#!/bin/sh
#
# This script uploads given file to the server.
# Usage:
# $ upload.sh [...script options] filename [...curl options]
#   where script options are:
#
#   --inline           Set disposition type to inline.
#   --attachment       Set disposition type to attachment.
#
#   --public           Produce shorter hash prefix in url.
#   --private          Produce longer hash prefix in url.
#
#   --expire n         Do not serve file after n days.

DISPOSITION_TYPE=x-default
VISIBILITY=default
EXPIRE=

while true; do
  case $1 in
  --inline)
    DISPOSITION_TYPE="inline"
    ;;
  --attachment)
    DISPOSITION_TYPE="attachment"
    ;;
  --public)
    VISIBILITY="public"
    ;;
  --private)
    VISIBILITY="private"
    ;;
  --expire)
    shift
    EXPIRE="$1"
    ;;
  *) break;
  esac
  shift
done

FILE=$1
shift

curl --data-binary "@$FILE" \
  -H "Content-Type: `file -bi "$FILE"`" \
  -H 'Visibility-Type: '$VISIBILITY \
  -H 'Content-Disposition: '$DISPOSITION_TYPE'; filename="'`basename "$FILE"`'"' \
  -H 'Expiration-Time: '$EXPIRE \
  "$@" \
