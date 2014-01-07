#!/bin/sh

# demonize : http://stackoverflow.com/questions/6830806/running-erlang-shell-as-a-deamon-service

OPTS=$*

NOSHELL="-noshell"
COMPILE=false
CLEAN=false
APP=""
NAME=""

help() {
  MESSAGE=$1
  if [ "x$MESSAGE" != "x" ] ; then
    echo $MESSAGE
  fi
  echo "Usage : start.sh [options] server|storage|poller"
  echo ""
  echo "Options :"
  echo "  -c --console     : Run in console mode"
  echo "  -C --compile     : Compile code before run"
  echo "  -K --clean       : Clean and compile code before run"
  echo "  -n --name [name] : Set node name"
  echo "  -h --help        : Display this message"
}

while (( "$#" )); do
  case $1 in
    -c|--console) 
      NOSHELL="" ;;
    -C|--compile) 
      COMPILE=true ;;
    -K|--clean) 
      CLEAN=true ; COMPILE=true ;;
    -n|--name)
      shift ; NAME=$1 ;;
    -h|--help)
      help ; exit 0 ;;
    *)
      APP=$1
  esac
  shift
done

if [ "x$APP" = "x" ] ; then
  help "No app given. I don't know what to start!"
  exit 1
fi

if [ "x$NAME" = "x" ] ; then
  NAME=${APP}_$(uuidgen)
fi

if [ $CLEAN = true ] ; then
  ./rebar clean
fi
if [ $COMPILE = true ] ; then
  ./rebar compile
fi

erl +pc unicode -pa $PWD/ebin $PWD/apps/ebill_$APP/ebin $PWD/deps/*/ebin $NOSHELL -name $NAME -s ebill_$APP
