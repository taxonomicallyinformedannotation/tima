#!/usr/bin/env bash

OS='unsupported'
OS="$(uname)"
case $OS in
'Linux')
  OS='linux'
  alias ls='ls --color=auto'
  ;;
'FreeBSD')
  OS='unsupported'
  alias ls='ls -G'
  ;;
'WindowsNT')
  OS='unsupported'
  ;;
'MINGW64_NT-10.0-17763')
  OS='MINGW64_NT-10.0-17763'
  ;;
'Darwin')
  OS='mac'
  ;;
'SunOS')
  OS='unsupported'
  ;;
'AIX') ;;
*) ;;
esac

echo Detected OS: $OS