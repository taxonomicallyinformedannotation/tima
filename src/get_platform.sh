#!/usr/bin/env bash
# -*- coding: utf-8 -*-

OS='unsupported'
OS="`uname`"
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