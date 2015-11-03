#!/usr/bin/python3

import curses
import json
import os
import socket
import struct
import sys

if len(sys.argv) != 4:
    print("USAGE: %s <exec> <src.pi> <debugsymbols>" % (sys.argv[0],))
    sys.exit(1)



############### setup the curses windows

stdscr = curses.initscr()

(ymax, xmax) = stdscr.getmaxyx()

codebox = curses.newwin(ymax - 10, xmax // 2, 0, 0)
codebox.box(0, 0)
codebox.refresh()

code = curses.newwin(ymax - 12, (xmax // 2) - 2, 1, 1)
code.refresh()

infosbox = curses.newwin(ymax - 10, xmax // 2, 0, xmax // 2)
infosbox.box(0, 0)
infosbox.refresh()

infos = curses.newwin(ymax - 12, (xmax // 2) - 2, 1, (xmax // 2) + 1)
infos.refresh()

bottombox = curses.newwin(10, xmax, ymax - 10, 0)
bottombox.box(0, 0)
bottombox.refresh()

log = curses.newwin(7, xmax - 2, ymax - 9, 1)
log.refresh()

prompt = curses.newwin(1, xmax - 2, ymax - 2, 1)
prompt.refresh()

log.scrollok(True)
log.move(0, 0)

def logPrint(s):
    global log
    log.addstr(s + b"\n")
    log.refresh()

###############

with open(sys.argv[3]) as f:
    data = f.read()
events = []
for l in data.split('\n'):
    if l:
        events.append(json.loads(l))
logPrint(b"events table loaded")

def evtToLine(evtId):
    for evt in events:
        if evt["id"] == evtId:
            return evt["line"]

with open(sys.argv[2]) as f:
    src = f.read().split('\n')
logPrint(b"code loaded")

breakpoints = []
running     = True
currentPC   = -1

sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
sock.connect(("127.0.0.1", 5049))
logPrint(b"debugger connected to debuggee server")
currentPC = evtToLine(int(sock.recv(1)[0]))

def codePrint():
    global running, currentPC
    code.move(0, 0)
    for i in range(len(src)):
        if running and currentPC == i + 1:
            code.addstr("=>")
        else:
            code.addstr("  ")
        if i + 1 in breakpoints:
            code.addstr("* %3d %s" % ((i+1), src[i]))
        else:
            code.addstr("  %3d %s" % ((i+1), src[i]))
        code.addstr("\n")
    code.refresh()

def infosPrint(data):
    infos.clear()
    infos.move(0, 0)
    infos.addstr(data)
    infos.refresh()

def promptInput():
    prompt.move(0, 0)
    prompt.addstr("> ")
    s = prompt.getstr(0, 2, 30)
    prompt.clear()
    prompt.move(0, 0)
    prompt.addstr("> ")
    return s.strip()

def runToBreakpoint():
    global sock, currentPC
    while True:
        currentPC = evtToLine(int(sock.recv(1)[0]))
        if currentPC in breakpoints:
            break
        sock.send(b"c")

def getAndPrintInfos():
    global sock
    sock.send(b"i")
    size = struct.unpack("<I", sock.recv(4))[0]
    logPrint(b"reading " + bytes(str(size), "utf-8") + b" bytes of data")
    data = sock.recv(size)
    infosPrint(data)



codePrint()
while True:
    cmd = promptInput()

    ############################# exit
    if cmd == b"exit" or cmd == b"quit":
        break

    ############################# breakpoints list
    elif cmd == b"b":
        if breakpoints == []:
            logPrint(b"no breakpoint set")
        else:
            for i in range(len(breakpoints)):
                logPrint(b"breakpoint " + bytes(str(i + 1), "utf-8") +
                         b" at line " + bytes(str(breakpoints[i]), "utf-8"))

    ############################# breakpoint set
    elif cmd.startswith(b"b"):
        n = int(cmd.split(b" ")[-1])
        if n not in breakpoints:
            breakpoints.append(n)
            logPrint(b"breakpoint " + bytes(str(len(breakpoints)), "utf-8") +
                     b" set at line " + bytes(str(n), "utf-8"))
            codePrint()

    ############################# continue
    elif cmd == b"continue" or cmd == b"c":
        if not running:
            logPrint(b"error: process not started yet")
        else:
            sock.send(b"c")
            runToBreakpoint()
            codePrint()
            getAndPrintInfos()

    ############################# step
    elif cmd == b"step" or cmd == b"s":
        if not running:
            logPrint(b"error: process not started yet")
        else:
            sock.send(b"c")
            currentPC = evtToLine(int(sock.recv(1)[0]))
            codePrint()
            getAndPrintInfos()

    ############################# infos
    elif cmd == b"infos" or cmd == b"i":
        if not running:
            logPrint(b"error: process not started yet")
        else:
            getAndPrintInfos()

    ############################# command not found!
    else:
        logPrint(b"error: unknown command \"" + cmd + b"\"")
