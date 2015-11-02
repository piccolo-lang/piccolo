#!/usr/bin/python3

import json
import os
import socket
import subprocess
import sys
import time

if len(sys.argv) != 4:
    print("USAGE: %s <exec> <src.pi> <debugsymbols>" % (sys.argv[0],))
    sys.exit(1)

executable = sys.argv[1]
with open(sys.argv[2]) as f:
    src = f.read().split('\n')

# LOADING DEBUG EVENTS
dbgFd = open(sys.argv[3])
dbgData = dbgFd.read()
dbgFd.close()
events = []
for l in dbgData.split("\n"):
    if l:
        events.append(json.loads(l))

def evtToLine(evtId):
    for evt in events:
        if evt["id"] == evtId:
            return evt["line"]
    return -1

# DEBUG VARIABLES
breakpoints = []
running     = True
currentPC   = -1

# PARSING DEBUG EVENTS
# TODO

# SETTING UP THE PROCESS AND COMMUNICATION
#subprocess.Popen(executable, env=dict(os.environ,
#                                      **{"PICC_DEBUG": "true"}))
#time.sleep(1)
# TODO: for now, user should manually start the process with
# PICC_DEBUG=true
sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
sock.connect(("127.0.0.1", 5049))
print("[debugger connected]")
r = sock.recv(1)
currentPC = evtToLine(int(r[0]))


def runToBreakpoint():
    global sock, currentPC
    while True:
        r = sock.recv(1)
        currentPC = evtToLine(int(r[0]))
        if currentPC in breakpoints:
            break
        sock.send(b"c")

while True:
    try:
        cmd = input("(piccdbg) ").strip()
    except EOFError:
        break

    ##################### exit
    if cmd == "exit" or cmd == "quit":
        break

    ##################### list
    elif cmd == "list":
        for i in range(len(src)):
            if running and currentPC == i+1:
                print("=>", end="")
            else:
                print("  ", end="")
            if i+1 in breakpoints:
                print("* %3d %s" % ((i+1), src[i]))
            else:
                print("  %3d %s" % ((i+1), src[i]))

    
    ##################### breakpoints list
    elif cmd == "b":
        for i in range(len(breakpoints)):
            print("breakpoint " + str(i+1) + " at line " + str(breakpoints[i]))

    ##################### breakpoint set
    elif cmd.startswith("b"):
        n = int(cmd.split(" ")[-1])
        if n not in breakpoints:
            breakpoints.append(n)
            print("breakpoint " + str(len(breakpoints)) + " set at line " + str(n))

    ##################### run
    elif cmd == "run" or cmd == "r":
        sock.send(b"c")
        runToBreakpoint()

    ##################### continue
    elif cmd == "continue" or cmd == "c":
        if not running:
            print("error: process not started yet")
        else:
            sock.send(b"c")
            runToBreakpoint()

    ##################### step
    elif cmd == "step" or cmd == "s":
        if not running:
            print("error: process not started yet")
        else:
            sock.send(b"c")
            r = sock.recv(1)
            currentPC = evtToLine(int(r[0]))


    else:
        print(cmd + ": command not found")

