#!/usr/bin/env python

import os
import sys
import subprocess

class bcolors:
    OK   = '\033[92m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'

def testing(testdir, with_valgrind=False):
    """ Test a piccolo file.
    Return (actual, expected)
    """
    os.chdir(testdir)

    with open('expected0') as expf:
        expected = expf.read().strip()
    
    try:
        cmd = ['bash', 'run']
        if with_valgrind:
            cmd.append('--with-valgrind')
        out = subprocess.check_output(cmd, universal_newlines=True)
    except:
        return ("test error", expected)

    os.chdir('..')
    if with_valgrind:
        return ("OK", "OK")
    else:
        return (out.strip(), expected.strip())

if __name__ == '__main__':
    testdir = sys.argv[1]
    if len(sys.argv) == 3 and sys.argv[2] == "--with-valgrind":
        with_valgrind = True
    else:
        with_valgrind = False
    
    sys.stdout.write("Testing " + testdir + "...\t\t")
    sys.stdout.flush()
    (act, exp) = testing(testdir)
    if act == exp:
        sys.stdout.write(bcolors.OK + "OK" + bcolors.ENDC)
    else:
        sys.stdout.write(bcolors.FAIL + "FAILED" + bcolors.ENDC)
        sys.stdout.write("\nactual  : " + act + "\n")
        sys.stdout.write(  "expected: " + exp + "\n")
        sys.exit(0)
    sys.stdout.flush()

    if with_valgrind:
        sys.stdout.write("\twith valgrind...\t")
        sys.stdout.flush()
        (act, exp) = testing(testdir, with_valgrind=True)
        if act == exp:
            sys.stdout.write(bcolors.OK + "OK" + bcolors.ENDC)
        else:
            sys.stdout.write(bcolors.FAIL + "FAILED" + bcolors.ENDC)
    sys.stdout.write("\n")
