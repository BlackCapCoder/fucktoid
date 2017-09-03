# Figurehead interpreter
# written by Keymaker

import sys

if len(sys.argv) < 2:
    print "Error, need a Figurehead program to run."
    sys.exit()

try:
    f = open(sys.argv[1])
except IOError:
    print "Error in reading the program."
    sys.exit()
s = f.read()
f.close()

debug = 0
if len(sys.argv) == 3:
    debug = 1
elif len(sys.argv) > 3:
    debug = 2

prog = []
mem = []

def popval():
    global mem

    if len(mem) == 0:
        print "Error, can't access an empty stack."
        sys.exit()

    return mem.pop()

def remval(a):
    global mem

    i = 0
    while 1:
        if mem[i] == a:
            del mem[i]
            break
        i = i + 1

al = 0
bl = 0
p = -1
s = s + "_"
i = 0
while i < len(s):
    if s[i] == "|":
        if p == 0 or i == 0:
            al = al + 1
        else:
            prog.append(bl * -1)
            bl = 0
            al = al + 1
        p = 0
    elif s[i] == " ":
        if p == 1 or i == 0:
            bl = bl + 1
        else:
            prog.append(al)
            al = 0
            bl = bl + 1
        p = 1
    elif s[i] == "_" and i == len(s) - 1:
        if p == 0:
            prog.append(al)
        elif p == 1:
            prog.append(bl * -1)
    else:
        print "Erroneous data in the program."
        sys.exit()
    i = i + 1

i = 0
while i < len(prog):
    if prog[i] == -1 or prog[i] == 1:
        del prog[i]
        i = i - 1
    i = i + 1

e = []
i = 0
while i < len(prog):
    if prog[i] < 0:
        if e.count(prog[i]) == 1:
            if e[len(e) - 1] == prog[i]:
                e.pop()
            else:
                print "Erroneous looping."
                sys.exit()
        else:
            e.append(prog[i])
    i = i + 1
if len(e) > 0:
    print "Erroneous looping."
    sys.exit()

lid = []
lval = []
n = 0
while n < len(prog):
    if debug == 2:
        print mem

    if prog[n] < 0:
        if lid.count(prog[n]) == 0:
            a = popval()
            if mem.count(a) > 0:
                remval(a)
                lid.append(prog[n])
                lval.append(a)
            else:
                i = n + 1
                while prog[i] != prog[n]:
                    i = i + 1
                n = i
        else:
            if mem.count(lval[len(lval) - 1]) == 0:
                lid.pop()
                lval.pop()
            else:
                remval(lval[len(lval) - 1])
                i = n - 1
                while prog[i] != prog[n]:
                    i = i - 1
                n = i
    elif prog[n] > 0:
        mem.append(prog[n])
    n = n + 1

if debug:
    print "Done."
    print "Program:", prog
    print "Memory:", mem
    c = []
    for i in mem:
        c.append(str(i) + ":" + str(mem.count(i)))
    print "Counts:", list(set(c))
