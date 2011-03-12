#! /usr/bin/python

import sys
currentchar = ''

# "i" through "reprint" form the "apply" half of an eval/apply mutual
# recursion

i = lambda a,c: (c,a)
k = lambda a,c: (c, lambda _, c2: (c2, a))
s = lambda a,c: (c, lambda a2,c2: (c2, lambda a3, c3:\
                                   (descend, (((a,a3),(a2,a3)),c3))))
# you would think that the below definition of s would work, but it
# stumbles on "d".
#s = lambda a,c: (c, lambda a2,c2: (c2, lambda a3,c3: a(a3, lambda r1: a2(a3, lambda r2: r1(r2, c3)))))
v = lambda _,c: (c,v)
#c = lambda a,k: a(lambda a2, _: (k, a2), k)
c = lambda a, k: (descend, ((a, lambda a2, _: (k, a2)),k))
e = lambda a,_: sys.exit(a != i)

## d is a special form; it is occasionally applied, but its argument
## is already evaluated if it is (provided we are careful never to
## invoke an argument directly---see e.g. def'n of s above.)
def d(a,c):
    assert False, "This should never happen."

def readchar(a, k):
    global currentchar
    currentchar = sys.stdin.read(1)
    if not currentchar:    return (descend, ((a,v),k))  #a(v,k)
    else: return (descend, ((a,i),k)) #a(i,k)

def printchar(c):
    def p(a, k):
        sys.stdout.write(c)
        return (k,a)
    return p
r = printchar('\n')

def compchar(c):
    def comp(a, k):
        if c == currentchar: return (descend, ((a,i),k)) # a(i,k)
        else:                return (descend, ((a,v),k)) # a(v,k)
    return comp
        
def reprint(a, k):
    pc = printchar(currentchar)
    if not currentchar: return (descend, ((a,v),k)) #a(v,k)
    else: return (descend, ((a,pc),k)) # a(pc, k)

takesarg = {'.':printchar, '?':compchar}
noarg = {'|':reprint, 's':s, 'k':k, 'c':c, 'e':e, 'i':i, 'v':v, 'r':r,
         'd':d, '@':readchar}

### a pretty cowboy approach to parsing!
def bt(i):
    tree, stack, ticks, tot = [], [], 0, 0
    while tot != 2*ticks + 1:
        cur = i(1).lower()
        if not cur: raise EOFError
        tot += 1
        if cur == '#':
            while cur and cur != '\n': cur = i(1)
        if cur in takesarg:
            stack.append(takesarg[cur](i(1)))
        elif cur in noarg: stack.append(noarg[cur])
        elif cur == '`':
            stack.append(cur)
            ticks += 1
        else: tot -= 1
    while stack:
        cur = stack.pop()
        if cur == '`':
            l = tree.pop()
            tree.append((l,tree.pop()))
        else:  tree.append(cur)
    return tree[0]

### better known as "eval".
def descend(tree_cont):
    tree, cont = tree_cont
    def dcheck(lval):
        if lval == d:
            return cont(lambda rv,k2: (descend, (tree[1], lambda lv:lv(rv,k2))))
        else:
            return (descend, (tree[1], lambda rv: lval(rv, cont)))

    if isinstance(tree, tuple): ### application of tree[0] to tree[1]
        return (descend, (tree[0], dcheck))
    else:
        return (cont, tree)

if __name__ == '__main__':
    tree = bt((open(sys.argv[1]) if len(sys.argv) == 2 else sys.stdin).read)
    k, a = descend((tree, lambda x: sys.exit(x != i)))
    while 1:
        k, a = k(a)
