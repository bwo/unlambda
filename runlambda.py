import os, sys

class Currentchar:
    def __init__(self):
        self.c = ''
curr = Currentchar()
curr.c = ''

class Op(object):
    def __init__(self):
        pass
class Tick(Op): #annoying
    pass
class I(Op):
    def __call__(self, a, c):
        return (c,a)
class K(Op):
    def __call__(self, a, c):
        return (c, K2(a))
class K2(Op):
    def __init__(self, r):
        self.r = r
    def __call__(self, _, c):
        return (c, self.r)
class S(Op):
    def __call__(self, a, c):
        return (c, S2(a))
class S2(Op):
    def __init__(self, first):
        self.first = first
    def __call__(self, a, c):
        return (c, S3(self.first, a))
class S3(Op):
    def __init__(self, first, second):
        self.f = first
        self.s = second
    def __call__(self, a, c):
        return (Cont(), P(c,App(App(self.f,a),App(self.s,a))))
class V(Op):
    def __call__(self, _, c):
        return (c, self)
class E(Op):
    def __call__(self, a, c):
        raise SystemExit(not isinstance(a, I))
        return (c, a)
class D(Op):
    pass
        
class D2(Op):
    def __init__(self, right):
        self.right = right
    def __call__(self, rv, k2):
        return (Cont(), P(D_delayed(rv, k2), self.right))

class Readchar(Op):
    def __call__(self, a, c):
        curr.c = os.read(0,1)
        if not curr.c: return (Cont(), P(c, App(a,V())))
        return (Cont(), P(c, App(a,I())))
class Printchar(I):
    def __init__(self, char):
        self.char = char or ''# convince pypy translation that this is never None
    def __call__(self, a, c):
        os.write(1, self.char)
        return (c,a)
class R(Printchar):
    def __init__(self):
        Printchar.__init__(self,'\n')
class Compchar(Op):
    def __init__(self, char):
        self.char = char
    def __call__(self, a, c):
        if self.char == curr.c:
            return (Cont(), P(c, App(a,I())))
        return (Cont(), P(c, App(a,V())))
class Reprint(Op):
    def __call__(self, a, c):
        if not curr.c: return (Cont(), P(c,App(a,V())))
        return (Cont(), P(c,App(a, Printchar(curr.c))))
class C(Op):
    def __call__(self, a, c):
        return (Cont(), P(c,App(a, Callcc(c))))
class Callcc(Op):
    def __init__(self, cont):
        self.cont = cont
    def __call__(self, a, _):
        return (self.cont, a)
class App(Op):
    def __init__(self, left, right):
        self.left = left
        self.right = right
class P(Op): # also annoying
    def __init__(self, cont, tree):
        self.cont = cont
        self.tree = tree
noarg_ = {'i':I, 'v':V, 'e':E, 'd':D, 's':S, 'k':K, '|':Reprint, 'r':R,
         '@':Readchar, 'c':C}
takesarg = {'.':Printchar, '?':Compchar}

def bt(fileno):
    noarg = {}
    for n in noarg_.keys():
        noarg[n] = noarg_[n]()
    tree, stack, ticks, tot = [], [], 0, 0
    while tot != 2*ticks + 1:
        cur = os.read(fileno,1).lower()
        if not cur: raise EOFError
        tot += 1
        if cur == '#':
            while cur and cur != '\n': cur = os.read(fileno, 1)
            if not cur: raise EOFError
        if cur in noarg:
            stack.append(noarg[cur])
        elif cur in takesarg:
            arg = os.read(fileno, 1)
            if not arg: raise EOFError
            stack.append(takesarg[cur](arg))
        elif cur == '`':
            stack.append(Tick())
            ticks += 1
        else:
            tot -= 1
    while stack:
        item = stack.pop()
        if isinstance(item, Tick):
            left = tree.pop()
            right = tree.pop()
            tree.append(App(left, right))
        else:
            tree.append(item)
    return tree[0]

class Cont(object):
    def __init__(self):
        pass
    def __call__(self, p):
        tree = p.tree
        cont = p.cont
        if isinstance(tree, App):
            dchecker = Dchecker(tree.right, cont)
            return (self, P(dchecker, tree.left))
        else:
            return cont.__call__(tree)

class Exiter(Cont):
    def __call__(self, o):
        raise SystemExit(not isinstance(o, I))

class Dchecker(Cont):
    def __init__(self, right, cont):
        self.right = right
        self.cont = cont
    def __call__(self, lval):
        if isinstance(lval, D):
            return self.cont.__call__(D2(self.right))
        else:
            return (Cont(), P(D_undelayed(lval, self.cont), self.right))

class D_delayed(Cont):
    def __init__(self, rv, k2):
        self.rv = rv
        self.k2 = k2
    def __call__(self, lv):
        return lv.__call__(self.rv, self.k2)
    
class D_undelayed(Cont):
    def __init__(self, lval, cont):
        self.lval = lval
        self.cont = cont
    def __call__(self, rval):
        return self.lval.__call__(rval, self.cont)
    
def run(argv):
    if len(argv) == 1: fileno = 0
    else: fileno = os.open(argv[1], os.O_RDONLY, 0777)
    tree = bt(fileno)
    descend = Cont()
    k_, a = descend, P(Exiter(), tree)
    while not isinstance(k_, Exiter):
        k_, a = k_.__call__(a)
    if isinstance(a, I): return 0
    return 1

def target(*a):
    return (run, None)

if __name__ == '__main__':
    run(sys.argv)
