import os, sys

class Currentchar:
    def __init__(self):
        self.c = ''
curr = Currentchar()

class Op(object):
    def __init__(self):
        pass
class Tick(Op):
    '''
A graceless hack so that the stack in bt() is homogenous.
'''
    pass
class I(Op):
    def app(self, a, c):
        return (c,a)
class K(Op):
    def app(self, a, c):
        return (c, K2(a))
class K2(Op):
    def __init__(self, r):
        self.r = r
    def app(self, _, c):
        return (c, self.r)
class S(Op):
    def app(self, a, c):
        return (c, S2(a))
class S2(Op):
    def __init__(self, first):
        self.first = first
    def app(self, a, c):
        return (c, S3(self.first, a))
class S3(Op):
    def __init__(self, first, second):
        self.f = first
        self.s = second
    def app(self, a, c):
        return (descend, P(c,App(App(self.f,a),App(self.s,a))))
class V(Op):
    def app(self, _, c):
        return (c, self)
class E(Op):
    def app(self, a, c):
        raise SystemExit(not isinstance(a, I))
        return (c, a)
class D(Op):
    def app(self, a, c):
        assert False, "This never happens"        
class D2(Op):
    def __init__(self, right):
        self.right = right
    def app(self, rv, k2):
        return (descend, P(D_delayed(rv, k2), self.right))
class Readchar(Op):
    def app(self, a, c):
        curr.c = os.read(0,1)
        if not curr.c: return (descend, P(c, App(a,v)))
        return (descend, P(c, App(a,i)))
class Printchar(I):
    def __init__(self, char):
        self.char = char or ''# convince pypy translation that this is never None
    def app(self, a, c):
        os.write(1, self.char)
        return (c,a)
class R(Printchar):
    def __init__(self):
        Printchar.__init__(self,'\n')
class Compchar(Op):
    def __init__(self, char):
        self.char = char
    def app(self, a, c):
        if self.char == curr.c:
            return (descend, P(c, App(a,i)))
        return (descend, P(c, App(a,V())))
class Reprint(Op):
    def app(self, a, c):
        if not curr.c: return (descend, P(c,App(a,v)))
        return (descend, P(c,App(a, Printchar(curr.c))))
class C(Op):
    def app(self, a, c):
        return (descend, P(c,App(a, Callcc(c))))
class Callcc(Op):
    def __init__(self, cont):
        self.cont = cont
    def app(self, a, _):
        return (self.cont, a)

i = I()
v = V()
def get_pchar(c):
    if c not in pcharmap:
        pcharmap[c] = Printchar(c)
    return pcharmap[c]
pcharmap = {}
def get_cchar(c):
    if c not in ccharmap:
        ccharmap[c] = Compchar(c)
    return ccharmap[c]
ccharmap = {}

class App(Op):
    def __init__(self, left, right):
        self.left = left
        self.right = right


class P(Op):
    '''
A graceless convenience so that instances of Eval (but not of its subclasses!) see a single argument of type Op. (The subclasses also see a single argument of type Op, but they do so without this blatant hackery.)
'''
    def __init__(self, cont, tree):
        self.cont = cont
        self.tree = tree

noarg = {'i':i, 'v':v, 'e':E(), 'd':D(), 's':S(), 'k':K(),
         '|':Reprint(), 'r':R(), '@':Readchar(), 'c':C()}
takesarg = {'.':get_pchar, '?':get_cchar}

def bt(fileno):
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

class Eval(object):
    def __init__(self):
        pass
    def evaluate(self, p):
        tree = p.tree
        cont = p.cont
        if isinstance(tree, App):
            dchecker = Dchecker(tree.right, cont)
            return (self, P(dchecker, tree.left))
        else:
            return cont.evaluate(tree)

class Exiter(Eval):
    def evaluate(self, o):
        raise SystemExit(not isinstance(o, I))

class Dchecker(Eval):
    def __init__(self, right, cont):
        self.right = right
        self.cont = cont
    def evaluate(self, lval):
        if isinstance(lval, D):
            return self.cont.evaluate(D2(self.right))
        else:
            return (descend, P(D_undelayed(lval, self.cont), self.right))

class D_delayed(Eval):
    def __init__(self, rv, k2):
        self.rv = rv
        self.k2 = k2
    def evaluate(self, lv):
        return lv.app(self.rv, self.k2)
    
class D_undelayed(Eval):
    def __init__(self, lval, cont):
        self.lval = lval
        self.cont = cont
    def evaluate(self, rval):
        return self.lval.app(rval, self.cont)

descend = Eval()

def run(argv):
    if len(argv) == 1: fileno = 0
    else: fileno = os.open(argv[1], os.O_RDONLY, 0777)
    tree = bt(fileno)
    k, a = descend, P(Exiter(), tree)
    while not isinstance(k, Exiter):
        k, a = k.evaluate(a)
    if isinstance(a, I): return 0
    return 1

def target(*a):
    return (run, None)

if __name__ == '__main__':
    run(sys.argv)
