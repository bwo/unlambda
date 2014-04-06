use std::rc::Rc;

#[deriving(Clone)]
enum Term {I, K, K2(Rc<Term>), S, S2(Rc<Term>), S3(Rc<Term>, Rc<Term>), V, E, Readchar, Printchar(char),
           Compchar(char), Reprint, D, D2(Rc<Term>), C, Callcc(Rc<Cont>), App(Rc<Term>, Rc<Term>)
}

#[deriving(Clone)]
enum Cont {Exit, DCheck(Rc<Term>, Rc<Cont>), DDelayed(Rc<Term>, Rc<Cont>), DUndelayed(Rc<Term>, Rc<Cont>)
}

fn eval(k: Rc<Cont>, t: Rc<Term>, c: Option<char>) -> Result<(Rc<Term>, Rc<Cont>, Option<char>), int>
{
    match *k {
        Exit => Err(0),
        DDelayed(ref right, ref cont) => apply(t, right.clone(), cont.clone(), c),
        DUndelayed(ref left, ref cont) => apply(left.clone(), t, cont.clone(), c),
        DCheck(ref right, ref cont) => match *t {
            D => eval(cont.clone(), Rc::new(D2(right.clone())), c),
            ref x => descend(Rc::new(DUndelayed(Rc::new(x.clone()), cont.clone())), right.clone(), c)
        }
    }
}

fn descend(k: Rc<Cont>, t: Rc<Term>, c: Option<char>) -> Result<(Rc<Term>, Rc<Cont>, Option<char>), int>
{
    match *t {
        App(ref left,ref right) => descend(Rc::new(DCheck(right.clone(), k)), left.clone(), c),
        _ => eval(k, t.clone(), c)
    }
}

fn apply(s: Rc<Term>, t: Rc<Term>, k: Rc<Cont>, c: Option<char>) -> Result<(Rc<Term>, Rc<Cont>, Option<char>), int>
{
    match *s {
        I => Ok((t, k, c)),
        K => Ok((Rc::new(K2(t)), k, c)),
        K2(ref a) => Ok((a.clone(), k, c)),
        S => Ok((Rc::new(S2(t)), k, c)),
        S2(ref a) => Ok((Rc::new(S3(a.clone(), t)), k, c)),
        S3(ref a,ref a2) => descend(k, Rc::new(App(Rc::new(App(a.clone(),t.clone())), Rc::new(App(a2.clone(),t)))), c),
        V => Ok((Rc::new(V),k,c)),
        E => match *t {
            I => Err(0),
            _ => Err(1)
        },
        D2(ref right) => descend(Rc::new(DDelayed(t, k)), right.clone(), c),
        Readchar => match std::io::stdin().read_char() {
            Ok(c) => descend(k, Rc::new(App(t, Rc::new(I))), Some(c)),
            _     => descend(k, Rc::new(App(t, Rc::new(V))), None)
        },
        Printchar(ch) => {
            print!("{:c}",ch);
            Ok((t, k, c))
        },
        Compchar(ch) => {
            let eq = c.map_or(false, |c| { ch == c });
            descend(k, Rc::new(App(t, Rc::new((if eq { I } else { V })))), c)
        },
        Reprint => descend(k, Rc::new(App(t, Rc::new(c.map_or(V, Printchar)))), c),
        C => descend(k.clone(), Rc::new(App(t, Rc::new(Callcc(k)))), c),
        Callcc(ref cont) => Ok((t, cont.clone(), c)),
        // D and App should never happen here.
        _ => Err(-1)
    }
}

fn noarg(ch: char) -> Option<Term> {
    match ch {
        '|' => Some(Reprint), 's'|'S' => Some(S), 'i'|'I' => Some(I),
        'k'|'K' => Some(K), 'c'|'C' => Some(C), 'e'|'E' => Some(E),
        'v'|'V' => Some(V), 'r'|'R' => Some(Printchar('\n')),
        'd'|'D' => Some(D), '@' => Some(Readchar),
        _ => None
    }
}

enum TermOrApp {
    T(Term), A
}

fn build<B: std::io::Buffer>(reader: &mut B) -> Result<Term, std::io::IoError> {
    let mut tot = 0;
    let mut ticks = 0;
    let mut stack = std::vec::Vec::new();
    let mut tree = std::vec::Vec::new();
    let mut cur;
    while tot != 2*ticks + 1 {
        cur = try!(reader.read_char());
        tot = tot + 1;
        match (cur,noarg(cur)) {
            ('#',None) => { try!(reader.read_line()); tot = tot - 1;},
            ('`', None) => { ticks = ticks + 1; stack.push(A) },
            (_,Some(t)) => stack.push(T(t)),
            ('.',_) => stack.push(T(Printchar(try!(reader.read_char())))),
            ('?',_) => stack.push(T(Compchar(try!(reader.read_char())))),
            _ => tot = tot - 1
        }
    };
    let mut curterm;
    while !stack.is_empty() {
        curterm = match stack.pop() { Some(s) => s, None => unreachable!() };
        match curterm {
            A => {
                let left = match tree.pop() {
                    Some(l) => l,
                    None => unreachable!()
                };
                let right = match tree.pop() {
                    Some(r) => r,
                    None => unreachable!()
                };
                tree.push(App(Rc::new(left),Rc::new(right)))
            }
            T(t) => tree.push(t)
        }
    }
    Ok(match tree.pop() { Some(s) => s , None => unreachable!() })
}

fn run(t : Term) -> int
{
    let mut start = descend(Rc::new(Exit), Rc::new(t), None);
    let mut exitcode;
    loop {
        match start {
            Err(i) => {
                exitcode = i;
                break;
            },
            Ok((term, cont, c)) => {
                start = eval(cont, term, c)
            }
        }
    }
    exitcode
}

fn main() {
    match build(&mut std::io::stdin()) {
        Err(e) => (),
        Ok(r) => println!("{}", run(r))
    };
}
