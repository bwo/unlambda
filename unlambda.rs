use std::collections::HashMap;
use std::env;
use std::process::exit;
use std::rc::Rc;
use std::io::{self, BufRead, Bytes, Read};
use std::fs::File;
use std::path::Path;

#[derive(Clone)]
enum Term {
    I, K, K2(Rc<Term>), S, S2(Rc<Term>), S3(Rc<Term>, Rc<Term>), V, E, Readchar, Printchar(char),
    Compchar(char), Reprint, D, D2(Rc<Term>), C, Callcc(Rc<Cont>), App(Rc<Term>, Rc<Term>)
}

#[derive(Clone)]
enum Cont {
    Exit, DCheck(Rc<Term>, Rc<Cont>), DDelayed(Rc<Term>, Rc<Cont>), DUndelayed(Rc<Term>, Rc<Cont>)
}

fn eval<B: Read>(k: Rc<Cont>, t: Rc<Term>, c: Option<char>, b: &mut Bytes<B>) -> Result<(Rc<Term>, Rc<Cont>, Option<char>), i32>
{
    match *k {
        Cont::Exit => Err(0),
        Cont::DDelayed(ref right, ref cont) => apply(t, right.clone(), cont.clone(), c, b),
        Cont::DUndelayed(ref left, ref cont) => apply(left.clone(), t, cont.clone(), c, b),
        Cont::DCheck(ref right, ref cont) => match *t {
            Term::D => eval(cont.clone(), Rc::new(Term::D2(right.clone())), c, b),
            ref x => descend(Rc::new(Cont::DUndelayed(Rc::new(x.clone()), cont.clone())), right.clone(), c, b)
        }
    }
}

fn descend<B: Read>(k: Rc<Cont>, t: Rc<Term>, c: Option<char>, b: &mut Bytes<B>) -> Result<(Rc<Term>, Rc<Cont>, Option<char>), i32>
{
    match *t {
        Term::App(ref left,ref right) => descend(Rc::new(Cont::DCheck(right.clone(), k)), left.clone(), c, b),
        _ => eval(k, t.clone(), c, b)
    }
}

fn apply<B: Read>(s: Rc<Term>, t: Rc<Term>, k: Rc<Cont>, c: Option<char>, b: &mut Bytes<B>) -> Result<(Rc<Term>, Rc<Cont>, Option<char>), i32>
{
    match *s {
        Term::I => Ok((t, k, c)),
        Term::K => Ok((Rc::new(Term::K2(t)), k, c)),
        Term::K2(ref a) => Ok((a.clone(), k, c)),
        Term::S => Ok((Rc::new(Term::S2(t)), k, c)),
        Term::S2(ref a) => Ok((Rc::new(Term::S3(a.clone(), t)), k, c)),
        Term::S3(ref a,ref a2) => descend(k, Rc::new(Term::App(Rc::new(Term::App(a.clone(),t.clone())), Rc::new(Term::App(a2.clone(),t)))), c, b),
        Term::V => Ok((Rc::new(Term::V),k,c)),
        Term::E => Err(match *t {
            Term::I => 0,
            _ => 1
        }),
        Term::D2(ref right) => descend(Rc::new(Cont::DDelayed(t, k)), right.clone(), c, b),
        Term::Readchar => match b.next() {
            Some(Ok(c)) => descend(k, Rc::new(Term::App(t, Rc::new(Term::I))), Some(c as char), b),
            _     => descend(k, Rc::new(Term::App(t, Rc::new(Term::V))), None, b)
        },
        Term::Printchar(ch) => {
            print!("{}",ch);
            Ok((t, k, c))
        },
        Term::Compchar(ch) => {
            let eq = c.map_or(false, |c| ch == c);
            descend(k, Rc::new(Term::App(t, Rc::new((if eq { Term::I } else { Term::V })))), c, b)
        },
        Term::Reprint => descend(k, Rc::new(Term::App(t, Rc::new(c.map_or(Term::V, Term::Printchar)))), c, b),
        Term::C => descend(k.clone(), Rc::new(Term::App(t, Rc::new(Term::Callcc(k)))), c, b),
        Term::Callcc(ref cont) => Ok((t, cont.clone(), c)),
        // D and App should never happen here.
        _ => Err(-1)
    }
}

enum TermOrApp {
    T(Rc<Term>), A
}

fn getch<B: BufRead>(reader: &mut B) -> Result<char, io::Error> {
    if let Some(b) = reader.bytes().next() {
        b.map(|b| b as char)
    } else {
        Err(io::Error::new(io::ErrorKind::UnexpectedEof, "getch failed"))
    }
}

fn build<B: BufRead>(reader: &mut B, m: HashMap<char, Rc<Term>>) -> Result<Rc<Term>, io::Error> {
    let mut tot = 0;
    let mut ticks = 0;
    let mut stack = Vec::new();
    let mut tree = Vec::new();
    while tot != 2*ticks + 1 {
        let cur = try!(getch(reader));
        tot = tot + 1;
        match (cur, m.get(&cur).map(|t| t.clone() )) {
            ('#',None) => { try!(reader.read_line(&mut String::new())); tot = tot - 1;},
            ('`', None) => { ticks = ticks + 1; stack.push(TermOrApp::A) },
            (_,Some(t)) => stack.push(TermOrApp::T(t)),
            ('.',_) => stack.push(TermOrApp::T(Rc::new(Term::Printchar(try!(getch(reader)))))),
            ('?',_) => stack.push(TermOrApp::T(Rc::new(Term::Compchar(try!(getch(reader)))))),
            _ => tot = tot - 1
        }
    };
    while let Some(curterm) = stack.pop() {
        match curterm {
            TermOrApp::A => {
                let left = tree.pop().unwrap();
                let right = tree.pop().unwrap();
                tree.push(Rc::new(Term::App(left,right)))
            }
            TermOrApp::T(t) => tree.push(t)
        }
    }
    // can't replace this with tree.pop().unwrap(); type can't be determined!
    Ok(tree.pop().unwrap())
}

fn run<B: Read>(t : Rc<Term>, mut b: Bytes<B>) -> i32
{
    let mut start = descend(Rc::new(Cont::Exit), t, None, &mut b);
    loop {
        match start {
            Err(i) => {
                return i;
            },
            Ok((term, cont, c)) => {
                start = eval(cont, term, c, &mut b)
            }
        }
    }
}


macro_rules! hashmap {
    ($($key:expr => $val:expr),*) => ( {
        let mut m = HashMap::new();
        ($(m.insert($key, $val)),*);
        m
    })
}

fn main() {
    let p = hashmap!{'i' => Rc::new(Term::I),
                     'v' => Rc::new(Term::V),
                     'e' => Rc::new(Term::E),
                     'k' => Rc::new(Term::K),
                     's' => Rc::new(Term::S),
                     '|' => Rc::new(Term::Reprint),
                     '@' => Rc::new(Term::Readchar),
                     'c' => Rc::new(Term::C),
                     'r' => Rc::new(Term::Printchar('\n')),
                     'd' => Rc::new(Term::D)};
    let stdin_root = io::stdin();
    let mut stdin = stdin_root.lock();
    let res = match env::args().nth(1).and_then(|s| File::open(&Path::new(&s)).ok()) {
        None => build(&mut stdin, p),
        Some(h) => build(&mut io::BufReader::new(h), p),
    };
    exit(match res {
        Err(e) => {
            println!("{}",e); 
            1
        },
        Ok(r) => run(r, stdin.bytes())
    }); 
}
