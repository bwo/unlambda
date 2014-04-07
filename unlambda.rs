#![feature(macro_rules)]
extern crate collections;
use collections::hashmap::{HashMap};
use std::rc::Rc;
use std::io::File;

#[deriving(Clone)]
enum Term {I, K, K2(Rc<Term>), S, S2(Rc<Term>), S3(Rc<Term>, Rc<Term>), V, E, Readchar, Printchar(char),
           Compchar(char), Reprint, D, D2(Rc<Term>), C, Callcc(Rc<Cont>), App(Rc<Term>, Rc<Term>)
}

#[deriving(Clone)]
enum Cont {Exit, DCheck(Rc<Term>, Rc<Cont>), DDelayed(Rc<Term>, Rc<Cont>), DUndelayed(Rc<Term>, Rc<Cont>)
}

fn eval<B: std::io::Buffer>(k: Rc<Cont>, t: Rc<Term>, c: Option<char>, b: & mut B) -> Result<(Rc<Term>, Rc<Cont>, Option<char>), int>
{
    match *k {
        Exit => Err(0),
        DDelayed(ref right, ref cont) => apply(t, right.clone(), cont.clone(), c, b),
        DUndelayed(ref left, ref cont) => apply(left.clone(), t, cont.clone(), c, b),
        DCheck(ref right, ref cont) => match *t {
            D => eval(cont.clone(), Rc::new(D2(right.clone())), c, b),
            ref x => descend(Rc::new(DUndelayed(Rc::new(x.clone()), cont.clone())), right.clone(), c, b)
        }
    }
}

fn descend<B: std::io::Buffer>(k: Rc<Cont>, t: Rc<Term>, c: Option<char>, b: & mut B) -> Result<(Rc<Term>, Rc<Cont>, Option<char>), int>
{
    match *t {
        App(ref left,ref right) => descend(Rc::new(DCheck(right.clone(), k)), left.clone(), c, b),
        _ => eval(k, t.clone(), c, b)
    }
}

fn apply<B: std::io::Buffer>(s: Rc<Term>, t: Rc<Term>, k: Rc<Cont>, c: Option<char>, b: &mut B) -> Result<(Rc<Term>, Rc<Cont>, Option<char>), int>
{
    match *s {
        I => Ok((t, k, c)),
        K => Ok((Rc::new(K2(t)), k, c)),
        K2(ref a) => Ok((a.clone(), k, c)),
        S => Ok((Rc::new(S2(t)), k, c)),
        S2(ref a) => Ok((Rc::new(S3(a.clone(), t)), k, c)),
        S3(ref a,ref a2) => descend(k, Rc::new(App(Rc::new(App(a.clone(),t.clone())), Rc::new(App(a2.clone(),t)))), c, b),
        V => Ok((Rc::new(V),k,c)),
        E => match *t {
            I => Err(0),
            _ => Err(1)
        },
        D2(ref right) => descend(Rc::new(DDelayed(t, k)), right.clone(), c, b),
        Readchar => match b.read_char() {
            Ok(c) => descend(k, Rc::new(App(t, Rc::new(I))), Some(c), b),
            _     => descend(k, Rc::new(App(t, Rc::new(V))), None, b)
        },
        Printchar(ch) => {
            print!("{:c}",ch);
            Ok((t, k, c))
        },
        Compchar(ch) => {
            let eq = c.map_or(false, |c| { ch == c });
            descend(k, Rc::new(App(t, Rc::new((if eq { I } else { V })))), c, b)
        },
        Reprint => descend(k, Rc::new(App(t, Rc::new(c.map_or(V, Printchar)))), c, b),
        C => descend(k.clone(), Rc::new(App(t, Rc::new(Callcc(k)))), c, b),
        Callcc(ref cont) => Ok((t, cont.clone(), c)),
        // D and App should never happen here.
        _ => Err(-1)
    }
}

enum TermOrApp {
    T(Rc<Term>), A
}

fn getch<B: std::io::Buffer>(reader: &mut B) -> Result<char, std::io::IoError> {
    Ok(std::char::from_u32(try!(reader.read_byte()) as u32).unwrap())
}

fn build<B: std::io::Buffer>(reader: &mut B, m: HashMap<char, Rc<Term>>) -> Result<Rc<Term>, std::io::IoError> {
    let mut tot = 0;
    let mut ticks = 0;
    let mut stack = std::vec::Vec::new();
    let mut tree = std::vec::Vec::new();
    let mut cur;
    while tot != 2*ticks + 1 {
        cur = try!(getch(reader));
        tot = tot + 1;
        match (cur, m.find(&cur).map(|t| t.clone() )) {
            ('#',None) => { try!(reader.read_line()); tot = tot - 1;},
            ('`', None) => { ticks = ticks + 1; stack.push(A) },
            (_,Some(t)) => stack.push(T(t)),
            ('.',_) => stack.push(T(Rc::new(Printchar(try!(getch(reader)))))),
            ('?',_) => stack.push(T(Rc::new(Compchar(try!(getch(reader)))))),
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
                tree.push(Rc::new(App(left,right)))
            }
            T(t) => tree.push(t)
        }
    }
    Ok(match tree.pop() { Some(s) => s , None => unreachable!() })
}

fn run<B: std::io::Buffer>(t : Rc<Term>, b: &mut B) -> int
{
    let mut start = descend(Rc::new(Exit), t, None, b);
    let mut exitcode;
    loop {
        match start {
            Err(i) => {
                exitcode = i;
                break;
            },
            Ok((term, cont, c)) => {
                start = eval(cont, term, c, b)
            }
        }
    }
    exitcode
}


macro_rules! hashmap (
    ($($key:expr => $val:expr),*) => ( {
        let mut m = HashMap::new();
        ($(m.insert($key, $val)),*);
        m
    })
)

fn main() {
    let args = std::os::args();
    let p = hashmap!('i' => Rc::new(I),
                     'v' => Rc::new(V),
                     'e' => Rc::new(E),
                     'k' => Rc::new(K),
                     's' => Rc::new(S),
                     '|' => Rc::new(Reprint),
                     '@' => Rc::new(Readchar),
                     'c' => Rc::new(C),
                     'r' => Rc::new(Printchar('\n')),
                     'd' => Rc::new(D));
    let mut stdin = std::io::stdin();
    let res = match args.get(1) {
        None => build(& mut stdin, p),
        Some(s) => match File::open(&Path::new(s.clone())) {
            Ok(h) => build(& mut std::io::BufferedReader::new(h), p),
            Err(_) => build(& mut stdin, p)
        }
    };
    std::os::set_exit_status(match res {
        Err(e) => {
            println!("{}",e); 
            1
        },
        Ok(r) => run(r, &mut stdin) 
    }); 
}
