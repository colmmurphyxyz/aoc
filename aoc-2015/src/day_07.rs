use regex::Regex;
use std::collections::{HashMap, VecDeque};

use crate::util::fs_utils::read_input_lines;

struct Vm {
    environment: HashMap<String, u16>,
}

impl Vm {
    pub fn is_assigned(&self, variable: &str) -> bool {
        is_num(variable) || self.environment.contains_key(variable)
    }

    pub fn get_value(&self, variable: &str) -> u16 {
        if is_num(variable) {
            return variable.parse::<u16>().unwrap();
        }
        self.environment[variable]
    }

    pub fn set_value(&mut self, variable: &str, value: u16) {
        self.environment.insert(variable.to_string(), value);
    }
}

type VmPredicate = Box<dyn Fn(&Vm) -> bool>;
type VmAction = Box<dyn Fn(&mut Vm)>;

struct Operation {
    can_exec: VmPredicate,
    exec: VmAction,
}

fn is_num(s: &str) -> bool {
    s.chars().all(|f| f.is_numeric())
}

fn parse_line(line: String) -> Operation {
    let xs: Vec<&str> = line.split(' ').collect();
    if xs[0] == "NOT" {
        // NOT a -> b
        let operand = xs[1].to_string();
        let destination = xs[3].to_string();
        return Operation {
            can_exec: {
                let operand = operand.clone();
                Box::new(move |vm| vm.is_assigned(&operand))
            },
            exec: Box::new(move |vm| {
                let x = vm.get_value(&operand);
                vm.set_value(&destination, !x);
            }),
        };
    } else if xs[1] == "->" {
        // assignment operation. a -> b sort of thing
        let value = xs[0];
        let dest = xs[2];
        return Operation {
            can_exec: {
                let value = value.to_string();
                Box::new(move |vm| vm.is_assigned(&value))
            },
            exec: {
                let dest = dest.to_string();
                let value = value.to_string();
                Box::new(move |vm| {
                    let new_value = vm.get_value(&value);
                    vm.set_value(&dest, new_value);
                })
            },
        };
    }

    // expect binary operation e.g. lhs OP rhs -> dest
    let lhs = xs[0];
    let op = xs[1];
    let rhs = xs[2];
    let dest = xs[4];

    Operation {
        can_exec: {
            let lhs = lhs.to_string();
            let rhs = rhs.to_string();
            Box::new(move |vm| vm.is_assigned(&lhs) && vm.is_assigned(&rhs))
        },
        exec: {
            let lhs = lhs.to_string();
            let op = op.to_string();
            let rhs = rhs.to_string();
            let dest = dest.to_string();
            Box::new(move |vm| {
                let res = match op.as_str() {
                    "AND" => vm.get_value(&lhs) & vm.get_value(&rhs),
                    "OR" => vm.get_value(&lhs) | vm.get_value(&rhs),
                    "LSHIFT" => vm.get_value(&lhs) << vm.get_value(&rhs),
                    "RSHIFT" => vm.get_value(&lhs) >> vm.get_value(&rhs),
                    _ => panic!("Unsupported operation"),
                };

                vm.set_value(&dest, res);
            })
        },
    }
}

pub fn day07_a() -> i64 {
    let mut queue: VecDeque<Operation> = VecDeque::from_iter(
        read_input_lines("Day07")
            .iter()
            .map(|x| parse_line(x.to_string())),
    );
    let mut vm = Vm {
        environment: HashMap::new(),
    };
    while !queue.is_empty() {
        let next_op = queue.pop_front().unwrap();
        if (next_op.can_exec)(&vm) {
            (next_op.exec)(&mut vm)
        } else {
            queue.push_back(next_op);
        }
    }

    let final_value = vm.get_value("a");
    return final_value.into();
}

pub fn day07_b() -> i64 {
    let initial_b = day07_a() as u16;
    let mut vm = Vm {
        environment: HashMap::new(),
    };

    vm.environment.insert(String::from("b"), initial_b);
    let excluded_ops = Regex::new(r"-> b$").unwrap();
    let mut queue: VecDeque<Operation> = VecDeque::from_iter(
        read_input_lines("Day07")
            .iter()
            // filter out that pesky instruction that re-assigns b
            .filter(|line| !excluded_ops.is_match(line))
            .map(|x| parse_line(x.to_string())),
    );

    while !queue.is_empty() {
        let next_op = queue.pop_front().unwrap();
        if (next_op.can_exec)(&vm) {
            (next_op.exec)(&mut vm)
        } else {
            queue.push_back(next_op);
        }
    }

    vm.get_value("a").into()
}
