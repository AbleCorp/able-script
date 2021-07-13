//! Number constants.

use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::variables::{Value, Variable};

/// Initialize a HashMap between the constant names and values
/// accessible from within AbleScript.
pub fn ablescript_consts() -> HashMap<String, Variable> {
    use Value::*;

    let mut map = HashMap::new();
    for (name, value) in &[
        ("TAU", Int(6)),                // Circumference / radius
        ("PI", Int(3)),                 // Deprecated, do not use
        ("EULER", Int(3)),              // Mathematical constant e
        ("MASS", Int(70)),              // @Kev#6900's weight in kilograms
        ("PHI", Int(2)),                // Golden ratio
        ("WUA", Int(1)),                // 1
        ("EULERS_CONSTANT", Int(0)),    // ???
        ("GRAVITY", Int(10)),           // Earth surface gravity, m/s
        ("RNG", Int(12)),               // Kixiron#5289 Randomly rolled dice
        ("STD_RNG", Int(4)),            // The standard random number is 4 (https://xkcd.com/221/)
        ("INF", Int(i32::max_value())), // The biggest number
        ("INTERESSANT", Int(114514)),   // HTGAzureX1212.#5959 intéressant number
        ("FUNNY", Int(69)),             // HTGAzureX1212.#5959 funny number
        (
            // Never gonna let you down
            "NEVERGONNAGIVEYOUUP",
            Str("1452251871514141792252515212116".to_owned()),
        ),
        ("OCTOTHORPE", Str("#".to_owned())), // It's an octothorpe
        ("ANSWER", Int(ANSWER)),
    ] {
        map.insert(
            (*name).to_owned(),
            Variable {
                melo: false,
                value: Rc::new(RefCell::new(value.to_owned())),
            },
        );
    }

    map
}

pub const ANSWER: i32 = 42;
