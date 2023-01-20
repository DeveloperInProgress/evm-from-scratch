/**
 * EVM From Scratch
 * Rust template
 *
 * To work on EVM From Scratch in Rust:
 *
 * - Install Rust: https://www.rust-lang.org/tools/install
 * - Edit `rust/lib.rs`
 * - Run `cd rust && cargo run` to run the tests
 *
 * Hint: most people who were trying to learn Rust and EVM at the same
 * gave up and switched to JavaScript, Python, or Go. If you are new
 * to Rust, implement EVM in another programming language first.
 */

use evm::{evm, EvmTx, EvmBlock, StateData, EvmLog};
use primitive_types::U256;
use serde::Deserialize;
use std::{collections::HashMap, borrow::BorrowMut};


#[derive(Debug, Deserialize)]
struct Evmtest {
    name: String,
    hint: String,
    code: Code,
    expect: Expect,
    tx: Option<EvmTx>,
    block: Option<EvmBlock>,
    state: Option<HashMap<String, StateData>>,
}

#[derive(Debug, Deserialize)]
struct Code {
    asm: String,
    bin: String,
}

#[derive(Debug, Deserialize)]
struct Expect {
    stack: Option<Vec<String>>,
    logs: Option<Vec<EvmLog>>,
    success: bool,
    returns: Option<String>
    // #[serde(rename = "return")]
    // ret: Option<String>,
}


fn main() {
    let text = std::fs::read_to_string("../evm.json").unwrap();
    let data: Vec<Evmtest> = serde_json::from_str(&text).unwrap();

    let total = data.len();

    for (index, test) in data.iter().enumerate() {
        println!("Test {} of {}: {}", index + 1, total, test.name);

        let code: Vec<u8> = hex::decode(&test.code.bin).unwrap();
        let tx1 = &(test.tx.clone().unwrap_or_default());
        let block1 = &(test.block.clone().unwrap_or_default());
        let mut state = test.state.clone().unwrap_or_default();
        let result = evm(&code, tx1, block1, &mut state, false.try_into().unwrap());

        let mut expected_stack: Vec<U256> = Vec::new();
        if let Some(ref stacks) = test.expect.stack {
            for value in stacks {
                expected_stack.push(U256::from_str_radix(value, 16).unwrap());
            }
        }

        let mut matching = result.stack.len() == expected_stack.len();
        if matching {
            for i in 0..result.stack.len() {
                if result.stack[i] != expected_stack[i] {
                    matching = false;
                    break;
                }
            }
        }

        let mut expected_log: Vec<EvmLog> = Vec::new();
        if let Some(ref logs) = test.expect.logs {
            for value in logs {
                expected_log.push(value.clone());
            }
        }
        matching = matching && result.logs.len() == expected_log.len();
        if matching {
            for i in 0..result.logs.len() {
                if result.logs[i].address != expected_log[i].address {
                    matching = false;
                    break;
                }
                if result.logs[i].data != expected_log[i].data {
                    matching = false;
                    break;
                }
                if result.logs[i].topics.len() != expected_log[i].topics.len() {
                    matching = false;
                    break;
                }
                for j in 0..result.logs[i].topics.len() {
                    if result.logs[i].topics[j] != expected_log[i].topics[j] {
                        matching = false;
                        break;
                    }
                }
                if !matching {break;}
            }
        }
        if !test.expect.returns.is_none() {
            matching = matching && result.returns == test.expect.returns.clone().unwrap_or_default();
        }
        matching = matching && result.success == test.expect.success;

        if !matching {
            println!("Instructions: \n{}\n", test.code.asm);

            println!("Expected success: {:?}", test.expect.success);
            println!("Expected stack: [");
            for v in expected_stack {
                println!("  {:#X},", v);
            }
            println!("]\n");
            println!("Expected log: [");
            for v in expected_log {
                println!("  {:?},", v);
            }
            println!("]\n");
            
            println!("Actual success: {:?}", result.success);
            println!("Actual stack: [");
            for v in result.stack {
                println!("  {:#X},", v);
            }
            println!("]\n");
            println!("Actual log: [");
            for v in result.logs {
                println!("  {:?},", v);
            }
            println!("]\n");

            println!("\nHint: {}\n", test.hint);
            println!("Progress: {}/{}\n\n", index, total);
            panic!("Test failed");
        }
        println!("PASS");
    }
    println!("Congratulations!");
}
