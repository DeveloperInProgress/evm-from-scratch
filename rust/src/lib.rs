use std::borrow::BorrowMut;
use sha3::{Digest, Keccak256};
use hex::ToHex;
use primitive_types::U256;
use primitive_types::U512;
use serde::Deserialize;
use std::default::Default;
use std::collections::HashMap;

#[derive(Debug, Deserialize, Default, Clone)]
pub struct EvmLog {
    pub address: String,
    pub data: String,
    pub topics: Vec<String>
}

pub struct EvmResult {
    pub stack: Vec<U256>,
    pub success: bool,
    pub logs: Vec<EvmLog>,
    pub returns: String
}

#[derive(Debug, Deserialize, Default, Clone)]
pub struct EvmTx {
    pub to: Option<String>,
    pub from: Option<String>,
    pub origin: Option<String>,
    pub gasprice: Option<String>,
    pub value: Option<String>,
    pub data: Option<String>,
} 

#[derive(Debug, Deserialize, Default,  Clone)]
pub struct EvmCode {
    pub asm: Option<String>,
    pub bin: Option<String>,
}

#[derive(Debug, Deserialize, Default, Clone)]
pub struct StateData {
    pub balance: Option<String>,
    pub code: Option<EvmCode>,
    pub storage: Option<HashMap<String, Vec<u8>>>
}

#[derive(Debug, Deserialize, Default, Clone)]
pub struct EvmBlock {
    pub basefee: Option<String>,
    pub coinbase: Option<String>,
    pub timestamp: Option<String>,
    pub number: Option<String>,
    pub difficulty: Option<String>,
    pub gaslimit: Option<String>,
    pub chainid: Option<String>,
}

fn u256_to_hex(val: U256) -> String {
    let val_bytes = &mut [0 as u8; 32];
    val.to_big_endian(val_bytes);
    let mut val_hex = hex::encode(val_bytes).trim_start_matches('0').to_string();
    val_hex.insert_str(0, "0x");
    return  val_hex;
}

pub fn evm(_code: impl AsRef<[u8]>, tx: &EvmTx, block: &EvmBlock, state: &mut HashMap<String, StateData>, isStatic: Option<bool>) -> EvmResult {
    let mut stack: Vec<U256> = Vec::with_capacity(1024);
    let mut logs: Vec<EvmLog> = Vec::new();
    let mut memory: Vec<u8> = Vec::with_capacity(4 * 1024);
    let mut pc: usize = 0;
    let mut returns: String = "".to_string();
    let _isStatic = isStatic.unwrap_or(false);
    let code: &[u8] = _code.as_ref();
    //let tx: &EvmTx = _tx.as_ref();

    let _address = tx.to.clone().unwrap_or_default();

    if state.get(&_address).is_none() {
        let state_data = StateData {
            balance: "0x0".to_string().try_into().unwrap(),
            code: None,
            storage: HashMap::new().try_into().unwrap()
        };
        state.insert(_address.clone(), state_data);
    } 

    let mut account_state = state.get(&_address).unwrap();

    if !tx.value.is_none() {
        let mut balance = U256::from_str_radix(account_state.balance.clone().unwrap_or_default().as_str(), 16).unwrap();
        balance += U256::from_str_radix(tx.value.clone().unwrap().as_str(), 16).unwrap();
        state.entry(_address).and_modify(|s| s.balance = u256_to_hex(balance).try_into().unwrap());
    }

    while pc < code.len() {
        let opcode = code[pc];
        pc += 1;

        if opcode == 0x00 {
            // STOP
            return EvmResult {
                stack: stack,
                success: true,
                logs: logs,
                returns: "0x0".to_string()
            };
        } else if opcode == 0x01 {
            // ADD
            let op1 = stack.remove(0);
            let op2 = stack.remove(0);
            stack.insert(0,op1.overflowing_add(op2).0);
        } else if opcode == 0x02 {
            //MUL
            let op1 = stack.remove(0);
            let op2 = stack.remove(0);
            stack.insert(0,op2.overflowing_mul(op1).0);
        } else if opcode == 0x03 {
            //SUB
            let op1 = stack.remove(0);
            let op2 = stack.remove(0);
            stack.insert(0,op1.overflowing_sub(op2).0);
        } else if opcode == 0x04 {
            // DIV
            let op1 = stack.remove(0);
            let op2 = stack.remove(0);
            stack.insert(0,op1.checked_div(op2).unwrap_or_default());
        } else if opcode == 0x05 {
            // SDIV
            let mut op1 = stack.remove(0);
            let mut op2 = stack.remove(0);
            
            let op1sign;
            let op2sign;

            if !op1.bit(255) {
                op1sign = if op1.is_zero() {0} else {1};
            } else {
                op1sign = -1;
                op1 = (!op1) + 1;
            }

            if !op2.bit(255) {
                op2sign = if op2.is_zero() {0} else {1};
            } else {
                op2sign = -1;
                op2 = (!op2) + 1;
            }

            let mut res = op1.checked_div(op2).unwrap_or_default();
            if op1sign * op2sign == -1 {
                res = !res + 1;
            }

            stack.insert(0,res);
        } else if opcode == 0x06 {
            // MOD
            let op1 = stack.remove(0);
            let op2 = stack.remove(0);
            stack.insert(0,op1.checked_rem(op2).unwrap_or_default());
        } else if opcode == 0x07 {
            // SMOD
            let mut op1 = stack.remove(0);
            let mut op2 = stack.remove(0);
            
            let op1sign;
            let op2sign;

            if !op1.bit(255) {
                op1sign = if op1.is_zero() {0} else {1};
            } else {
                op1sign = -1;
                op1 = (!op1).overflowing_add(U256::one()).0;
            }

            if !op2.bit(255) {
                op2sign = if op2.is_zero() {0} else {1};
            } else {
                op2sign = -1;
                op2 = (!op2).overflowing_add(U256::from(1)).0;
            }

            let mut res = op1.checked_rem(op2).unwrap_or_default();
            if op1sign == -1 {
                res = (!res).overflowing_add(U256::from(1)).0;
            }

            stack.insert(0,res);
        } else if opcode == 0x08 {
            // ADDMOD
            let op1 = stack.remove(0);
            let op2 = stack.remove(0);
            let N = stack.remove(0);
            stack.insert(0,(op1.overflowing_add(op2).0).checked_rem(N).unwrap_or_default());
        } else if opcode == 0x09 {
            // MULMOD
            let op1 = stack.remove(0);
            let op2 = stack.remove(0);
            let N = stack.remove(0);
            let op1mod = op1.checked_rem(N).unwrap_or_default();
            let op2mod = op2.checked_rem(N).unwrap_or_default();
            stack.insert(0, op1mod.overflowing_mul(op2mod).0);
        } else if opcode == 0x0A {
            // EXP
            let op = stack.remove(0);
            let exp = stack.remove(0);
            stack.insert(0,op.pow(exp));
        } else if opcode == 0x0B {
            // SIGNEXTEND
            let size = stack.remove(0);
            let value = stack.remove(0);
            if size < U256::from(32) {
                let bit_index = (8*size.as_usize() + 7) as usize;
                let bit = value.bit(bit_index);
                let mask = (U256::from(1) << bit_index) - U256::from(1);
                let res = if bit { value | !mask } else { value & mask};
                stack.insert(0, res); 
            }
        } else if opcode == 0x10 {
            // LT
            let op1 = stack.remove(0);
            let op2 = stack.remove(0);
            stack.insert(0,if op1 < op2 {U256::one()} else {U256::zero()});
        } else if opcode == 0x11 {
            // GT
            let op1 = stack.remove(0);
            let op2 = stack.remove(0);
            stack.insert(0,if op1 > op2 {U256::one()} else {U256::zero()});
        } else if opcode == 0x12 {
            // SLT
            let mut op1 = stack.remove(0);
            let mut op2 = stack.remove(0);
            
            let op1sign;
            let op2sign;

            if !op1.bit(255) {
                op1sign = if op1.is_zero() {0} else {1};
            } else {
                op1sign = -1;
                op1 = (!op1).overflowing_add(U256::one()).0;
            }

            if !op2.bit(255) {
                op2sign = if op2.is_zero() {0} else {1};
            } else {
                op2sign = -1;
                op2 = (!op2).overflowing_add(U256::from(1)).0;
            }

            if op1sign == -1 && op2sign != -1 {
                stack.insert(0, U256::one());
            } else if op1sign != -1 && op2sign == -1 {
                stack.insert(0, U256::zero());
            } else if op1sign == -1 && op2sign == -1 {
                stack.insert(0, if op1 > op2 {U256::one()} else {U256::zero()});
            } else {
                stack.insert(0, if op1 < op2 {U256::one()} else {U256::zero()});
            }
        
        } else if opcode == 0x13 {
            // SGT
            let mut op1 = stack.remove(0);
            let mut op2 = stack.remove(0);
            
            let op1sign;
            let op2sign;

            if !op1.bit(255) {
                op1sign = if op1.is_zero() {0} else {1};
            } else {
                op1sign = -1;
                op1 = (!op1).overflowing_add(U256::one()).0;
            }

            if !op2.bit(255) {
                op2sign = if op2.is_zero() {0} else {1};
            } else {
                op2sign = -1;
                op2 = (!op2).overflowing_add(U256::from(1)).0;
            }

            if op1sign == -1 && op2sign != -1 {
                stack.insert(0, U256::zero());
            } else if op1sign != -1 && op2sign == -1 {
                stack.insert(0, U256::one());
            } else if op1sign == -1 && op2sign == -1 {
                stack.insert(0, if op1 < op2 {U256::one()} else {U256::zero()});
            } else {
                stack.insert(0, if op1 > op2 {U256::one()} else {U256::zero()});
            }
        } else if opcode == 0x14 {
            // EQ
            let op1 = stack.remove(0);
            let op2 = stack.remove(0);
            stack.insert(0,if op1 == op2 {U256::one()} else {U256::zero()});
        } else if opcode == 0x15 {
            // ISZERO
            let op1 = stack.remove(0);
            stack.insert(0,if op1.is_zero() {U256::one()} else {U256::zero()});
        } else if opcode == 0x16 {
            // AND
            let op1 = stack.remove(0);
            let op2 = stack.remove(0);
            stack.insert(0,op1 & op2);
        } else if opcode == 0x17 {
            // OR
            let op1 = stack.remove(0);
            let op2 = stack.remove(0);
            stack.insert(0,op1 | op2);
        } else if opcode == 0x18 {
            // XOR
            let op1 = stack.remove(0);
            let op2 = stack.remove(0);
            stack.insert(0,op1 ^ op2);
        } else if opcode == 0x19 {
            // NOT
            let op1 = stack.remove(0);
            stack.insert(0,!op1);
        } else if opcode == 0x1A {
            // BYTE
            let pos = stack.remove(0);
            let value = stack.remove(0);
            let mut ret = U256::zero();
            for i in 0..256 {
                if i < 8 && pos < U256::from(32) {
                    let o = pos.as_usize();
                    let t = 255 - (7 - i + 8 * o);
                    let bit_mask = U256::from(1) << t;
                    let val = (value & bit_mask) >> t;
                    ret = ret.overflowing_add(val << i).0;
                }
            }
            stack.insert(0,U256::from(ret));
        } else if opcode == 0x1B {
            // SHL
            let op1 = stack.remove(0);
            let op2 = stack.remove(0);

            stack.insert(0,op2 << op1);
        } else if opcode == 0x1C {
            // SHR
            let op1 = stack.remove(0);
            let op2 = stack.remove(0);
            stack.insert(0,op2 >> op1);
        } else if opcode == 0x1D {
            // SAR
            let mut op1 = stack.remove(0);
            let mut op2 = stack.remove(0);
            
            let op1sign;
            let op2sign;

            if !op1.bit(255) {
                op1sign = if op1.is_zero() {0} else {1};
            } else {
                op1sign = -1;
                op1 = (!op1).overflowing_add(U256::one()).0;
            }

            if !op2.bit(255) {
                op2sign = if op2.is_zero() {0} else {1};
            } else {
                op2sign = -1;
                op2 = (!op2).overflowing_add(U256::from(1)).0;
            }

            let result = if op2 == U256::zero() || op1 >= U256::from(256) {
                match op2sign {
                    0 | 1 => U256::zero(),
                    -1 => (!U256::one()).overflowing_add(U256::one()).0,
                    _ => U256::zero(),
                }
            } else {
                let shift = usize::try_from(op1).unwrap();
                match op2sign {
                    0 | 1 => op2 >> shift,
                    -1 => {
                        let shifted = ((op2.overflowing_sub(U256::one()).0) >> shift)
                            .overflowing_add(U256::one())
                            .0;
                        (!shifted).overflowing_add(U256::one()).0
                    },
                    _ => U256::zero(),
                }
            };

            stack.insert(0, result);
        } else if opcode == 0x20 {
            // SHA3
            let offset = stack.remove(0).as_usize();
            let size = stack.remove(0).as_usize();
            let limit_32 = if (offset+size) % 32 != 0 {
                (((offset+size) / 32 )+1) * 32
            } else {
                ((offset+size) / 32 ) * 32
            };
            
            if memory.len() < limit_32 {
                for i in memory.len()..limit_32 {
                    memory.push(0);
                }
            }
            
            let val: &[u8] = memory[offset..offset+size].try_into().unwrap();
            let hash = Keccak256::digest(val);
            let hash_slice = hash.as_slice();
            stack.insert(0, U256::from_big_endian(hash_slice));

        } else if opcode == 0x30 {
            // ADDRESS
            stack.insert(0, U256::from_str_radix(&tx.to.clone().unwrap_or_default(), 16).unwrap())

        } else if opcode == 0x31 {
            // BALANCE
            let account = stack.remove(0);
            let account_bytes = &mut [0 as u8; 32];
            account.to_big_endian(account_bytes);
            let mut account_hex = hex::encode(account_bytes).trim_start_matches('0').to_string();
            account_hex.insert_str(0, "0x");
            let result = if state.get(account_hex.as_str()).is_none() {
                U256::zero()
            } else {
                let state_data = state.get(account_hex.as_str()).unwrap();
                U256::from_str_radix(state_data.clone().balance.unwrap_or_default().as_str(), 16).unwrap()
            };
            stack.insert(0, result)
        } else if opcode == 0x32 {
            // ORIGIN
            stack.insert(0, U256::from_str_radix(&tx.origin.clone().unwrap_or_default(), 16).unwrap())
        } else if opcode == 0x33 {
            // CALLER
            stack.insert(0, U256::from_str_radix(&tx.from.clone().unwrap_or_default(), 16).unwrap())
        } else if opcode == 0x34 {
            // CALLVALUE
            stack.insert(0, U256::from_str_radix(&tx.value.clone().unwrap_or_default(), 16).unwrap())
        } else if opcode == 0x35 {
            // CALLDATALOAD
            let offset = stack.remove(0).as_usize();
            let data = tx.data.clone().unwrap_or_default();
            let mut data_bytes = hex::decode(data).unwrap();
            if(offset+32 > data_bytes.len()) {     
                for i in data_bytes.len()..offset+32{
                    data_bytes.push(0x0);
                }
            }
            let data_trimmed = &data_bytes[offset..offset+32];
            let data_trimmed_hex = hex::encode(data_trimmed);
            stack.insert(0, U256::from_str_radix(&data_trimmed_hex.as_str(), 16).unwrap());
        } else if opcode == 0x36 {
            // CALLDATASIZE
            let data = tx.data.clone().unwrap_or_default();
            let data_bytes = hex::decode(data).unwrap();
            stack.insert(0, U256::from(data_bytes.len()));
        } else if opcode == 0x37 {
            // CALLDATACOPY
            let dest_offset = stack.remove(0).as_usize();
            let offset = stack.remove(0).as_usize();
            let size = stack.remove(0).as_usize();
            if dest_offset+size > memory.len() {
                for _ in memory.len()..dest_offset+size {
                    memory.push(0x0);
                }
            }
            let data = tx.data.clone().unwrap_or_default();
            let mut data_bytes = hex::decode(data).unwrap();
            let data_trimmed = &data_bytes[offset..offset+size];
            memory[dest_offset..dest_offset+size].copy_from_slice(data_trimmed);
        } else if opcode == 0x38 {
            // CODESIZE
            stack.insert(0, U256::from(code.len()));
        } else if opcode == 0x39 {
            // CODECOPY
            let dest_offset = stack.remove(0).as_usize();
            let offset = stack.remove(0).as_usize();
            let size = stack.remove(0).as_usize();
            if dest_offset+size > memory.len() {
                for _ in memory.len()..dest_offset+size {
                    memory.push(0x0);
                }
            }
            let codesize = if code.len() > size {size} else {code.len()};
            memory[dest_offset..dest_offset+codesize].copy_from_slice(&code[offset..offset+codesize]);
        } else if opcode == 0x3A {
            // GASPRICE
            stack.insert(0, U256::from_str_radix(&tx.gasprice.clone().unwrap_or_default(), 16).unwrap())
        } else if opcode == 0x3B {
            // EXTCODESIZE
            let account = stack.remove(0);
            let account_bytes = &mut [0 as u8; 32];
            account.to_big_endian(account_bytes);
            let mut account_hex = hex::encode(account_bytes).trim_start_matches('0').to_string();
            account_hex.insert_str(0, "0x");
            let result = if state.get(account_hex.as_str()).is_none() {
                U256::zero()
            } else {
                let state_data = state.get(account_hex.as_str()).unwrap();
                U256::from(hex::decode(state_data.clone().code.unwrap_or_default().bin.unwrap_or_default()).unwrap().len())
            };
            stack.insert(0, result)
        } else if opcode == 0x3C {
            // EXTCODECOPY
            let account = stack.remove(0);
            let account_bytes = &mut [0 as u8; 32];
            account.to_big_endian(account_bytes);
            let mut account_hex = hex::encode(account_bytes).trim_start_matches('0').to_string();
            account_hex.insert_str(0, "0x");
            let extcode = if state.get(account_hex.as_str()).is_none() {
                hex::decode("0x0").unwrap()
            } else {
                let state_data = state.get(account_hex.as_str()).unwrap();
                hex::decode(state_data.clone().code.unwrap_or_default().bin.unwrap_or_default()).unwrap()
            };
            let dest_offset = stack.remove(0).as_usize();
            let offset = stack.remove(0).as_usize();
            let size = stack.remove(0).as_usize();
            if dest_offset+size > memory.len() {
                for _ in memory.len()..dest_offset+size {
                    memory.push(0x0);
                }
            }
            let mut extcode_clone = extcode.clone();
            if offset + size > extcode_clone.len() {
                for _ in extcode_clone.len()..offset + size {
                    extcode_clone.push(0x0);
                }
            }

            memory[dest_offset..dest_offset+size].copy_from_slice(&extcode_clone[offset..offset+size]);
        } else if opcode == 0x3D {
            // RETURNDATASIZE
            let return_data = hex::decode(returns.clone()).unwrap();
            stack.insert(0, U256::from(return_data.len()))
        } else if opcode == 0x3E {
            // RETURNDATACOPY
            let destoffset = stack.remove(0).as_usize();
            let offset = stack.remove(0).as_usize();
            let size = stack.remove(0).as_usize();

            let return_data = hex::decode(returns.clone()).unwrap();
            
            if memory.len() < destoffset+size {
                for i in memory.len()..destoffset+size {
                    memory.push(0);
                }
            }

            memory[destoffset..destoffset+size].copy_from_slice(&return_data[offset..offset+size])
        } else if opcode == 0x3F {
            // EXTCODEHASH
            let account = stack.remove(0);
            let account_bytes = &mut [0 as u8; 32];
            account.to_big_endian(account_bytes);
            let mut account_hex = hex::encode(account_bytes).trim_start_matches('0').to_string();
            account_hex.insert_str(0, "0x");
            let extcode = if state.get(account_hex.as_str()).is_none() {
                Vec::from([0x0])
            } else {
                let state_data = state.get(account_hex.as_str()).unwrap();
                hex::decode(state_data.clone().code.unwrap_or_default().bin.unwrap_or_default()).unwrap()
            };

            let hash = Keccak256::digest(extcode);
            let hash_slice = if state.get(account_hex.as_str()).is_none() {&[0x0 as u8]} else {hash.as_slice()};
            stack.insert(0, U256::from_big_endian(hash_slice));
        } else if opcode == 0x40 {
            // BLOCKHASH
            let blocknum = stack.remove(0);
            stack.insert(0, U256::zero());
        } else if opcode == 0x41 {
            // COINBASE
            stack.insert(0, U256::from_str_radix(&block.coinbase.clone().unwrap_or_default(), 16).unwrap())
        } else if opcode == 0x42 {
            // TIMESTAMP
            stack.insert(0, U256::from_str_radix(&block.timestamp.clone().unwrap_or_default(), 16).unwrap())
        } else if opcode == 0x43 {
            // NUMBER
            stack.insert(0, U256::from_str_radix(&block.number.clone().unwrap_or_default(), 16).unwrap())
        } else if opcode == 0x44 {
            // PREVRANDAO
            stack.insert(0, U256::from_str_radix(&block.difficulty.clone().unwrap_or_default(), 16).unwrap())
        } else if opcode == 0x45 {
            // GASLIMIT
            stack.insert(0, U256::from_str_radix(&block.gaslimit.clone().unwrap_or_default(), 16).unwrap())
        } else if opcode == 0x46 {
            // CHAINID
            stack.insert(0, U256::from_str_radix(&block.chainid.clone().unwrap_or_default(), 16).unwrap())
        } else if opcode == 0x47 {
            // SELFBALANCE
            let address = tx.to.clone().unwrap_or_default();
            let balance = if state.get(address.as_str()).is_none() {
                U256::zero()
            } else {
                let state_data = state.get(address.as_str()).unwrap();
                U256::from_str_radix(state_data.clone().balance.unwrap_or_default().as_str(), 16).unwrap()
            };
            stack.insert(0, balance)
        } else if opcode == 0x48 {
            // BASEFEE
            stack.insert(0, U256::from_str_radix(&block.basefee.clone().unwrap_or_default(), 16).unwrap())
        } else if opcode == 0x50 {
            // POP
            stack.remove(0);
        } else if opcode == 0x51 {
            // MLOAD
            let offset = stack.remove(0).as_usize();
            let limit_32 = if (offset+32) % 32 != 0 {
                (((offset+32) / 32 )+1) * 32
            } else {
                ((offset+32) / 32 ) * 32
            };
            
            if memory.len() < limit_32 {
                for i in memory.len()..limit_32 {
                    memory.push(0);
                }
            }
            let val = U256::from_big_endian(memory[offset..offset+32].try_into().unwrap());
            stack.insert(0,val);
        } else if opcode == 0x52 {
            // MSTORE
            let offset = stack.remove(0).as_usize();
            let val = stack.remove(0);
            let limit_32 = (((offset+32) / 32 )+1) * 32;
            if memory.len() < limit_32 {
                for i in memory.len()..limit_32 {
                    memory.push(0);
                }
            }
            val.to_big_endian(&mut memory[offset..offset+32]);
        } else if opcode == 0x53 {
            // MSTORE8
            let offset = stack.remove(0);
            let val = stack.remove(0);
            let byte = val.byte(0);
            if memory.len() < offset.as_usize()+1 {
                for i in memory.len()..offset.as_usize()+1 {
                    memory.push(0);
                }
            }
            memory[offset.as_usize()] = byte;
        } else if opcode == 0x54 {
            // SLOAD
            let address = if tx.to.is_none() {
                "0x0".to_string()
            } else {
                tx.clone().to.unwrap_or_default().clone()
            };



            if state.get(&address).is_none() {
                let state_date = StateData {
                    balance: None,
                    code: None,
                    storage: HashMap::new().try_into().unwrap()
                };
                state.insert(address.clone(), state_date);
            } 

            let account_state = state.get(&address).unwrap();



            let loc = stack.remove(0);
            let loc_string = u256_to_hex(loc);
            let account_storage = account_state.storage.clone().unwrap_or_default();
            let value = if account_storage.get(&loc_string).is_none() {U256::zero()} else {
                let data = account_storage.get(&loc_string).unwrap();
                U256::from_big_endian(&data.clone() as &[u8])
            };
            stack.insert(0, value);
        } else if opcode == 0x55 {
            // SSTORE
            if _isStatic {
                return EvmResult {
                    stack: [].to_vec(),
                    success: false,
                    logs: logs,
                    returns: "".to_string()
                };
            }
            let address = if tx.to.is_none() {
                "0x0".to_string()
            } else {
                tx.clone().to.unwrap_or_default()
            };



            if state.get(&address).is_none() {
                let state_date = StateData {
                    balance: None,
                    code: None,
                    storage: HashMap::new().try_into().unwrap()
                };
                state.insert(address.clone(), state_date);
            } 

            let mut account_state = state.get(&address).unwrap();
            
            let loc = stack.remove(0);
            let value = stack.remove(0);

            let loc_string = u256_to_hex(loc);
            let value_data = &mut [0; 32];
            value.to_big_endian(value_data);

            let mut account_storage = account_state.storage.clone().unwrap();
            
            account_storage.insert(loc_string, value_data.to_vec());
            
            state.entry(address).and_modify(|s| s.storage = account_storage.try_into().unwrap());
            // /account_state.storage = account_storage.try_into().unwrap();
        } else if opcode == 0x56 {
            // JUMP
            pc = stack.remove(0).as_usize();
            if code[pc] != 0x5B 
                || ((code[pc-1] >= 0x60) && (code[pc-1] <= 0x7E)){
                logs.clear();
                return EvmResult {
                    stack: [].to_vec(),
                    success: false,
                    logs: logs,
                    returns: "0x0".to_string()
                };
            } 
        } else if opcode == 0x57 {
            // JUMPI
            let dest = stack.remove(0);
            let valid = stack.remove(0);
            if valid != U256::zero() {
                pc = dest.as_usize();
                if code[pc] != 0x5B 
                    || ((code[pc-1] >= 0x60) && (code[pc-1] <= 0x7E)){
                    logs.clear();
                    return EvmResult {
                        stack: [].to_vec(),
                        success: false,
                        logs: logs,
                        returns: "0x0".to_string()
                    };
                }
            }
        } else if opcode == 0x58 {
            // PC
            stack.insert(0,U256::from(pc-1));
        } else if opcode == 0x59 {
            // MSIZE
            stack.insert(0,U256::from(memory.len()))
        } else if opcode == 0x5A {
            // GAS
            stack.insert(0, U256::MAX);
        } else if opcode == 0x5B {
            // JUMPDEST
        } else if opcode == 0x60 {
            // PUSH1
            stack.insert(0,U256::from(code[pc]));
            pc+=1;
        } else if opcode == 0x61 {
            // PUSH2
            stack.insert(0,U256::from_big_endian(&code[pc..pc+2]));
            pc+=2;
        } else if opcode == 0x62 {
            // PUSH3
            stack.insert(0,U256::from_big_endian(&code[pc..pc+3]));
            pc+=3;
        } else if opcode == 0x63 {
            // PUSH4
            stack.insert(0,U256::from_big_endian(&code[pc..pc+4]));
            pc+=4;
        } else if opcode == 0x64 {
            // PUSH5
            stack.insert(0,U256::from_big_endian(&code[pc..pc+5]));
            pc+=5;
        } else if opcode == 0x65 {
            // PUSH6
            stack.insert(0,U256::from_big_endian(&code[pc..pc+6]));
            pc+=6;
        } else if opcode == 0x66 {
            // PUSH7
            stack.insert(0,U256::from_big_endian(&code[pc..pc+7]));
            pc+=7;
        } else if opcode == 0x67 {
            // PUSH8
            stack.insert(0,U256::from_big_endian(&code[pc..pc+8]));
            pc+=8;
        } else if opcode == 0x68 {
            // PUSH9
            stack.insert(0,U256::from_big_endian(&code[pc..pc+9]));
            pc+=9;
        } else if opcode == 0x69 {
            // PUSH10
            stack.insert(0,U256::from_big_endian(&code[pc..pc+10]));
            pc+=10;
        } else if opcode == 0x6A {
            // PUSH11
            stack.insert(0,U256::from_big_endian(&code[pc..pc+11]));
            pc+=11;
        } else if opcode == 0x6B {
            // PUSH12
            stack.insert(0,U256::from_big_endian(&code[pc..pc+12]));
            pc+=12;
        } else if opcode == 0x6C {
            // PUSH13
            stack.insert(0,U256::from_big_endian(&code[pc..pc+13]));
            pc+=13;
        } else if opcode == 0x6D {
            // PUSH14
            stack.insert(0,U256::from_big_endian(&code[pc..pc+14]));
            pc+=14;
        } else if opcode == 0x6E {
            // PUSH15
            stack.insert(0,U256::from_big_endian(&code[pc..pc+15]));
            pc+=15;
        } else if opcode == 0x6F {
            // PUSH16
            stack.insert(0,U256::from_big_endian(&code[pc..pc+16]));
            pc+=16;
        } else if opcode == 0x70 {
            // PUSH17
            stack.insert(0,U256::from_big_endian(&code[pc..pc+17]));
            pc+=17;
        } else if opcode == 0x71 {
            // PUSH18
            stack.insert(0,U256::from_big_endian(&code[pc..pc+18]));
            pc+=18;
        } else if opcode == 0x72 {
            // PUSH19
            stack.insert(0,U256::from_big_endian(&code[pc..pc+19]));
            pc+=19;
        } else if opcode == 0x73 {
            // PUSH20
            stack.insert(0,U256::from_big_endian(&code[pc..pc+20]));
            pc+=20;
        } else if opcode == 0x74 {
            // PUSH21
            stack.insert(0,U256::from_big_endian(&code[pc..pc+21]));
            pc+=21;
        } else if opcode == 0x75 {
            // PUSH22
            stack.insert(0,U256::from_big_endian(&code[pc..pc+22]));
            pc+=22;
        } else if opcode == 0x76 {
            // PUSH23
            stack.insert(0,U256::from_big_endian(&code[pc..pc+23]));
            pc+=23;
        } else if opcode == 0x77 {
            // PUSH24
            stack.insert(0,U256::from_big_endian(&code[pc..pc+24]));
            pc+=24;
        } else if opcode == 0x78 {
            // PUSH25
            stack.insert(0,U256::from_big_endian(&code[pc..pc+25]));
            pc+=25;
        } else if opcode == 0x79 {
            // PUSH26
            stack.insert(0,U256::from_big_endian(&code[pc..pc+26]));
            pc+=26;
        } else if opcode == 0x7A {
            // PUSH27
            stack.insert(0,U256::from_big_endian(&code[pc..pc+27]));
            pc+=27;
        } else if opcode == 0x7B {
            // PUSH28
            stack.insert(0,U256::from_big_endian(&code[pc..pc+28]));
            pc+=28;
        } else if opcode == 0x7C {
            // PUSH29
            stack.insert(0,U256::from_big_endian(&code[pc..pc+29]));
            pc+=29;
        } else if opcode == 0x7D {
            // PUSH30
            stack.insert(0,U256::from_big_endian(&code[pc..pc+30]));
            pc+=30;
        } else if opcode == 0x7E {
            // PUSH31
            stack.insert(0,U256::from_big_endian(&code[pc..pc+31]));
            pc+=31;
        } else if opcode == 0x7F {
            // PUSH32
            stack.insert(0,U256::from_big_endian(&code[pc..pc+32]));
            pc+=32;
        } else if opcode == 0x80 {
            // DUP1
            stack.insert(0,stack[0]);
        } else if opcode == 0x81 {
            // DUP2
            stack.insert(0,stack[1]);
        } else if opcode == 0x82 {
            // DUP3
            stack.insert(0,stack[2]);
        } else if opcode == 0x83 {
            // DUP4
            stack.insert(0,stack[3]);
        } else if opcode == 0x84 {
            // DUP5
            stack.insert(0,stack[4]);
        } else if opcode == 0x85 {
            // DUP6
            stack.insert(0,stack[5]);
        } else if opcode == 0x86 {
            // DUP7
            stack.insert(0,stack[6]);
        } else if opcode == 0x87 {
            // DUP8
            stack.insert(0,stack[7]);
        } else if opcode == 0x88 {
            // DUP9
            stack.insert(0,stack[8]);
        } else if opcode == 0x89 {
            // DUP10
            stack.insert(0,stack[9]);
        } else if opcode == 0x8A {
            // DUP11
            stack.insert(0,stack[10]);
        } else if opcode == 0x8B {
            // DUP12
            stack.insert(0,stack[11]);
        } else if opcode == 0x8C {
            // DUP13
            stack.insert(0,stack[12]);
        } else if opcode == 0x8D {
            // DUP14
            stack.insert(0,stack[13]);
        } else if opcode == 0x8E {
            // DUP15
            stack.insert(0,stack[14]);
        } else if opcode == 0x8F {
            // DUP16
            stack.insert(0,stack[15]);
        } else if opcode == 0x90 {
            // SWAP1
           let op = stack[stack.len() - 2];
           let top = stack[stack.len() - 1];
           
           let stack_len = stack.len();
           
           stack[stack_len - 2] = top;
           stack[stack_len - 1] = op;
        } else if opcode == 0x91 {
            // SWAP2
           let op = stack[stack.len() - 3];
           let top = stack[stack.len() - 1];

           let stack_len = stack.len();
           
           stack[stack_len - 3] = top;
           stack[stack_len - 1] = op;
        } else if opcode == 0x92 {
            // SWAP3
           let op = stack[stack.len() - 4];
           let top = stack[stack.len() - 1];

           let stack_len = stack.len();
           
           stack[stack_len - 4] = top;
           stack[stack_len - 1] = op;
        } else if opcode == 0x93 {
            // SWAP4
           let op = stack[stack.len() - 5];
           let top = stack[stack.len() - 1];

           let stack_len = stack.len();
           
           stack[stack_len - 5] = top;
           stack[stack_len - 1] = op;
        } else if opcode == 0x94 {
            // SWAP5
           let op = stack[stack.len() - 6];
           let top = stack[stack.len() - 1];

           let stack_len = stack.len();
           
           stack[stack_len - 6] = top;
           stack[stack_len - 1] = op;
        } else if opcode == 0x95 {
            // SWAP6
           let op = stack[stack.len() - 7];
           let top = stack[stack.len() - 1];

           let stack_len = stack.len();
           
           stack[stack_len - 7] = top;
           stack[stack_len - 1] = op;
        } else if opcode == 0x96 {
            // SWAP7
           let op = stack[stack.len() - 8];
           let top = stack[stack.len() - 1];

           let stack_len = stack.len();
           
           stack[stack_len - 8] = top;
           stack[stack_len - 1] = op;
        } else if opcode == 0x97 {
            // SWAP8
           let op = stack[stack.len() - 9];
           let top = stack[stack.len() - 1];
           
           let stack_len = stack.len();

           stack[stack_len - 9] = top;
           stack[stack_len - 1] = op;
        } else if opcode == 0x98 {
            // SWAP9
           let op = stack[stack.len() - 10];
           let top = stack[stack.len() - 1];

           let stack_len = stack.len();
           
           stack[stack_len - 10] = top;
           stack[stack_len - 1] = op;
        } else if opcode == 0x99 {
            // SWAP10
           let op = stack[stack.len() - 11];
           let top = stack[stack.len() - 1];

           let stack_len = stack.len();
           
           stack[stack_len - 11] = top;
           stack[stack_len - 1] = op;
        } else if opcode == 0x9A {
            // SWAP11
           let op = stack[stack.len() - 12];
           let top = stack[stack.len() - 1];

           let stack_len = stack.len();
           
           stack[stack_len - 12] = top;
           stack[stack_len - 1] = op;
        } else if opcode == 0x9B {
            // SWAP12
           let op = stack[stack.len() - 13];
           let top = stack[stack.len() - 1];

           let stack_len = stack.len();
           
           stack[stack_len - 13] = top;
           stack[stack_len - 1] = op;
        } else if opcode == 0x9C {
            // SWAP13
           let op = stack[stack.len() - 14];
           let top = stack[stack.len() - 1];

           let stack_len = stack.len();
           
           stack[stack_len - 14] = top;
           stack[stack_len - 1] = op;
        } else if opcode == 0x9D {
            // SWAP14
           let op = stack[stack.len() - 15];
           let top = stack[stack.len() - 1];

           let stack_len = stack.len();
           
           stack[stack_len - 15] = top;
           stack[stack_len - 1] = op;
        } else if opcode == 0x9E {
            // SWAP15
           let op = stack[stack.len() - 16];
           let top = stack[stack.len() - 1];

           let stack_len = stack.len();
           
           stack[stack_len - 16] = top;
           stack[stack_len - 1] = op;
        } else if opcode == 0x9F {
            // SWAP16
           let op = stack[stack.len() - 17];
           let top = stack[stack.len() - 1];

           let stack_len = stack.len();
           
           stack[stack_len - 17] = top;
           stack[stack_len - 1] = op;
        } else if opcode == 0xA0 {
            // LOG0
            let offset = stack.remove(0).as_usize();
            let size = stack.remove(0).as_usize();

            let address = tx.to.clone().unwrap_or_default();

            if memory.len() < offset+size {
                for i in memory.len()..offset+size {
                    memory.push(0);
                }
            }
            let data = hex::encode(&memory[offset..offset+size]);
            let topics:Vec<String>= Vec::new();
            let log = EvmLog {
                address: address,
                data: data,
                topics: topics
            };
            logs.push(log);
        } else if opcode == 0xA1 {
            // LOG1
            let offset = stack.remove(0).as_usize();
            let size = stack.remove(0).as_usize();

            let address = tx.to.clone().unwrap_or_default();

            if memory.len() < offset+size {
                for i in memory.len()..offset+size {
                    memory.push(0);
                }
            }
            let data = hex::encode(&memory[offset..offset+size]);
            let mut topics:Vec<String>= Vec::new();
            
            let topic1 = stack.remove(0);

            topics.push(u256_to_hex(topic1));

            let log = EvmLog {
                address: address,
                data: data,
                topics: topics
            };
            logs.push(log);
        } else if opcode == 0xA2 {
            // LOG2
            let offset = stack.remove(0).as_usize();
            let size = stack.remove(0).as_usize();

            let address = tx.to.clone().unwrap_or_default();

            if memory.len() < offset+size {
                for i in memory.len()..offset+size {
                    memory.push(0);
                }
            }
            let data = hex::encode(&memory[offset..offset+size]);
            let mut topics:Vec<String>= Vec::new();
            
            let topic1 = stack.remove(0);
            let topic2 = stack.remove(0);

            topics.push(u256_to_hex(topic1));
            topics.push(u256_to_hex(topic2));

            let log = EvmLog {
                address: address,
                data: data,
                topics: topics
            };
            logs.push(log);
        } else if opcode == 0xA3 {
            // LOG3
            let offset = stack.remove(0).as_usize();
            let size = stack.remove(0).as_usize();

            let address = tx.to.clone().unwrap_or_default();

            if memory.len() < offset+size {
                for i in memory.len()..offset+size {
                    memory.push(0);
                }
            }
            let data = hex::encode(&memory[offset..offset+size]);
            let mut topics:Vec<String>= Vec::new();
            
            let topic1 = stack.remove(0);
            let topic2 = stack.remove(0);
            let topic3 = stack.remove(0);

            topics.push(u256_to_hex(topic1));
            topics.push(u256_to_hex(topic2));
            topics.push(u256_to_hex(topic3));

            let log = EvmLog {
                address: address,
                data: data,
                topics: topics
            };
            logs.push(log);
        } else if opcode == 0xA4 {
            // LOG4
            let offset = stack.remove(0).as_usize();
            let size = stack.remove(0).as_usize();

            let address = tx.to.clone().unwrap_or_default();

            if memory.len() < offset+size {
                for i in memory.len()..offset+size {
                    memory.push(0);
                }
            }
            let data = hex::encode(&memory[offset..offset+size]);
            let mut topics:Vec<String>= Vec::new();
            
            let topic1 = stack.remove(0);
            let topic2 = stack.remove(0);
            let topic3 = stack.remove(0);
            let topic4 = stack.remove(0);

            topics.push(u256_to_hex(topic1));
            topics.push(u256_to_hex(topic2));
            topics.push(u256_to_hex(topic3));
            topics.push(u256_to_hex(topic4));

            let log = EvmLog {
                address: address,
                data: data,
                topics: topics
            };
            logs.push(log);
        } else if opcode == 0xF0 {
            // CREATE
            let address = tx.to.clone().unwrap();
            let value = stack.remove(0);
            let offset = stack.remove(0).as_usize();
            let size = stack.remove(0).as_usize();

            let code = &memory[offset..offset+size];
            
            let call_tx =  EvmTx {
                to: tx.to.clone(),
                from: tx.to.clone(),
                origin: None,
                gasprice: None,
                value: u256_to_hex(value).try_into().unwrap(),
                data: tx.data.clone()
            };
            
            let call_result = evm(&code, &call_tx, &block, state, false.try_into().unwrap());
        
            if call_result.success {
                state.entry(tx.to.clone().unwrap()).and_modify(|s| s.code = Some(EvmCode {
                    asm: call_result.returns.clone().try_into().unwrap(),
                    bin: call_result.returns.clone().try_into().unwrap()
                }));
                stack.insert(0, U256::from_str_radix(&tx.to.clone().unwrap(), 16).unwrap());
            } else {
                stack.insert(0, U256::zero());
            }
        } else if opcode == 0xF1 {
            // CALL
            let gas = stack.remove(0);
            let address = stack.remove(0);
            let value = stack.remove(0);
            let argsoffset = stack.remove(0);
            let argsSize = stack.remove(0);
            let retOffset = stack.remove(0).as_usize();
            let retSize = stack.remove(0).as_usize();

            let account_bytes = &mut [0 as u8; 32];
            address.to_big_endian(account_bytes);
            let mut account_hex = hex::encode(account_bytes).trim_start_matches('0').to_string();
            account_hex.insert_str(0, "0x");
            let extcode = if state.get(account_hex.as_str()).is_none() {
                hex::decode("0x0").unwrap()
            } else {
                let state_data = state.get(account_hex.as_str()).unwrap();
                hex::decode(state_data.clone().code.unwrap_or_default().bin.unwrap_or_default()).unwrap()
            };

            let call_tx =  EvmTx {
                to: account_hex.try_into().unwrap(),
                from: tx.to.clone(),
                origin: None,
                gasprice: None,
                value: u256_to_hex(value).try_into().unwrap(),
                data: tx.data.clone()
            };

            let call_result = evm(&extcode, &call_tx, &block, state, false.try_into().unwrap());

            if call_result.success {
                stack.insert(0, U256::one());
            } else {
                stack.insert(0, U256::zero());
            }

            returns = call_result.returns.clone();
            let return_data = hex::decode(returns.clone()).unwrap();
            if memory.len() < retOffset+retSize {
                for i in memory.len()..retOffset+retSize {
                    memory.push(0);
                }
            }

            memory[retOffset..retOffset+retSize].copy_from_slice(&return_data[0..retSize]);
        } else if opcode == 0xF2 {
            // CALLCODE
        } else if opcode == 0xF3 {
            // RETURN
            let offset = stack.remove(0).as_usize();
            let size = stack.remove(0).as_usize();
            let limit_32 = if (offset+32) % 32 != 0 {
                (((offset+32) / 32 )+1) * 32
            } else {
                ((offset+32) / 32 ) * 32
            };
            
            if memory.len() < offset+size {
                for i in memory.len()..offset+size {
                    memory.push(0);
                }
            }
            let val = hex::encode(&memory[offset..offset+size]);
            returns = val;
        } else if opcode == 0xF4 {
            // DELEGATECALL
            let gas = stack.remove(0);
            let address = stack.remove(0);
            let argsoffset = stack.remove(0);
            let argsSize = stack.remove(0);
            let retOffset = stack.remove(0).as_usize();
            let retSize = stack.remove(0).as_usize();

            let account_bytes = &mut [0 as u8; 32];
            address.to_big_endian(account_bytes);
            let mut account_hex = hex::encode(account_bytes).trim_start_matches('0').to_string();
            account_hex.insert_str(0, "0x");
            let extcode = if state.get(account_hex.as_str()).is_none() {
                hex::decode("0x0").unwrap()
            } else {
                let state_data = state.get(account_hex.as_str()).unwrap();
                hex::decode(state_data.clone().code.unwrap_or_default().bin.unwrap_or_default()).unwrap()
            };

            let call_result = evm(&extcode, &tx.clone(), &block, state, false.try_into().unwrap());

            println!("{}", call_result.success);
            if call_result.success {
                stack.insert(0, U256::one());
            } else {
                stack.insert(0, U256::zero());
            }

            if call_result.success {
                returns = call_result.returns.clone();
                let return_data = hex::decode(returns.clone()).unwrap();
                if memory.len() < retOffset+retSize {
                    for i in memory.len()..retOffset+retSize {
                        memory.push(0);
                    }
                }

                memory[retOffset..retOffset+retSize].copy_from_slice(&return_data[0..retSize]);
            }
            
        } else if opcode == 0xF5 {
            // CREATE2
        } else if opcode == 0xFA {
            // STATICCALL
            let gas = stack.remove(0);
            let address = stack.remove(0);
            let argsoffset = stack.remove(0);
            let argsSize = stack.remove(0);
            let retOffset = stack.remove(0).as_usize();
            let retSize = stack.remove(0).as_usize();

            let account_bytes = &mut [0 as u8; 32];
            address.to_big_endian(account_bytes);
            let mut account_hex = hex::encode(account_bytes).trim_start_matches('0').to_string();
            account_hex.insert_str(0, "0x");
            let extcode = if state.get(account_hex.as_str()).is_none() {
                hex::decode("0x0").unwrap()
            } else {
                let state_data = state.get(account_hex.as_str()).unwrap();
                hex::decode(state_data.clone().code.unwrap_or_default().bin.unwrap_or_default()).unwrap()
            };

            let call_tx =  EvmTx {
                to: account_hex.try_into().unwrap(),
                from: tx.to.clone(),
                origin: None,
                gasprice: None,
                value: u256_to_hex(U256::zero()).try_into().unwrap(),
                data: tx.data.clone()
            };

            let call_result = evm(&extcode, &call_tx, &block, state, true.try_into().unwrap());

            if call_result.success {
                stack.insert(0, U256::one());
            } else {
                stack.insert(0, U256::zero());
            }

            if call_result.success {
                returns = call_result.returns.clone();
                let return_data = hex::decode(returns.clone()).unwrap();
                if memory.len() < retOffset+retSize {
                    for i in memory.len()..retOffset+retSize {
                        memory.push(0);
                    }
                }

                memory[retOffset..retOffset+retSize].copy_from_slice(&return_data[0..retSize]);
            }
        } else if opcode == 0xFD {
            // REVERT
            logs.clear();
            let offset = stack.remove(0).as_usize();
            let size = stack.remove(0).as_usize();
            let limit_32 = if (offset+32) % 32 != 0 {
                (((offset+32) / 32 )+1) * 32
            } else {
                ((offset+32) / 32 ) * 32
            };
            
            if memory.len() < offset+size {
                for i in memory.len()..offset+size {
                    memory.push(0);
                }
            }
            return EvmResult {
                stack: [].to_vec(),
                success: false,
                logs: logs,
                returns: hex::encode(&memory[offset..offset+size])
            }
        } else if opcode == 0xFE {
            // INVALID
            logs.clear();
            return EvmResult {
                stack: [].to_vec(),
                success: false,
                logs: logs,
                returns: "".to_string()
            }
        } else if opcode == 0xFF {
            // SELFDESTRUCT
            let address = tx.to.clone().unwrap_or_default();
            let value = state.get(&address).unwrap().balance.clone().unwrap();
            let transfer_address = u256_to_hex(stack.remove(0));
            
            if state.get(&transfer_address).is_none() {
                let state_data = StateData {
                    balance: "0x0".to_string().try_into().unwrap(),
                    code: None,
                    storage: HashMap::new().try_into().unwrap()
                };
                state.insert(transfer_address.clone(), state_data);
            } 
        
            let mut account_state = state.get(&transfer_address).unwrap();
        
            let mut balance = U256::from_str_radix(account_state.balance.clone().unwrap_or_default().as_str(), 16).unwrap();
            balance += U256::from_str_radix(value.clone().as_str(), 16).unwrap();
            state.entry(transfer_address).and_modify(|s| s.balance = u256_to_hex(balance).try_into().unwrap());
            
            state.remove(&address);
        }
    }

    // TODO: Implement me

    return EvmResult {
        stack: stack,
        success: true,
        logs: logs,
        returns: returns
    };
}
