use ompas_structs::execution::resource::{WaiterPriority, WaiterTicket};

pub fn main() {
    let t1 = WaiterTicket::new(0, WaiterPriority::Execution(1000));
    let t2 = WaiterTicket::new(1, WaiterPriority::Execution(1));
    let t3 = WaiterTicket::new(2, WaiterPriority::Execution(1000));
    let t4 = WaiterTicket::new(3, WaiterPriority::Execution(1));

    let mut priorities: Vec<WaiterTicket> = vec![t1, t2, t3, t4];
    let r = priorities[0].cmp(&priorities[1]);

    println!("{:?}", r);

    println!("{:?}", priorities);

    priorities.sort_by(|k1, k2| k1.cmp(&k2));

    println!("{:?}", priorities);
}
