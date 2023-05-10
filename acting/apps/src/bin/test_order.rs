use ompas_core::ompas::manager::resource::{Ticket, WaiterPriority};

pub fn main() {
    let t1 = Ticket::new(0, WaiterPriority::Execution(1000));
    let t2 = Ticket::new(1, WaiterPriority::Execution(1));
    let t3 = Ticket::new(2, WaiterPriority::Execution(1000));
    let t4 = Ticket::new(3, WaiterPriority::Execution(1));

    let mut priorities: Vec<Ticket> = vec![t1, t2, t3, t4];
    let r = priorities[0].cmp(&priorities[1]);

    println!("{:?}", r);

    println!("{:?}", priorities);

    priorities.sort();

    println!("{:?}", priorities);
}
