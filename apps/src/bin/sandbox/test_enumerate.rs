use aries_utils::enumerate;
use aries_utils::StreamingIterator;
use ompas_lisp::structs::LValue;

fn main() {
    println!("hello, world!");
    let vec_robot: Vec<LValue> = vec!["robot_0".into(), "robot_1".into()];
    let vec_places: Vec<LValue> = vec!["kitchen".into(), "bedrooom".into()];

    /*let xs = vec!["x1", "x2"];
    let it = enumerate(vec![xs.iter()]);
    let number = it.count();
    println!("number of elements in enumerator: {}", number);*/

    enumerate(vec![vec_robot.iter(), vec_places.iter()]).for_each(|x| {
        let mut str = "(".to_string();
        for e in x {
            str.push_str(format!("{},", e).as_str())
        }

        str.push(')');

        println!("param instantiation: {}", str)
    });

    let mut iter_params = enumerate(vec![vec_robot.iter(), vec_places.iter()]);

    while let params = iter_params.next() {
        match params {
            None => break,
            Some(params) => {
                let mut str = "(".to_string();
                for param in params {
                    str.push_str(format!("{},", param).as_str())
                }

                str.push(')');

                println!("param instantiation: {}", str)
            }
        }
    }

    /*let params =
    enumerate(vec![vec_robot.iter(), vec_places.iter()]).map(|x| );*/
    //println!("number of elements in enumerator: {}", params.count());

    /*for param in params {
        let mut str = "(".to_string();
        for e in param {
            str.push_str(format!("{},", e).as_str())
        }

        str.push(')');

        println!("param instantiation{}", str)
    }*/
}
