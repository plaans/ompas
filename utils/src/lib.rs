#[macro_use]
extern crate lazy_static;
pub mod log;
pub mod task_handler;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
