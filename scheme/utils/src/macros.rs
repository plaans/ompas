#[macro_export]
macro_rules! blocking_async {
    ($expression:expr) => {{
        use std::thread;
        let handle = tokio::runtime::Handle::current();
        thread::spawn(move || handle.block_on(async move { $expression })).join()
    }};
}
