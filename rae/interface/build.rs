use std::fs;

//Build GRPC server and client for UPF planning service
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let proto_file = "src/platform_interface.proto";

    let x: [&str; 0] = [];
    tonic_build::configure()
        .build_server(true)
        .build_client(true)
        .out_dir("src/")
        .compile(&[proto_file], &x)
        .unwrap_or_else(|e| panic!("Failed to compile proto: {}", e));

    fs::rename("src/_.rs", "src/platform_interface.rs")?;

    Ok(())
}
