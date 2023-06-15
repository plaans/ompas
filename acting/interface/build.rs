use std::fs;
use std::path::PathBuf;

#[cfg(feature = "generate_bindings")]
//Build GRPC server and client for UPF planning service
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let proto_file: PathBuf = "../../grpc/platform_interface.proto".into();
    //let proto_file = proto_file.canonicalize().unwrap();

    let x: [&str; 1] = ["../../grpc"];
    tonic_build::configure()
        .build_server(true)
        .build_client(true)
        .out_dir("src/")
        .compile(&[proto_file], &x)
        .unwrap_or_else(|e| panic!("Failed to compile proto: {}", e));

    fs::rename("src/_.rs", "src/platform_interface.rs")?;

    Ok(())
}

#[cfg(not(feature = "generate_bindings"))]
fn main() -> Result<(), Box<dyn std::error::Error>> {
    Ok(())
}
