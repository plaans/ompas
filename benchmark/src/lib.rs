use crate::config::mail_config::MailConfig;
use lettre::message::header::ContentType;
use lettre::message::{Attachment, MultiPart, SinglePart};
use lettre::transport::smtp::authentication::Credentials;
use lettre::{Message, SmtpTransport, Transport};
use std::fmt::Display;
use std::fs;
use std::path::PathBuf;
use std::process::Command;
use std::time::Duration;
use zip_archive::Archiver;

pub mod config;

#[derive(Default)]
pub struct BenchmarkData {
    pub runs: Vec<RunData>,
}

impl BenchmarkData {
    pub fn format_data(&self, bench_name: String) -> String {
        "".to_string();
        let mut total_duration = Duration::from_secs(0);

        for run in &self.runs {
            total_duration += run.duration;
        }

        let n_run = self.runs.len();

        let seconds = total_duration.as_secs() % 60;
        let minutes = (total_duration.as_secs() / 60) % 60;
        let hours = (total_duration.as_secs() / 60) / 60;

        format!(
            "{}\n\
            \t-total time: {}:{}:{}\n\
            \t-number of runs: {}\
        ",
            bench_name, hours, minutes, seconds, n_run
        )
    }

    pub fn mean_time(&self) -> Duration {
        let mut total = Duration::from_secs(0);
        for r in &self.runs {
            total += r.duration;
        }
        total / (self.runs.len() as u32)
    }
}

pub struct RunData {
    pub duration: Duration,
}

pub fn send_email(config: &MailConfig, message: String, benchmark_path: PathBuf) {
    println!("sending email");

    let mut archiver = Archiver::new();
    let benchmark_label = benchmark_path
        .iter()
        .last()
        .unwrap()
        .to_str()
        .unwrap()
        .to_string();
    archiver.push(benchmark_path);
    let mut dest: PathBuf = "/tmp".into();
    archiver.set_destination(dest.clone());
    dest.push(format!("{}.zip", benchmark_label));

    let num_thread = std::thread::available_parallelism().unwrap().get();
    archiver.set_thread_count(num_thread as u32);

    match archiver.archive() {
        Ok(_) => (),
        Err(e) => println!("Cannot archive the directory! {}", e),
    };

    let attachement = Attachment::new(benchmark_label).body(
        fs::read(dest).unwrap(),
        ContentType::parse("application/zip").unwrap(),
    );

    let message: Message = Message::builder()
        .from(config.from.parse().unwrap())
        .to(config.to.parse().unwrap())
        .subject("Ompas Benchmark")
        .multipart(
            MultiPart::mixed()
                .singlepart(attachement)
                .singlepart(SinglePart::plain(message)),
        )
        .unwrap();

    let creds = Credentials::new(config.from.to_string(), config.password.to_string());

    // Open a remote connection to gmail
    let mailer = SmtpTransport::relay(&config.smtp)
        .unwrap()
        .credentials(creds)
        .build();

    // Send the email
    match mailer.send(&message) {
        Ok(_) => println!("Email sent successfully!"),
        Err(e) => panic!("Could not send email: {:?}", e),
    }
}

pub fn install_binary(name: impl Display, path: PathBuf) {
    println!("Installation of the latest version of {}", name);
    let child = Command::new("cargo")
        .args([
            "install",
            "--force",
            "--bin",
            name.to_string().as_str(),
            "--path",
            path.to_str().unwrap(),
        ])
        .spawn();

    if let Ok(mut c) = child {
        //bar.println("Spawned successfully");
        match c.wait() {
            Ok(e) => {
                if e.code().unwrap_or(0) != 0 {
                    panic!("{}", e)
                }
            }
            Err(e) => panic!("{}", e),
        }
        //bar.println(format!("Exit with: {:?}", c.wait()));
    } else {
        panic!("panic");
    }
}
