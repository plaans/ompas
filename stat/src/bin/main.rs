use ompas_stat::stat::system::SystemRunData;
use ompas_stat::statos_config::StatosConfig;
use std::fs;
use std::fs::{File, OpenOptions};
use std::io::Write;
use std::path::PathBuf;
use std::time::SystemTime;
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(name = "Statos", about = "Generation of problems for gripper domain")]
struct Opt {
    #[structopt(short = "c", long = "config")]
    config: PathBuf,
}

pub fn main() {
    println!("Hello, world!");

    let opt = Opt::from_args();

    let str = fs::read_to_string(opt.config).expect("Could not read config file");

    let config: StatosConfig =
        serde_yaml::from_str(&str).expect("Could not deserialize content of config");

    //println!("config: {:?}", config);

    for config in config.configs {
        let output_dir = &config.output_dir;
        let time = SystemTime::now();
        let system_run = SystemRunData::new(&config);
        println!(
            "Loaded {} files in {:.3} ms",
            system_run.get_number_of_run(),
            time.elapsed().unwrap().as_secs_f64() * 1000.0
        );
        let stat = system_run.compute_stat();

        for output in &config.output_gaussians {
            let time = SystemTime::now();

            let bar = output.to_latex(&system_run);

            for dat in &bar.dats {
                let mut path = output_dir.clone();
                path.push(&dat.path);
                let mut file = OpenOptions::new()
                    .create(true)
                    .write(true)
                    .truncate(true)
                    .open(path)
                    .unwrap();
                file.write_all(dat.content.as_bytes()).unwrap();
            }

            let mut path = output_dir.clone();
            path.push(&output.output_tex);

            let mut file = OpenOptions::new()
                .create(true)
                .write(true)
                .truncate(true)
                .open(path)
                .unwrap();
            file.write_all(bar.tex.as_bytes()).unwrap();

            println!(
                "time to compute stat : {:.3} ms",
                time.elapsed().unwrap().as_secs_f32() * 1000.0
            );
        }

        for output in &config.output_bars {
            let time = SystemTime::now();

            let bar = output.to_latex(&system_run);

            let mut path = output_dir.clone();
            path.push(&bar.output_dat);
            let mut file = OpenOptions::new()
                .create(true)
                .write(true)
                .truncate(true)
                .open(path)
                .unwrap();
            file.write_all(bar.dat.as_bytes()).unwrap();

            let mut path = output_dir.clone();
            path.push(&bar.output_tex);
            let mut file = OpenOptions::new()
                .create(true)
                .write(true)
                .truncate(true)
                .open(path)
                .unwrap();
            file.write_all(bar.tex.as_bytes()).unwrap();

            println!(
                "time to compute stat : {:.3} ms",
                time.elapsed().unwrap().as_secs_f32() * 1000.0
            );
        }

        for output in &config.output_plots {
            let time = SystemTime::now();

            let bar = output.to_latex(&system_run);

            for dat in &bar.dats {
                let mut path = output_dir.clone();
                path.push(&dat.path);
                let mut file = OpenOptions::new()
                    .create(true)
                    .write(true)
                    .truncate(true)
                    .open(path)
                    .unwrap();
                file.write_all(dat.content.as_bytes()).unwrap();
            }

            let mut path = output_dir.clone();
            path.push(&output.output_tex);

            let mut file = OpenOptions::new()
                .create(true)
                .write(true)
                .truncate(true)
                .open(path)
                .unwrap();
            file.write_all(bar.tex.as_bytes()).unwrap();

            println!(
                "time to compute stat : {:.3} ms",
                time.elapsed().unwrap().as_secs_f32() * 1000.0
            );
        }

        for output in &config.outputs {
            let time = SystemTime::now();

            let formatter = output.new_tabular_output(&stat);

            println!(
                "time to compute stat : {:.3} ms",
                time.elapsed().unwrap().as_secs_f32() * 1000.0
            );

            // println!("{}", formatter);

            if let Some(csv_output) = &output.csv_output {
                let csv = formatter.to_csv();

                let mut path = output_dir.clone();
                path.push(csv_output);
                let mut file = OpenOptions::new()
                    .create(true)
                    .write(true)
                    .truncate(true)
                    .open(path)
                    .unwrap();
                file.write_all(csv.as_bytes()).unwrap();
            }

            if let Some(latex_output) = &output.latex_output {
                let latex = formatter.to_latex();
                let path: PathBuf =
                    format!("{}{}", output_dir.display(), latex_output.display()).into();
                println!("path: {}", path.display());
                //let path = path.canonicalize().unwrap();
                //let path = latex_output;
                println!("path: {}", path.display());
                let _ = File::create(&path).unwrap();
                let mut file = OpenOptions::new()
                    .create(true)
                    .write(true)
                    .truncate(true)
                    .open(&path)
                    .unwrap();
                file.write_all(latex.as_bytes()).unwrap();
            }
        }
    }
}
