use std::time::Duration;

#[derive(Default)]
pub struct BenchmarkData {
    pub runs: Vec<RunData>,
}

impl BenchmarkData {
    pub fn format_data(&self) -> String {
        "".to_string();
        let mut total_duration = Duration::from_secs(0);

        for run in &self.runs {
            total_duration += run.duration;
        }

        let n_run = self.runs.len();

        format!(
            "GOBOT-SIM BENCHMARK\n\
            \t-total time: {}\n\
            \t-number of runs: {}\
        ",
            total_duration.as_secs(),
            n_run
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
