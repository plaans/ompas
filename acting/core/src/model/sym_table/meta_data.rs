#[derive(Default, Clone)]
pub struct SymTableMetaData {
    n_timepoint: usize,
    n_result: usize,
    n_presence: usize,
    n_if: usize,
    n_handle: usize,
    n_chronicle_result: usize,
    n_start: usize,
    n_end: usize,
    n_start_task: usize,
    n_end_task: usize,
    n_arbitrary: usize,
}

impl SymTableMetaData {
    pub fn new_timepoint_index(&mut self) -> usize {
        let n = self.n_timepoint;
        self.n_timepoint += 1;
        n
    }

    pub fn new_result_index(&mut self) -> usize {
        let n = self.n_result;
        self.n_result += 1;
        n
    }

    pub fn new_presence_index(&mut self) -> usize {
        let n = self.n_presence;
        self.n_presence += 1;
        n
    }

    pub fn new_if_index(&mut self) -> usize {
        let n = self.n_if;
        self.n_if += 1;
        n
    }
    pub fn new_handle_index(&mut self) -> usize {
        let n = self.n_handle;
        self.n_handle += 1;
        n
    }
    pub fn new_start_index(&mut self) -> usize {
        let n = self.n_start;
        self.n_start += 1;
        n
    }

    pub fn new_end_index(&mut self) -> usize {
        let n = self.n_end;
        self.n_end += 1;
        n
    }

    pub fn new_start_task_index(&mut self) -> usize {
        let n = self.n_start_task;
        self.n_start_task += 1;
        n
    }

    pub fn new_end_task_index(&mut self) -> usize {
        let n = self.n_end_task;
        self.n_end_task += 1;
        n
    }

    pub fn new_chronicle_result_index(&mut self) -> usize {
        let n = self.n_chronicle_result;
        self.n_chronicle_result += 1;
        n
    }

    pub fn new_arbitrary_index(&mut self) -> usize {
        let n = self.n_arbitrary;
        self.n_arbitrary += 1;
        n
    }
}
