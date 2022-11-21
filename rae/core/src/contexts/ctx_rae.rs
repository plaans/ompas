use ompas_rae_interface::platform::Platform;
use ompas_rae_interface::platform::*;
use ompas_rae_structs::agenda::Agenda;
use ompas_rae_structs::monitor::MonitorCollection;
use ompas_rae_structs::resource::ResourceCollection;
use ompas_rae_structs::state::task_status::TaskStatus;
use ompas_rae_structs::TaskId;

pub const CTX_RAE: &str = "CtxRae";

pub struct CtxRae {
    pub monitors: MonitorCollection,
    pub resources: ResourceCollection,
    pub platform_interface: Option<Platform>,
    pub agenda: Agenda,
}

impl CtxRae {
    pub async fn get_execution_status(&self, id: &TaskId) -> TaskStatus {
        self.agenda.get_status(id).await
    }

    pub fn add_platform(&mut self, platform: Option<Platform>) {
        self.platform_interface = platform;
    }
}
