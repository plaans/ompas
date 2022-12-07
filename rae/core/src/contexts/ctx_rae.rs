use ompas_middleware::logger::LogClient;
use ompas_rae_interface::platform::Platform;
use ompas_rae_structs::agenda::Agenda;
use ompas_rae_structs::monitor::MonitorCollection;
use ompas_rae_structs::resource::ResourceCollection;
use ompas_rae_structs::state::action_status::ActionStatus;
use ompas_rae_structs::ActionId;

pub struct CtxOMPAS {
    pub monitors: MonitorCollection,
    pub resources: ResourceCollection,
    pub platform_interface: Option<Platform>,
    pub agenda: Agenda,
    pub(crate) log_client: LogClient,
}

impl CtxOMPAS {
    pub async fn get_execution_status(&self, id: &ActionId) -> ActionStatus {
        self.agenda.get_status(id).await
    }

    pub fn add_platform(&mut self, platform: Option<Platform>) {
        self.platform_interface = platform;
    }

    pub fn get_log_client(&self) -> LogClient {
        self.log_client.clone()
    }
}
