use crate::ompas::manager::acting::acting_var::ActingVarRef;
use crate::ompas::manager::acting::interval::Timepoint;
use crate::ompas::manager::acting::process::ActingProcessInner;
use crate::ompas::manager::resource::{Capacity, ClientId, ResourceId, WaitAcquire};
use std::fmt::{Display, Formatter};

#[derive()]
pub struct AcquireProcess {
    pub(crate) resource: ActingVarRef<String>,
    pub(crate) quantity: ActingVarRef<Capacity>,
    pub(crate) s_acq: ActingVarRef<Timepoint>,
    pub(crate) reservation: Option<WaitAcquire>,
    client_id: Option<ClientId>,
    resource_id: Option<ResourceId>,
}

impl AcquireProcess {
    pub fn new(
        resource: ActingVarRef<String>,
        quantity: ActingVarRef<usize>,
        s_acq: ActingVarRef<Timepoint>,
    ) -> Self {
        Self {
            resource,
            quantity,
            s_acq,
            reservation: None,
            client_id: None,
            resource_id: None,
        }
    }

    pub fn set_acquire_id(&mut self, waiter: &WaitAcquire) {
        self.client_id = Some(waiter.get_client_id());
        self.resource_id = Some(waiter.get_resource_id());
    }

    pub fn get_client_id(&self) -> Option<ClientId> {
        self.client_id
    }

    pub fn get_resource_id(&self) -> Option<ResourceId> {
        self.resource_id
    }

    pub fn set_reservation(&mut self, waiter: WaitAcquire) {
        self.set_acquire_id(&waiter);
        self.reservation = Some(waiter)
    }

    pub fn move_reservation(&mut self) -> Option<WaitAcquire> {
        self.reservation.take()
    }
}

impl From<AcquireProcess> for ActingProcessInner {
    fn from(value: AcquireProcess) -> Self {
        Self::Acquire(value)
    }
}

impl Display for AcquireProcess {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "")
    }
}
