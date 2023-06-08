use crate::ompas::manager::acting::acting_var::{ActingValUpdate, ExecutionVar};
use crate::ompas::manager::acting::interval::Timepoint;
use crate::ompas::manager::acting::process::ActingProcessInner;
use crate::ompas::manager::resource::WaitAcquire;
use std::fmt::{Display, Formatter};

#[derive()]
pub struct AcquireProcess {
    pub(crate) resource: ExecutionVar<String>,
    pub(crate) quantity: ExecutionVar<usize>,
    pub(crate) s_acq: ExecutionVar<Timepoint>,
    pub(crate) reservation: Option<WaitAcquire>,
    waiter_id: usize,
    resource_id: usize,
}

impl AcquireProcess {
    pub fn new(
        resource: ExecutionVar<String>,
        quantity: ExecutionVar<usize>,
        s_acq: ExecutionVar<Timepoint>,
    ) -> Self {
        Self {
            resource,
            quantity,
            s_acq,
            reservation: None,
            waiter_id: 0,
            resource_id: 0,
        }
    }

    pub fn set_resource(&mut self, resource: String) -> Option<ActingValUpdate> {
        self.resource.set_val(resource)
    }

    pub fn set_quantity(&mut self, quantity: usize) -> Option<ActingValUpdate> {
        self.quantity.set_val(quantity)
    }
    pub fn set_acquire_id(&mut self, waiter: &WaitAcquire) {
        self.waiter_id = waiter.get_client_id();
        self.resource_id = waiter.get_resource_id();
    }

    pub fn set_reservation(&mut self, waiter: WaitAcquire) {
        self.set_acquire_id(&waiter);
        self.reservation = Some(waiter)
    }

    pub fn move_reservation(&mut self) -> Option<WaitAcquire> {
        self.reservation.take()
        //mem::replace(&mut self.reservation, None)
    }

    pub fn set_s_acq(&mut self, val: Timepoint) -> Option<ActingValUpdate> {
        self.s_acq.set_val(val)
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
