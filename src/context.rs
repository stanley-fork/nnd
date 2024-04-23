use crate::{*, executor::*, settings::*, util::*};
use std::{sync::Arc};

pub struct Context {
    pub settings: Settings,
    pub executor: Executor,
    // Wakes up the main thread, triggering a UI update. E.g. async search widget uses it to notify the UI of completion.
    pub wake_main_thread: Arc<EventFD>,
}

impl Context {
    pub fn invalid() -> Arc<Self> { Arc::new(Self {settings: Settings::default(), executor: Executor::invalid(), wake_main_thread: Arc::new(EventFD::new())}) }
}
