use crate::{*, executor::*, settings::*};
use std::{sync::Arc};

pub struct Context {
    pub settings: Settings,
    pub executor: Executor,
}

impl Context {
    pub fn invalid() -> Arc<Self> { Arc::new(Self {settings: Settings::default(), executor: Executor::invalid()}) }
}
