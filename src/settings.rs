

pub struct Settings {
    pub tab_width: usize,
    pub stop_on_initial_exec: bool,
    pub fps: f64,
    pub loading_fps: f64, // how often to re-render when there's a progress bar on screen
    pub max_threads: usize,
    pub save_period_seconds: f64, // how often to save state to file (watches, breakpoints, etc); also saved on clean exit (not on panic or crash)

    pub stdin_file: Option<String>,
    pub stdout_file: Option<String>,
}

impl Default for Settings {
    fn default() -> Self { Settings {
        tab_width: 2,
        stop_on_initial_exec: true,
        fps: 144.0,
        loading_fps: 10.0,
        max_threads: 128,
        save_period_seconds: 1.0,

        stdin_file: None,
        stdout_file: None,
    } }
}
