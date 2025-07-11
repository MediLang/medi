pub mod parser;

#[cfg(test)]
mod tests {
    use env_logger::Builder;
    use log::LevelFilter;
    use std::io::Write;
    use std::sync::Once;

    static INIT: Once = Once::new();

    /// Initialize the logger for tests
    pub fn init_test_logger() {
        INIT.call_once(|| {
            Builder::new()
                .filter_level(LevelFilter::Debug)
                .format(|buf, record| {
                    writeln!(
                        buf,
                        "[{}] {}: {}",
                        record.level(),
                        record.target(),
                        record.args()
                    )
                })
                .init();
            log::info!("Test logger initialized");
        });
    }
}

// Integration tests are in the tests/ directory
