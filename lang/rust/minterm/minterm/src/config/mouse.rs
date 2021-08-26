use minterm_config_derive::ConfigDeserialize;

#[derive(ConfigDeserialize, Default, Clone, Debug, PartialEq, Eq)]
pub struct Mouse {
    pub hide_when_typing: bool,
    #[config(deprecated = "use `hints` section instead")]
    pub url: Option<serde_yaml::Value>,
}

#[derive(ConfigDeserialize, Clone, Debug, PartialEq, Eq)]
pub struct ClickHandler {
    threshold: u16,
}

impl Default for ClickHandler {
    fn default() -> Self {
        Self { threshold: 300 }
    }
}

impl ClickHandler {
}
