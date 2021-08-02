#![allow(clippy::enum_glob_use)]

use std::fmt::{self, Debug, Display};

use bitflags::bitflags;
use glutin::event::VirtualKeyCode::*;
use glutin::event::{ModifiersState, MouseButton, VirtualKeyCode};
use serde::de::Error as SerdeError;
use serde::de::{self, MapAccess, Unexpected, Visitor};
use serde::{Deserialize, Deserializer};
use serde_yaml::Value as SerdeValue;

use minterm_config_derive::ConfigDeserialize;

use minterm_terminal::config::Program;
use minterm_terminal::term::TermMode;

/// Describes a state and action to take in that state.
///
/// This is the shared component of `MouseBinding` and `KeyBinding`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Binding<T> {
    /// Modifier keys required to activate binding.
    pub mods: ModifiersState,

    /// String to send to PTY if mods and mode match.
    pub action: Action,

    /// Binding mode required to activate binding.
    pub mode: BindingMode,

    /// Excluded binding modes where the binding won't be activated.
    pub notmode: BindingMode,

    /// This property is used as part of the trigger detection code.
    ///
    /// For example, this might be a key like "G", or a mouse button.
    pub trigger: T,
}

/// Bindings that are triggered by a keyboard key.
pub type KeyBinding = Binding<Key>;

/// Bindings that are triggered by a mouse button.
pub type MouseBinding = Binding<MouseButton>;

impl<T: Eq> Binding<T> {
    #[inline]
    pub fn is_triggered_by(&self, mode: BindingMode, mods: ModifiersState, input: &T) -> bool {
        // Check input first since bindings are stored in one big list. This is
        // the most likely item to fail so prioritizing it here allows more
        // checks to be short circuited.
        self.trigger == *input
            && self.mods == mods
            && mode.contains(self.mode)
            && !mode.intersects(self.notmode)
    }

    #[inline]
    pub fn triggers_match(&self, binding: &Binding<T>) -> bool {
        // Check the binding's key and modifiers.
        if self.trigger != binding.trigger || self.mods != binding.mods {
            return false;
        }

        let selfmode = if self.mode.is_empty() { BindingMode::all() } else { self.mode };
        let bindingmode = if binding.mode.is_empty() { BindingMode::all() } else { binding.mode };

        if !selfmode.intersects(bindingmode) {
            return false;
        }

        // The bindings are never active at the same time when the required modes of one binding
        // are part of the forbidden bindings of the other.
        if self.mode.intersects(binding.notmode) || binding.mode.intersects(self.notmode) {
            return false;
        }

        true
    }
}

#[derive(ConfigDeserialize, Debug, Clone, PartialEq, Eq)]
pub enum Action {
    /// Write an escape sequence.
    #[config(skip)]
    Esc(String),

    /// Run given command.
    #[config(skip)]
    Command(Program),

    /// Clear warning and error notices.
    ClearLogNotice,

    /// Allow receiving char input.
    ReceiveChar,

    /// No action.
    None,
}

impl From<&'static str> for Action {
    fn from(s: &'static str) -> Action {
        Action::Esc(s.into())
    }
}

/// Display trait used for error logging.
impl Display for Action {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            _ => write!(f, "{:?}", self),
        }
    }
}

macro_rules! bindings {
    (
        KeyBinding;
        $(
            $key:ident
            $(,$mods:expr)*
            $(,+$mode:expr)*
            $(,~$notmode:expr)*
            ;$action:expr
        );*
        $(;)*
    ) => {{
        bindings!(
            KeyBinding;
            $(
                Key::Keycode($key)
                $(,$mods)*
                $(,+$mode)*
                $(,~$notmode)*
                ;$action
            );*
        )
    }};
    (
        $ty:ident;
        $(
            $key:expr
            $(,$mods:expr)*
            $(,+$mode:expr)*
            $(,~$notmode:expr)*
            ;$action:expr
        );*
        $(;)*
    ) => {{
        let mut v = Vec::new();

        $(
            let mut _mods = ModifiersState::empty();
            $(_mods = $mods;)*
            let mut _mode = BindingMode::empty();
            $(_mode.insert($mode);)*
            let mut _notmode = BindingMode::empty();
            $(_notmode.insert($notmode);)*

            v.push($ty {
                trigger: $key,
                mods: _mods,
                mode: _mode,
                notmode: _notmode,
                action: $action.into(),
            });
        )*

        v
    }};
}

pub fn default_mouse_bindings() -> Vec<MouseBinding> {
    bindings!(
        MouseBinding;
    )
}

pub fn default_key_bindings() -> Vec<KeyBinding> {
    let mut bindings = bindings!(
        KeyBinding;
        L, ModifiersState::CTRL; Action::ClearLogNotice;
        L,    ModifiersState::CTRL; Action::Esc("\x0c".into());
        Up,    +BindingMode::APP_CURSOR; Action::Esc("\x1bOA".into());
        Up,    ~BindingMode::APP_CURSOR; Action::Esc("\x1b[A".into());
        Down,  +BindingMode::APP_CURSOR; Action::Esc("\x1bOB".into());
        Down,  ~BindingMode::APP_CURSOR; Action::Esc("\x1b[B".into());
        Right, +BindingMode::APP_CURSOR; Action::Esc("\x1bOC".into());
        Right, ~BindingMode::APP_CURSOR; Action::Esc("\x1b[C".into());
        Left,  +BindingMode::APP_CURSOR; Action::Esc("\x1bOD".into());
        Left,  ~BindingMode::APP_CURSOR; Action::Esc("\x1b[D".into());
        Back        ; Action::Esc("\x7f".into());
        Insert      ; Action::Esc("\x1b[2~".into());
        Delete      ; Action::Esc("\x1b[3~".into());
        PageUp      ; Action::Esc("\x1b[5~".into());
        PageDown    ; Action::Esc("\x1b[6~".into());
        F1          ; Action::Esc("\x1bOP".into());
        F2          ; Action::Esc("\x1bOQ".into());
        F3          ; Action::Esc("\x1bOR".into());
        F4          ; Action::Esc("\x1bOS".into());
        F5          ; Action::Esc("\x1b[15~".into());
        F6          ; Action::Esc("\x1b[17~".into());
        F7          ; Action::Esc("\x1b[18~".into());
        F8          ; Action::Esc("\x1b[19~".into());
        F9          ; Action::Esc("\x1b[20~".into());
        F10         ; Action::Esc("\x1b[21~".into());
        F11         ; Action::Esc("\x1b[23~".into());
        F12         ; Action::Esc("\x1b[24~".into());
        F13         ; Action::Esc("\x1b[25~".into());
        F14         ; Action::Esc("\x1b[26~".into());
        F15         ; Action::Esc("\x1b[28~".into());
        F16         ; Action::Esc("\x1b[29~".into());
        F17         ; Action::Esc("\x1b[31~".into());
        F18         ; Action::Esc("\x1b[32~".into());
        F19         ; Action::Esc("\x1b[33~".into());
        F20         ; Action::Esc("\x1b[34~".into());
        NumpadEnter ; Action::Esc("\n".into());
    );

    //   Code     Modifiers
    // ---------+---------------------------
    //    2     | Shift
    //    3     | Alt
    //    4     | Shift + Alt
    //    5     | Control
    //    6     | Shift + Control
    //    7     | Alt + Control
    //    8     | Shift + Alt + Control
    // ---------+---------------------------
    //
    // from: https://invisible-island.net/xterm/ctlseqs/ctlseqs.html#h2-PC-Style-Function-Keys
    let mut modifiers = vec![
        ModifiersState::SHIFT,
        ModifiersState::ALT,
        ModifiersState::SHIFT | ModifiersState::ALT,
        ModifiersState::CTRL,
        ModifiersState::SHIFT | ModifiersState::CTRL,
        ModifiersState::ALT | ModifiersState::CTRL,
        ModifiersState::SHIFT | ModifiersState::ALT | ModifiersState::CTRL,
    ];

    for (index, mods) in modifiers.drain(..).enumerate() {
        let modifiers_code = index + 2;
        bindings.extend(bindings!(
            KeyBinding;
            Delete, mods; Action::Esc(format!("\x1b[3;{}~", modifiers_code));
            Up,     mods; Action::Esc(format!("\x1b[1;{}A", modifiers_code));
            Down,   mods; Action::Esc(format!("\x1b[1;{}B", modifiers_code));
            Right,  mods; Action::Esc(format!("\x1b[1;{}C", modifiers_code));
            Left,   mods; Action::Esc(format!("\x1b[1;{}D", modifiers_code));
            F1,     mods; Action::Esc(format!("\x1b[1;{}P", modifiers_code));
            F2,     mods; Action::Esc(format!("\x1b[1;{}Q", modifiers_code));
            F3,     mods; Action::Esc(format!("\x1b[1;{}R", modifiers_code));
            F4,     mods; Action::Esc(format!("\x1b[1;{}S", modifiers_code));
            F5,     mods; Action::Esc(format!("\x1b[15;{}~", modifiers_code));
            F6,     mods; Action::Esc(format!("\x1b[17;{}~", modifiers_code));
            F7,     mods; Action::Esc(format!("\x1b[18;{}~", modifiers_code));
            F8,     mods; Action::Esc(format!("\x1b[19;{}~", modifiers_code));
            F9,     mods; Action::Esc(format!("\x1b[20;{}~", modifiers_code));
            F10,    mods; Action::Esc(format!("\x1b[21;{}~", modifiers_code));
            F11,    mods; Action::Esc(format!("\x1b[23;{}~", modifiers_code));
            F12,    mods; Action::Esc(format!("\x1b[24;{}~", modifiers_code));
            F13,    mods; Action::Esc(format!("\x1b[25;{}~", modifiers_code));
            F14,    mods; Action::Esc(format!("\x1b[26;{}~", modifiers_code));
            F15,    mods; Action::Esc(format!("\x1b[28;{}~", modifiers_code));
            F16,    mods; Action::Esc(format!("\x1b[29;{}~", modifiers_code));
            F17,    mods; Action::Esc(format!("\x1b[31;{}~", modifiers_code));
            F18,    mods; Action::Esc(format!("\x1b[32;{}~", modifiers_code));
            F19,    mods; Action::Esc(format!("\x1b[33;{}~", modifiers_code));
            F20,    mods; Action::Esc(format!("\x1b[34;{}~", modifiers_code));
        ));

        // We're adding the following bindings with `Shift` manually above, so skipping them here.
        if modifiers_code != 2 {
            bindings.extend(bindings!(
                KeyBinding;
                Insert,   mods; Action::Esc(format!("\x1b[2;{}~", modifiers_code));
                PageUp,   mods; Action::Esc(format!("\x1b[5;{}~", modifiers_code));
                PageDown, mods; Action::Esc(format!("\x1b[6;{}~", modifiers_code));
                End,      mods; Action::Esc(format!("\x1b[1;{}F", modifiers_code));
                Home,     mods; Action::Esc(format!("\x1b[1;{}H", modifiers_code));
            ));
        }
    }

    bindings.extend(platform_key_bindings());

    bindings
}

#[cfg(not(test))]
pub fn platform_key_bindings() -> Vec<KeyBinding> {
    bindings!(
        KeyBinding;
        Insert, ModifiersState::SHIFT; Action::Esc("\x1b[2;2~".into());
        K, ModifiersState::LOGO; Action::Esc("\x0c".into());
    )
}

// Don't return any bindings for tests since they are commented-out by default.
#[cfg(test)]
pub fn platform_key_bindings() -> Vec<KeyBinding> {
    vec![]
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum Key {
    Scancode(u32),
    Keycode(VirtualKeyCode),
}

impl<'a> Deserialize<'a> for Key {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'a>,
    {
        let value = SerdeValue::deserialize(deserializer)?;
        match u32::deserialize(value.clone()) {
            Ok(scancode) => Ok(Key::Scancode(scancode)),
            Err(_) => {
                let keycode = VirtualKeyCode::deserialize(value).map_err(D::Error::custom)?;
                Ok(Key::Keycode(keycode))
            },
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct ModeWrapper {
    pub mode: BindingMode,
    pub not_mode: BindingMode,
}

bitflags! {
    /// Modes available for key bindings.
    pub struct BindingMode: u8 {
        const APP_CURSOR          = 0b0000_0001;
        const APP_KEYPAD          = 0b0000_0010;
        const ALT_SCREEN          = 0b0000_0100;
    }
}

impl BindingMode {
    pub fn new(mode: &TermMode) -> BindingMode {
        let mut binding_mode = BindingMode::empty();
        binding_mode.set(BindingMode::APP_CURSOR, mode.contains(TermMode::APP_CURSOR));
        binding_mode.set(BindingMode::APP_KEYPAD, mode.contains(TermMode::APP_KEYPAD));
        binding_mode.set(BindingMode::ALT_SCREEN, mode.contains(TermMode::ALT_SCREEN));
        binding_mode
    }
}

impl Default for ModeWrapper {
    fn default() -> Self {
        Self { mode: BindingMode::empty(), not_mode: BindingMode::empty() }
    }
}

impl<'a> Deserialize<'a> for ModeWrapper {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'a>,
    {
        struct ModeVisitor;

        impl<'a> Visitor<'a> for ModeVisitor {
            type Value = ModeWrapper;

            fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.write_str(
                    "a combination of AppCursor | AppKeypad | Alt",
                )
            }

            fn visit_str<E>(self, value: &str) -> Result<ModeWrapper, E>
            where
                E: de::Error,
            {
                let mut res =
                    ModeWrapper { mode: BindingMode::empty(), not_mode: BindingMode::empty() };

                for modifier in value.split('|') {
                    match modifier.trim().to_lowercase().as_str() {
                        "appcursor" => res.mode |= BindingMode::APP_CURSOR,
                        "~appcursor" => res.not_mode |= BindingMode::APP_CURSOR,
                        "appkeypad" => res.mode |= BindingMode::APP_KEYPAD,
                        "~appkeypad" => res.not_mode |= BindingMode::APP_KEYPAD,
                        "alt" => res.mode |= BindingMode::ALT_SCREEN,
                        "~alt" => res.not_mode |= BindingMode::ALT_SCREEN,
                        _ => return Err(E::invalid_value(Unexpected::Str(modifier), &self)),
                    }
                }

                Ok(res)
            }
        }
        deserializer.deserialize_str(ModeVisitor)
    }
}

struct MouseButtonWrapper(MouseButton);

impl MouseButtonWrapper {
    fn into_inner(self) -> MouseButton {
        self.0
    }
}

impl<'a> Deserialize<'a> for MouseButtonWrapper {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'a>,
    {
        struct MouseButtonVisitor;

        impl<'a> Visitor<'a> for MouseButtonVisitor {
            type Value = MouseButtonWrapper;

            fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.write_str("Left, Right, Middle, or a number from 0 to 65536")
            }

            fn visit_u64<E>(self, value: u64) -> Result<MouseButtonWrapper, E>
            where
                E: de::Error,
            {
                match value {
                    0..=65536 => Ok(MouseButtonWrapper(MouseButton::Other(value as u16))),
                    _ => Err(E::invalid_value(Unexpected::Unsigned(value), &self)),
                }
            }

            fn visit_str<E>(self, value: &str) -> Result<MouseButtonWrapper, E>
            where
                E: de::Error,
            {
                match value {
                    "Left" => Ok(MouseButtonWrapper(MouseButton::Left)),
                    "Right" => Ok(MouseButtonWrapper(MouseButton::Right)),
                    "Middle" => Ok(MouseButtonWrapper(MouseButton::Middle)),
                    _ => Err(E::invalid_value(Unexpected::Str(value), &self)),
                }
            }
        }

        deserializer.deserialize_any(MouseButtonVisitor)
    }
}

/// Bindings are deserialized into a `RawBinding` before being parsed as a
/// `KeyBinding` or `MouseBinding`.
#[derive(PartialEq, Eq)]
struct RawBinding {
    key: Option<Key>,
    mouse: Option<MouseButton>,
    mods: ModifiersState,
    mode: BindingMode,
    notmode: BindingMode,
    action: Action,
}

impl RawBinding {
    fn into_mouse_binding(self) -> Result<MouseBinding, Self> {
        if let Some(mouse) = self.mouse {
            Ok(Binding {
                trigger: mouse,
                mods: self.mods,
                action: self.action,
                mode: self.mode,
                notmode: self.notmode,
            })
        } else {
            Err(self)
        }
    }

    fn into_key_binding(self) -> Result<KeyBinding, Self> {
        if let Some(key) = self.key {
            Ok(KeyBinding {
                trigger: key,
                mods: self.mods,
                action: self.action,
                mode: self.mode,
                notmode: self.notmode,
            })
        } else {
            Err(self)
        }
    }
}

impl<'a> Deserialize<'a> for RawBinding {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'a>,
    {
        const FIELDS: &[&str] = &["key", "mods", "mode", "action", "chars", "mouse", "command"];

        enum Field {
            Key,
            Mods,
            Mode,
            Action,
            Chars,
            Mouse,
            Command,
        }

        impl<'a> Deserialize<'a> for Field {
            fn deserialize<D>(deserializer: D) -> Result<Field, D::Error>
            where
                D: Deserializer<'a>,
            {
                struct FieldVisitor;

                impl<'a> Visitor<'a> for FieldVisitor {
                    type Value = Field;

                    fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                        f.write_str("binding fields")
                    }

                    fn visit_str<E>(self, value: &str) -> Result<Field, E>
                    where
                        E: de::Error,
                    {
                        match value.to_ascii_lowercase().as_str() {
                            "key" => Ok(Field::Key),
                            "mods" => Ok(Field::Mods),
                            "mode" => Ok(Field::Mode),
                            "action" => Ok(Field::Action),
                            "chars" => Ok(Field::Chars),
                            "mouse" => Ok(Field::Mouse),
                            "command" => Ok(Field::Command),
                            _ => Err(E::unknown_field(value, FIELDS)),
                        }
                    }
                }

                deserializer.deserialize_str(FieldVisitor)
            }
        }

        struct RawBindingVisitor;
        impl<'a> Visitor<'a> for RawBindingVisitor {
            type Value = RawBinding;

            fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.write_str("binding specification")
            }

            fn visit_map<V>(self, mut map: V) -> Result<RawBinding, V::Error>
            where
                V: MapAccess<'a>,
            {
                let mut mods: Option<ModifiersState> = None;
                let mut key: Option<Key> = None;
                let mut chars: Option<String> = None;
                let mut action: Option<Action> = None;
                let mut mode: Option<BindingMode> = None;
                let mut not_mode: Option<BindingMode> = None;
                let mut mouse: Option<MouseButton> = None;
                let mut command: Option<Program> = None;

                use de::Error;

                while let Some(struct_key) = map.next_key::<Field>()? {
                    match struct_key {
                        Field::Key => {
                            if key.is_some() {
                                return Err(<V::Error as Error>::duplicate_field("key"));
                            }

                            let val = map.next_value::<SerdeValue>()?;
                            if val.is_u64() {
                                let scancode = val.as_u64().unwrap();
                                if scancode > u64::from(std::u32::MAX) {
                                    return Err(<V::Error as Error>::custom(format!(
                                        "Invalid key binding, scancode too big: {}",
                                        scancode
                                    )));
                                }
                                key = Some(Key::Scancode(scancode as u32));
                            } else {
                                let k = Key::deserialize(val).map_err(V::Error::custom)?;
                                key = Some(k);
                            }
                        },
                        Field::Mods => {
                            if mods.is_some() {
                                return Err(<V::Error as Error>::duplicate_field("mods"));
                            }

                            mods = Some(map.next_value::<ModsWrapper>()?.into_inner());
                        },
                        Field::Mode => {
                            if mode.is_some() {
                                return Err(<V::Error as Error>::duplicate_field("mode"));
                            }

                            let mode_deserializer = map.next_value::<ModeWrapper>()?;
                            mode = Some(mode_deserializer.mode);
                            not_mode = Some(mode_deserializer.not_mode);
                        },
                        Field::Action => {
                            if action.is_some() {
                                return Err(<V::Error as Error>::duplicate_field("action"));
                            }

                            let value = map.next_value::<SerdeValue>()?;

                            action = match Action::deserialize(value.clone()).map_err(V::Error::custom) {
                                    Ok(action) => Some(action),
                                    Err(err) => {
                                        let value = match value {
                                            SerdeValue::String(string) => string,
                                            SerdeValue::Mapping(map) if map.len() == 1 => {
                                                match map.into_iter().next() {
                                                    Some((
                                                        SerdeValue::String(string),
                                                        SerdeValue::Null,
                                                    )) => string,
                                                    _ => return Err(err),
                                                }
                                            },
                                            _ => return Err(err),
                                        };
                                        return Err(V::Error::custom(format!(
                                            "unknown keyboard action `{}`",
                                            value
                                        )));
                                    },
                                }
                        },
                        Field::Chars => {
                            if chars.is_some() {
                                return Err(<V::Error as Error>::duplicate_field("chars"));
                            }

                            chars = Some(map.next_value()?);
                        },
                        Field::Mouse => {
                            if chars.is_some() {
                                return Err(<V::Error as Error>::duplicate_field("mouse"));
                            }

                            mouse = Some(map.next_value::<MouseButtonWrapper>()?.into_inner());
                        },
                        Field::Command => {
                            if command.is_some() {
                                return Err(<V::Error as Error>::duplicate_field("command"));
                            }

                            command = Some(map.next_value::<Program>()?);
                        },
                    }
                }

                let mode = mode.unwrap_or_else(BindingMode::empty);
                let not_mode = not_mode.unwrap_or_else(BindingMode::empty);
                let mods = mods.unwrap_or_else(ModifiersState::default);

                let action = match (action, chars, command) {
                    (Some(action), None, None) => action,
                    (None, Some(chars), None) => Action::Esc(chars),
                    (None, None, Some(cmd)) => Action::Command(cmd),
                    _ => {
                        return Err(V::Error::custom(
                            "must specify exactly one of chars, action or command",
                        ))
                    },
                };

                if mouse.is_none() && key.is_none() {
                    return Err(V::Error::custom("bindings require mouse button or key"));
                }

                Ok(RawBinding { mode, notmode: not_mode, action, key, mouse, mods })
            }
        }

        deserializer.deserialize_struct("RawBinding", FIELDS, RawBindingVisitor)
    }
}

impl<'a> Deserialize<'a> for MouseBinding {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'a>,
    {
        let raw = RawBinding::deserialize(deserializer)?;
        raw.into_mouse_binding()
            .map_err(|_| D::Error::custom("expected mouse binding, got key binding"))
    }
}

impl<'a> Deserialize<'a> for KeyBinding {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'a>,
    {
        let raw = RawBinding::deserialize(deserializer)?;
        raw.into_key_binding()
            .map_err(|_| D::Error::custom("expected key binding, got mouse binding"))
    }
}

/// Newtype for implementing deserialize on glutin Mods.
///
/// Our deserialize impl wouldn't be covered by a derive(Deserialize); see the
/// impl below.
#[derive(Debug, Copy, Clone, Hash, Default, Eq, PartialEq)]
pub struct ModsWrapper(pub ModifiersState);

impl ModsWrapper {
    pub fn into_inner(self) -> ModifiersState {
        self.0
    }
}

impl<'a> de::Deserialize<'a> for ModsWrapper {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: de::Deserializer<'a>,
    {
        struct ModsVisitor;

        impl<'a> Visitor<'a> for ModsVisitor {
            type Value = ModsWrapper;

            fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.write_str("None or a subset of Shift|Control|Super|Command|Alt|Option")
            }

            fn visit_str<E>(self, value: &str) -> Result<ModsWrapper, E>
            where
                E: de::Error,
            {
                let mut res = ModifiersState::empty();
                for modifier in value.split('|') {
                    match modifier.trim().to_lowercase().as_str() {
                        "command" | "super" => res.insert(ModifiersState::LOGO),
                        "shift" => res.insert(ModifiersState::SHIFT),
                        "alt" | "option" => res.insert(ModifiersState::ALT),
                        "control" => res.insert(ModifiersState::CTRL),
                        "none" => (),
                        _ => return Err(E::invalid_value(Unexpected::Str(modifier), &self)),
                    }
                }

                Ok(ModsWrapper(res))
            }
        }

        deserializer.deserialize_str(ModsVisitor)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use glutin::event::ModifiersState;

    type MockBinding = Binding<usize>;

    impl Default for MockBinding {
        fn default() -> Self {
            Self {
                mods: Default::default(),
                action: Action::None,
                mode: BindingMode::empty(),
                notmode: BindingMode::empty(),
                trigger: Default::default(),
            }
        }
    }

    #[test]
    fn binding_matches_itself() {
        let binding = MockBinding::default();
        let identical_binding = MockBinding::default();

        assert!(binding.triggers_match(&identical_binding));
        assert!(identical_binding.triggers_match(&binding));
    }

    #[test]
    fn binding_matches_different_action() {
        let binding = MockBinding::default();
        let different_action =
            MockBinding { action: Action::ClearLogNotice, ..MockBinding::default() };

        assert!(binding.triggers_match(&different_action));
        assert!(different_action.triggers_match(&binding));
    }

    #[test]
    fn mods_binding_requires_strict_match() {
        let superset_mods = MockBinding { mods: ModifiersState::all(), ..MockBinding::default() };
        let subset_mods = MockBinding { mods: ModifiersState::ALT, ..MockBinding::default() };

        assert!(!superset_mods.triggers_match(&subset_mods));
        assert!(!subset_mods.triggers_match(&superset_mods));
    }

    #[test]
    fn binding_matches_identical_mode() {
        let b1 = MockBinding { mode: BindingMode::ALT_SCREEN, ..MockBinding::default() };
        let b2 = MockBinding { mode: BindingMode::ALT_SCREEN, ..MockBinding::default() };

        assert!(b1.triggers_match(&b2));
        assert!(b2.triggers_match(&b1));
    }

    #[test]
    fn binding_without_mode_matches_any_mode() {
        let b1 = MockBinding::default();
        let b2 = MockBinding {
            mode: BindingMode::APP_KEYPAD,
            notmode: BindingMode::ALT_SCREEN,
            ..MockBinding::default()
        };

        assert!(b1.triggers_match(&b2));
    }

    #[test]
    fn binding_with_mode_matches_empty_mode() {
        let b1 = MockBinding {
            mode: BindingMode::APP_KEYPAD,
            notmode: BindingMode::ALT_SCREEN,
            ..MockBinding::default()
        };
        let b2 = MockBinding::default();

        assert!(b1.triggers_match(&b2));
        assert!(b2.triggers_match(&b1));
    }

    #[test]
    fn binding_matches_modes() {
        let b1 = MockBinding {
            mode: BindingMode::ALT_SCREEN | BindingMode::APP_KEYPAD,
            ..MockBinding::default()
        };
        let b2 = MockBinding { mode: BindingMode::APP_KEYPAD, ..MockBinding::default() };

        assert!(b1.triggers_match(&b2));
        assert!(b2.triggers_match(&b1));
    }

    #[test]
    fn binding_matches_partial_intersection() {
        let b1 = MockBinding {
            mode: BindingMode::ALT_SCREEN | BindingMode::APP_KEYPAD,
            ..MockBinding::default()
        };
        let b2 = MockBinding {
            mode: BindingMode::APP_KEYPAD | BindingMode::APP_CURSOR,
            ..MockBinding::default()
        };

        assert!(b1.triggers_match(&b2));
        assert!(b2.triggers_match(&b1));
    }

    #[test]
    fn binding_mismatches_notmode() {
        let b1 = MockBinding { mode: BindingMode::ALT_SCREEN, ..MockBinding::default() };
        let b2 = MockBinding { notmode: BindingMode::ALT_SCREEN, ..MockBinding::default() };

        assert!(!b1.triggers_match(&b2));
        assert!(!b2.triggers_match(&b1));
    }

    #[test]
    fn binding_mismatches_unrelated() {
        let b1 = MockBinding { mode: BindingMode::ALT_SCREEN, ..MockBinding::default() };
        let b2 = MockBinding { mode: BindingMode::APP_KEYPAD, ..MockBinding::default() };

        assert!(!b1.triggers_match(&b2));
        assert!(!b2.triggers_match(&b1));
    }

    #[test]
    fn binding_matches_notmodes() {
        let subset_notmodes = MockBinding {
            notmode: BindingMode::APP_CURSOR,
            ..MockBinding::default()
        };
        let superset_notmodes =
            MockBinding { notmode: BindingMode::APP_CURSOR, ..MockBinding::default() };

        assert!(subset_notmodes.triggers_match(&superset_notmodes));
        assert!(superset_notmodes.triggers_match(&subset_notmodes));
    }

    #[test]
    fn binding_matches_mode_notmode() {
        let b1 = MockBinding {
            mode: BindingMode::APP_KEYPAD,
            notmode: BindingMode::APP_CURSOR,
            ..MockBinding::default()
        };
        let b2 = MockBinding { notmode: BindingMode::APP_CURSOR, ..MockBinding::default() };

        assert!(b1.triggers_match(&b2));
        assert!(b2.triggers_match(&b1));
    }

    #[test]
    fn binding_trigger_input() {
        let binding = MockBinding { trigger: 13, ..MockBinding::default() };

        let mods = binding.mods;
        let mode = binding.mode;

        assert!(binding.is_triggered_by(mode, mods, &13));
        assert!(!binding.is_triggered_by(mode, mods, &32));
    }

    #[test]
    fn binding_trigger_mods() {
        let binding = MockBinding {
            mods: ModifiersState::ALT | ModifiersState::LOGO,
            ..MockBinding::default()
        };

        let superset_mods = ModifiersState::all();
        let subset_mods = ModifiersState::empty();

        let t = binding.trigger;
        let mode = binding.mode;

        assert!(binding.is_triggered_by(mode, binding.mods, &t));
        assert!(!binding.is_triggered_by(mode, superset_mods, &t));
        assert!(!binding.is_triggered_by(mode, subset_mods, &t));
    }

    #[test]
    fn binding_trigger_modes() {
        let binding = MockBinding { mode: BindingMode::ALT_SCREEN, ..MockBinding::default() };

        let t = binding.trigger;
        let mods = binding.mods;

        assert!(binding.is_triggered_by(BindingMode::ALT_SCREEN, mods, &t));
        assert!(binding.is_triggered_by(BindingMode::ALT_SCREEN, mods, &t));
    }

    #[test]
    fn binding_trigger_notmodes() {
        let binding = MockBinding { notmode: BindingMode::ALT_SCREEN, ..MockBinding::default() };

        let t = binding.trigger;
        let mods = binding.mods;

        assert!(!binding.is_triggered_by(BindingMode::ALT_SCREEN, mods, &t));
        assert!(!binding.is_triggered_by(BindingMode::ALT_SCREEN, mods, &t));
    }
}
