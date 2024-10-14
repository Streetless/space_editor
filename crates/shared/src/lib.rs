use bevy::prelude::*;
use std::fmt::Display;

pub mod ext {
    pub use bevy_inspector_egui;
    // pub use egui_file;
    pub use rfd;
}

pub mod prelude {
    pub use crate::{
        EditorCameraMarker, EditorEvent, EditorPrefabPath, EditorSet, EditorState, PrefabMarker,
        PrefabMemoryCache, SelectParent,
    };
}

pub mod asset_fs;
pub(crate) mod gizmos;
pub mod toast;

/// Component Marker to display entity in Editor
#[derive(Component, Default, Clone, Reflect)]
#[reflect(Component)]
pub struct PrefabMarker;

/// Component marker that manages editor only camera
/// A camera tagged with this component will not be in use during playmode
#[derive(Component, Default, Clone, Reflect)]
#[reflect(Component)]
pub struct EditorCameraMarker;

/// Editor states (`Editor`, `GamePrepare`, `Game`)
#[derive(States, Default, Debug, Clone, Hash, Eq, PartialEq)]
pub enum EditorState {
    /// Editor Loading assets
    Loading,
    /// Displays Editor / Editor mode
    Editor,
    /// Editor is loading the game
    GamePrepare,
    /// Play mode, game is being executed
    #[default]
    Game,
}

/// Sets for separate game and editor logic
#[derive(SystemSet, Hash, Eq, PartialEq, Clone, Debug)]
pub enum EditorSet {
    /// Editor mode System Set
    Editor,
    /// Play mode System Set
    Game,
}

#[derive(Resource, Default)]
pub struct PrefabMemoryCache {
    pub scene: Option<Handle<DynamicScene>>,
}

#[derive(Clone, Debug)]
/// How/Where porefab data is stored
pub enum EditorPrefabPath {
    File(String),
    MemoryCache,
}

#[derive(Event)]
pub enum EditorEvent {
    Load(EditorPrefabPath),
    Save(EditorPrefabPath),
    Export(EditorPrefabPath),
    CreateBundle(EditorPrefabPath),
    LoadGltfAsPrefab(String),
    StartGame,
}

/// Component that makes the parent become selected when this mesh is.
/// Stores the parent entity for reading.
/// Must be not be paired with an entity that has PrefabMarker.
#[derive(Component)]
pub struct SelectParent {
    pub parent: Entity,
}

/// Component that determines if the light gizmo shall be visualized
#[derive(Component, Default, Clone, Reflect)]
#[reflect(Component)]
pub struct LightAreaToggle(pub bool);

pub enum FileType {
    Scene,
    Bundle,
    GLB,
    GLTF,
}

impl Display for FileType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            FileType::Scene => "scn.ron".to_string(),
            FileType::Bundle => "bundle.ron".to_string(),
            FileType::GLB => "glb".to_string(),
            FileType::GLTF => "gltf".to_string(),
        };
        write!(f, "{}", str)
    }
}

// impl From<FileType> for &'static str {
//     fn from(file_type: FileType) -> Self {
//         match file_type {
//             FileType::Scene => "scn.ron",
//             FileType::Bundle => "bundle.ron",
//             FileType::GLB => "glb",
//             FileType::GLTF => "gltf",
//         }
//     }
// }