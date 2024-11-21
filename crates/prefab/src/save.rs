use bevy::{
    ecs::{entity::MapEntities, reflect::ReflectMapEntities},
    prelude::*,
    tasks::IoTaskPool,
    utils::HashSet,
};
use space_shared::{EditorPrefabPath, PrefabMarker, PrefabMemoryCache};
use std::{any::TypeId, fs, io::Write};
use bevy::reflect::serde::TypedReflectSerializer;
use bevy::reflect::TypeRegistry;
use bevy::scene::DynamicEntity;
use bevy::scene::serde::{ENTITY_FIELD_COMPONENTS, ENTITY_STRUCT, SCENE_ENTITIES, SCENE_STRUCT};
use ron::Serializer;
use serde::ser::{SerializeMap, SerializeStruct, SerializeTuple};
use serde::Serialize;
use crate::prelude::{EditorRegistry, EditorRegistryExt, SceneAutoChild};
use std::fmt::Display;

#[derive(Reflect, Default, Component, Clone)]
#[reflect(Component, MapEntities)]
/// Component that holds children entity/prefab information
/// that should be serialized
pub struct ChildrenPrefab(pub Vec<Entity>);

impl ChildrenPrefab {
    pub fn from_children(children: &Children) -> Self {
        Self(children.to_vec())
    }
}

impl MapEntities for ChildrenPrefab {
    #[cfg(not(tarpaulin_include))]
    fn map_entities<M: EntityMapper>(&mut self, entity_mapper: &mut M) {
        self.0 = self
            .0
            .iter()
            .map(|e| entity_mapper.map_entity(*e))
            .collect();
    }
}

#[derive(Component)]
pub struct BundleEntity;

struct SaveResourcesPrefabPlugin;

impl Plugin for SaveResourcesPrefabPlugin {
    fn build(&self, app: &mut App) {
        app.editor_registry::<ChildrenPrefab>();

        app.init_resource::<SaveConfig>().init_state::<SaveState>();
    }
}

pub struct SavePrefabPlugin;

#[derive(Clone, Default, Reflect)]
enum CollisionType {
    #[default]
    Auto,
    // Static,
    // Dynamic,
    // Kinetic,
}

impl Display for CollisionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CollisionType::Auto => write!(f, "auto")
            // CollisionType::Static => write!(f, "static"),
            // CollisionType::Dynamic => write!(f, "dynamic"),
            // CollisionType::Kinetic => write!(f, "kinetic"),
        }
    }
}

#[derive(Reflect, Clone, Default)]
pub struct RigidBody {
    pub weight: f32,
    pub collision_type: CollisionType,
}

#[derive(Component, Default, Clone, Reflect)]
#[reflect(Component)]
pub struct GltfRigidBody {
    pub rigid_body: RigidBody,
}

#[derive(Component, Default, Clone, Reflect)]
#[reflect(Component)]
pub struct GrabSides {
    front: Vec3,
    left: Vec3,
    right: Vec3,
    back: Vec3,
}

#[derive(Component, Default, Clone, Reflect)]
#[reflect(Component)]
pub struct GltfGrababble {
    pub rigid_body: RigidBody,
    pub grab_magnitude: f32,
    pub grab_sides: GrabSides,
}

#[derive(Component, Default, Clone, Reflect)]
#[reflect(Component)]
pub struct Model {
    path: String,
}

impl Plugin for SavePrefabPlugin {
    #[cfg(not(tarpaulin_include))]
    fn build(&self, app: &mut App) {
        app.add_plugins(SaveResourcesPrefabPlugin {});
        app.editor_registry::<GltfRigidBody>();
        app.editor_registry::<GltfGrababble>();
        app.add_systems(
            OnEnter(SaveState::Save),
            (
                prepare_children,
                apply_deferred,
                serialize_scene,
                delete_prepared_children,
            )
                .chain(),
        );
        app.add_systems(
            OnEnter(SaveState::Export),
            (
                prepare_children,
                apply_deferred,
                serialize_scene_export,
                delete_prepared_children,
            )
                .chain(),
        );
        app.add_systems(
            OnEnter(SaveState::CreateBundle),
            (
                prepare_children,
                apply_deferred,
                create_bundle,
                delete_prepared_children,
            )
                .chain(),
        );
    }
}

/// This struct determine path to save prefab
#[cfg(not(tarpaulin_include))]
#[derive(Resource, Clone, Default)]
pub struct SaveConfig {
    pub path: Option<EditorPrefabPath>,
}

/// State system using to enable slow logic of saving
#[cfg(not(tarpaulin_include))]
#[derive(States, Debug, Clone, Copy, Default, Eq, PartialEq, Hash)]
pub enum SaveState {
    Save,
    Export,
    CreateBundle,
    #[default]
    Idle,
}

fn prepare_children(
    mut commands: Commands,
    query: Query<(Entity, &Children), (With<PrefabMarker>, Without<SceneAutoChild>)>,
) {
    for (entity, children) in query.iter() {
        commands
            .entity(entity)
            .insert(ChildrenPrefab::from_children(children));
    }
}

fn delete_prepared_children(mut commands: Commands, query: Query<Entity, With<ChildrenPrefab>>) {
    for entity in query.iter() {
        commands.entity(entity).remove::<ChildrenPrefab>();
    }
}

/// Convert world scene to prefab
pub fn serialize_scene(world: &mut World) {
    let Some(config) = world.get_resource::<SaveConfig>().cloned() else {
        #[cfg(feature = "editor")]
        world.send_event(space_shared::toast::ToastMessage::new(
            "Save config resource not initialized",
            space_shared::toast::ToastKind::Error,
        ));
        error!("Save config resource not initialized");
        return;
    };

    let mut prefab_query =
        world.query_filtered::<Entity, (With<PrefabMarker>, Without<SceneAutoChild>)>();
    let entities = prefab_query.iter(world).collect::<Vec<_>>();

    if entities.is_empty() {
        #[cfg(feature = "editor")]
        world.send_event(space_shared::toast::ToastMessage::new(
            "Saving empty scene",
            space_shared::toast::ToastKind::Warning,
        ));
        warn!("Saving empty scene");
    }

    let Some(registry) = world.get_resource::<EditorRegistry>().cloned() else {
        #[cfg(feature = "editor")]
        world.send_event(space_shared::toast::ToastMessage::new(
            "Editor Registry not initialized",
            space_shared::toast::ToastKind::Error,
        ));
        error!("Editor Registry not initialized");
        return;
    };
    let allow_types: Vec<TypeId> = registry
        .registry
        .read()
        .iter()
        .map(|a| a.type_info().type_id())
        .collect();

    let mut builder = DynamicSceneBuilder::from_world(world);
    builder = builder
        .allow_all()
        .with_filter(SceneFilter::Allowlist(HashSet::from_iter(
            allow_types.iter().cloned(),
        )))
        .extract_entities(entities.iter().copied());
    let scene = builder.build();

    let Some(app_registry) = world.get_resource::<AppTypeRegistry>() else {
        #[cfg(feature = "editor")]
        world.send_event(space_shared::toast::ToastMessage::new(
            "App Registry not initialized",
            space_shared::toast::ToastKind::Error,
        ));
        error!("App Registry not initialized");
        return;
    };

    let res = scene.serialize(&app_registry.read());

    if let Ok(str) = res {
        // Write the scene RON data to file
        let path = config.path;
        if let Some(path) = path {
            match path {
                EditorPrefabPath::File(path) => {
                    IoTaskPool::get()
                        .spawn(async move {
                            fs::OpenOptions::new()
                                .create(true)
                                .truncate(true)
                                .append(false)
                                .write(true)
                                .open(&path)
                                .and_then(|mut file| file.write(str.as_bytes()))
                                .inspect_err(|e| error!("Error while writing scene to file: {e}"))
                                .expect("Error while writing scene to file");
                            info!("Saved prefab to file {}", path);
                        })
                        .detach();
                }
                EditorPrefabPath::MemoryCache => {
                    let handle = world
                        .get_resource_mut::<Assets<DynamicScene>>()
                        .map(|mut assets| assets.add(scene));
                    if let Some(mut cache) = world.get_resource_mut::<PrefabMemoryCache>() {
                        cache.scene = handle;
                    }
                }
            }
        }
    } else if let Err(e) = res {
        // Any ideas on how to test this error case?
        #[cfg(not(tarpaulin_include))]
        let err = format!("failed to serialize prefab: {:?}", e);
        #[cfg(feature = "editor")]
        world.send_event(space_shared::toast::ToastMessage::new(
            &err,
            space_shared::toast::ToastKind::Error,
        ));
        error!(err);
    }

    if let Some(mut state) = world.get_resource_mut::<NextState<SaveState>>() {
        state.set(SaveState::Idle)
    }
}

pub fn serialize_scene_export(world: &mut World) {
    let Some(config) = world.get_resource::<SaveConfig>().cloned() else {
        #[cfg(feature = "editor")]
        world.send_event(space_shared::toast::ToastMessage::new(
            "Save config resource not initialized",
            space_shared::toast::ToastKind::Error,
        ));
        error!("Save config resource not initialized");
        return;
    };

    let mut prefab_query =
        world.query_filtered::<Entity, (With<PrefabMarker>, Without<SceneAutoChild>)>();
    let entities = prefab_query.iter(world).collect::<Vec<_>>();

    if entities.is_empty() {
        #[cfg(feature = "editor")]
        world.send_event(space_shared::toast::ToastMessage::new(
            "Exporting empty scene",
            space_shared::toast::ToastKind::Warning,
        ));
        warn!("Exporting empty scene");
    }

    let Some(registry) = world.get_resource::<EditorRegistry>().cloned() else {
        #[cfg(feature = "editor")]
        world.send_event(space_shared::toast::ToastMessage::new(
            "Editor Registry not initialized",
            space_shared::toast::ToastKind::Error,
        ));
        error!("Editor Registry not initialized");
        return;
    };
    let allow_types: Vec<TypeId> = registry
        .registry
        .read()
        .iter()
        .map(|a| a.type_id())
        .collect();
    let mut builder = DynamicSceneBuilder::from_world(world);
    builder = builder
        .allow_all()
        .with_filter(SceneFilter::Allowlist(HashSet::from_iter(
            allow_types.iter().cloned(),
        )))
        .extract_entities(entities.iter().copied());
    let scene = builder.build();

    let Some(app_registry) = world.get_resource::<AppTypeRegistry>() else {
        #[cfg(feature = "editor")]
        world.send_event(space_shared::toast::ToastMessage::new(
            "App Registry not initialized",
            space_shared::toast::ToastKind::Error,
        ));
        error!("App Registry not initialized");
        return;
    };

    let binding = app_registry.clone();
    let app_registry = binding.read();
    let export_scene = ExportSceneSerializer::new(&scene, &app_registry);
    let res = ron::ser::to_string_pretty(&export_scene, Default::default());

    if let Ok(str) = res {
        let mut de = ron::de::Deserializer::from_str(&*str).expect("Error while deserializing");
        let mut buffer = Vec::new();
        let mut ser = serde_json::Serializer::pretty(&mut buffer);
        serde_transcode::transcode(&mut de, &mut ser).expect("Error while transcode");
        let str = String::from_utf8(buffer).expect("Error while converting to utf8");
        // Write the scene JSON data to file
        let path = config.path;
        if let Some(path) = path {
            match path {
                EditorPrefabPath::File(path) => {
                    IoTaskPool::get()
                        .spawn(async move {
                            fs::OpenOptions::new()
                                .create(true)
                                .truncate(true)
                                .append(false)
                                .write(true)
                                .open(&path)
                                .and_then(|mut file| file.write(str.as_bytes()))
                                .inspect_err(|e| error!("Error while writing scene to file: {e}"))
                                .expect("Error while writing scene to file");
                            info!("Exported prefab to file {}", path);
                        })
                        .detach();
                }
                EditorPrefabPath::MemoryCache => {
                    let handle = world
                        .get_resource_mut::<Assets<DynamicScene>>()
                        .map(|mut assets| assets.add(scene));
                    if let Some(mut cache) = world.get_resource_mut::<PrefabMemoryCache>() {
                        cache.scene = handle;
                    }
                }
            }
        }
    } else if let Err(e) = res {
        // Any ideas on how to test this error case?
        #[cfg_attr(tarpaulin, ignore)]
        let err = format!("failed to serialize prefab: {:?}", e);
        #[cfg(feature = "editor")]
        world.send_event(space_shared::toast::ToastMessage::new(
            &err,
            space_shared::toast::ToastKind::Error,
        ));
        error!(err);
    }

    if let Some(mut state) = world.get_resource_mut::<NextState<SaveState>>() {
        state.set(SaveState::Idle)
    }
}

fn create_bundle(
    mut world: &mut World,
) {
    let Some(config) = world.get_resource::<SaveConfig>().cloned() else {
        #[cfg(feature = "editor")]
        world.send_event(space_shared::toast::ToastMessage::new(
            "Save config resource not initialized",
            space_shared::toast::ToastKind::Error,
        ));
        error!("Save config resource not initialized");
        return;
    };

    let mut bundle_query = world.query_filtered::<Entity, With<BundleEntity>>();
    let entities: Vec<_> = bundle_query.iter(world).collect();
    let Some(registry) = world.get_resource::<EditorRegistry>().cloned() else {
        #[cfg(feature = "editor")]
        world.send_event(space_shared::toast::ToastMessage::new(
            "Editor Registry not initialized",
            space_shared::toast::ToastKind::Error,
        ));
        error!("Editor Registry not initialized");
        return;
    };
    let allow_types: Vec<TypeId> = registry
        .registry
        .read()
        .iter()
        .map(|a| a.type_id())
        .collect();
    let scene = DynamicSceneBuilder::from_world(&mut world)
        .allow_all()
        .with_filter(SceneFilter::Allowlist(HashSet::from_iter(
            allow_types.iter().cloned(),
        )))
        .extract_entities(entities.iter().copied())
        .build();

    let Some(app_registry) = world.get_resource::<AppTypeRegistry>() else {
        #[cfg(feature = "editor")]
        world.send_event(space_shared::toast::ToastMessage::new(
            "App Registry not initialized",
            space_shared::toast::ToastKind::Error,
        ));
        error!("App Registry not initialized");
        return;
    };

    let res = scene.serialize(&app_registry.read());

    if let Ok(str) = res {
        let path = config.path;
        if let Some(path) = path {
            match path {
                EditorPrefabPath::File(path) => {
                    IoTaskPool::get()
                        .spawn(async move {
                            fs::OpenOptions::new()
                                .create(true)
                                .truncate(true)
                                .append(false)
                                .write(true)
                                .open(&path)
                                .and_then(|mut file| file.write(str.as_bytes()))
                                .inspect_err(|e| error!("Error while writing scene to file: {e}"))
                                .expect("Error while writing scene to file");
                            info!("Exported prefab to file {}", path);
                        })
                        .detach();
                }
                EditorPrefabPath::MemoryCache => {
                    let handle = world
                        .get_resource_mut::<Assets<DynamicScene>>()
                        .map(|mut assets| assets.add(scene));
                    if let Some(mut cache) = world.get_resource_mut::<PrefabMemoryCache>() {
                        cache.scene = handle;
                    }
                }
            }
        }
    } else if let Err(e) = res {
        // Any ideas on how to test this error case?
        #[cfg_attr(tarpaulin, ignore)]
        let err = format!("failed to serialize prefab: {:?}", e);
        #[cfg(feature = "editor")]
        world.send_event(space_shared::toast::ToastMessage::new(
            &err,
            space_shared::toast::ToastKind::Error,
        ));
        error!(err);
    }

    for entity in entities {
        world.entity_mut(entity).remove::<BundleEntity>();
    }
    if let Some(mut state) = world.get_resource_mut::<NextState<SaveState>>() {
        state.set(SaveState::Idle)
    }
}

pub struct ExportSceneSerializer<'a> {
    pub scene: &'a DynamicScene,
    pub registry: &'a TypeRegistry,
}

impl<'a> ExportSceneSerializer<'a> {
    pub fn new(scene: &'a DynamicScene, registry: &'a TypeRegistry) -> Self {
        Self { scene, registry }
    }
}

impl<'a> Serialize for ExportSceneSerializer<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        println!("Custom Serialize");
        let mut state = serializer.serialize_struct(SCENE_STRUCT, 2)?;
        state.serialize_field(
            SCENE_ENTITIES,
            &ExportEntitiesSerializer {
                entities: &self.scene.entities,
                registry: self.registry,
            },
        )?;
        state.end()
    }
}

pub struct ExportEntitiesSerializer<'a> {
    pub entities: &'a [DynamicEntity],
    pub registry: &'a TypeRegistry,
}

impl<'a> Serialize for ExportEntitiesSerializer<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state = serializer.serialize_tuple(self.entities.len())?;
        for entity in self.entities {
            state.serialize_element(
                &ExportEntitySerializer {
                    entity,
                    registry: self.registry,
                },
            )?;
        }
        state.end()
    }
}

pub enum ExportType {
    GLTFGrababble,
    GLTFRigidBody,
}

pub struct ExportEntitySerializer<'a> {
    pub entity: &'a DynamicEntity,
    pub registry: &'a TypeRegistry,
}

impl<'a> Serialize for ExportEntitySerializer<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut export_values_gatherer = ExportValuesGatherer {
            registry: self.registry,
            name: None,
            gltf_rigid_body: None,
            gltf_grababble: None,
            model_path: None,
            transform: None,
        };
        export_values_gatherer.get_values(&self.entity.components);
        let mut state = serializer.serialize_struct(ENTITY_STRUCT, 1)?;
        state.serialize_field(
            ENTITY_FIELD_COMPONENTS,
            &ExportSceneMapSerializer {
                entries: &self.entity.components,
                registry: self.registry,
            },
        )?;
        if let Some(ref name) = export_values_gatherer.name {
            state.serialize_field("name", &name)?;
        }
        let model_path = export_values_gatherer.model_path.unwrap_or("".to_string());
        let transform = export_values_gatherer.transform.unwrap_or(Transform::default());
        if let Some(gltf_rigidbody) = export_values_gatherer.gltf_rigid_body {
            state.serialize_field("type", "gltf_rigidbody")?;
            state.serialize_field("attributes", &GltfRigidBodySerializer::new(
                &gltf_rigidbody,
                &model_path,
                &transform,
            ))?;
        }
        if let Some(gltf_grababble) = export_values_gatherer.gltf_grababble {
            state.serialize_field("type", "gltf_grababble")?;
            state.serialize_field("attributes", &GltfGrababbleSerializer::new(
                &gltf_grababble,
                &model_path,
                &transform,
            ))?;
        }
        state.end()
    }
}

pub struct GltfRigidBodySerializer<'a> {
    pub gltf_rigid_body: &'a GltfRigidBody,
    pub model_path: &'a String,
    pub transform: &'a Transform,
}

impl<'a> GltfRigidBodySerializer<'a> {
    pub fn new(gltf_rigid_body: &'a GltfRigidBody, model_path: &'a String, transform: &'a Transform) -> Self {
        Self {
            gltf_rigid_body,
            model_path,
            transform,
        }
    }
}

impl<'a> Serialize for GltfRigidBodySerializer<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state = serializer.serialize_struct("attributes", 4)?;
        state.serialize_field("model_file", &self.model_path)?;
        state.serialize_field("weight_kg", &self.gltf_rigid_body.rigid_body.weight)?;
        state.serialize_field("collision_type", &self.gltf_rigid_body.rigid_body.collision_type.to_string())?;
        state.serialize_field("transform", &TransformSerializer {
            transform: &self.transform,
        })?;
        state.end()
    }
}

struct TransformSerializer<'a> {
    transform: &'a Transform,
}

impl<'a> TransformSerializer<'a> {
    pub fn new(transform: &'a Transform) -> Self {
        Self { transform }
    }

    fn quat_to_degree_vector(quat: Quat) -> Vec3 {
        let (axis, angle_rad) = quat.to_axis_angle();
        let angle_deg = angle_rad.to_degrees();
        axis * angle_deg
    }
}

impl<'a> Serialize for TransformSerializer<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state = serializer.serialize_struct("transform", 3)?;
        state.serialize_field("position", &Vector3Serializer::new(&self.transform.translation))?;
        let rotation_vector_deg = Self::quat_to_degree_vector(self.transform.rotation);
        state.serialize_field("rotation", &Vector3Serializer::new(&rotation_vector_deg))?;
        state.serialize_field("scale", &Vector3Serializer::new(&self.transform.scale))?;
        state.end()
    }
}

struct Vector3Serializer<'a> {
    vector: &'a Vec3,
}

impl<'a> Vector3Serializer<'a> {
    pub fn new(vector: &'a Vec3) -> Self {
        Self { vector }
    }
}

impl<'a> Serialize for Vector3Serializer<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state = serializer.serialize_tuple(3)?;
        state.serialize_element(&self.vector.x)?;
        state.serialize_element(&self.vector.y)?;
        state.serialize_element(&self.vector.z)?;
        state.end()
    }
}

pub struct GltfGrababbleSerializer<'a> {
    pub grababble: &'a GltfGrababble,
    pub model_path: &'a String,
    pub transform: &'a Transform,
}

impl<'a> GltfGrababbleSerializer<'a> {
    pub fn new(grababble: &'a GltfGrababble, model_path: &'a String, transform: &'a Transform) -> Self {
        Self {
            grababble,
            model_path,
            transform,
        }
    }
}

impl<'a> Serialize for GltfGrababbleSerializer<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state = serializer.serialize_struct("GltfGrababble", 6)?;
        state.serialize_field("model_file", &self.model_path)?;
        state.serialize_field("weight_kg", &self.grababble.rigid_body.weight)?;
        state.serialize_field("collision_type", &self.grababble.rigid_body.collision_type.to_string())?;
        state.serialize_field("grab_magnitude", &self.grababble.grab_magnitude)?;
        state.serialize_field("transform", &TransformSerializer {
            transform: &self.transform,
        })?;
        state.serialize_field("grab_sides", &GrabSidesSerializer::new(&self.grababble.grab_sides))?;
        state.end()
    }
}

pub struct GrabSidesSerializer<'a> {
    pub grab_sides: &'a GrabSides
}

impl<'a> GrabSidesSerializer<'a> {
    pub fn new(grab_sides: &'a GrabSides) -> Self {
        Self {
            grab_sides
        }
    }
}

impl<'a> Serialize for GrabSidesSerializer<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state = serializer.serialize_tuple(4)?;
        state.serialize_element(&Vector3Serializer::new(&self.grab_sides.front))?;
        state.serialize_element(&Vector3Serializer::new(&self.grab_sides.left))?;
        state.serialize_element(&Vector3Serializer::new(&self.grab_sides.right))?;
        state.serialize_element(&Vector3Serializer::new(&self.grab_sides.back))?;
        state.end()
    }
}

pub struct ExportValuesGatherer<'a> {
    pub registry: &'a TypeRegistry,
    pub name: Option<String>,
    pub gltf_rigid_body: Option<GltfRigidBody>,
    pub gltf_grababble: Option<GltfGrababble>,
    pub model_path: Option<String>,
    pub transform: Option<Transform>,
}

impl<'a> ExportValuesGatherer<'a> {
    pub fn get_values(&mut self, reflect: &Vec<Box<dyn Reflect>>) {
        for reflect in reflect {
            if let Some(name) = self.get_name(reflect) {
                self.name = Some(name);
            }
            if let Some(transform) = self.get_transform(reflect) {
                self.transform = Some(transform);
            }
            if let Some(rigid_body) = self.get_rigid_body(reflect) {
                self.gltf_rigid_body = Some(rigid_body);
            }
            if let Some(grababble) = self.get_grababble(reflect) {
                self.gltf_grababble = Some(grababble);
            }
            if let Some(model_path) = self.get_model_path(reflect) {
                self.model_path = Some(model_path);
            }
        }
    }

    pub(crate) fn get_name(&self, reflect: &Box<dyn Reflect>) -> Option<String> {
        if reflect.represents::<Name>() {
            let name = Name::from_reflect(reflect.as_ref());
            if let Some(name) = name {
                return Some(name.to_string().clone());
            }
            return None;
        }
        return None;
    }

    pub(crate) fn get_rigid_body(&self, reflect: &Box<dyn Reflect>) -> Option<GltfRigidBody> {
        if reflect.represents::<GltfRigidBody>() {
            let rigid_body = GltfRigidBody::from_reflect(reflect.as_ref());
            if let Some(rigid_body) = rigid_body {
                return Some(rigid_body.clone());
            }
            return None;
        }
        return None;
    }

    pub(crate) fn get_grababble(&self, reflect: &Box<dyn Reflect>) -> Option<GltfGrababble> {
        if reflect.represents::<GltfGrababble>() {
            let grababble = GltfGrababble::from_reflect(reflect.as_ref());
            if let Some(grababble) = grababble {
                return Some(grababble.clone());
            }
            return None;
        }
        return None;
    }

    pub(crate) fn get_model_path(&self, reflect: &Box<dyn Reflect>) -> Option<String> {
        if reflect.represents::<Model>() {
            let model = Model::from_reflect(reflect.as_ref());
            if let Some(model) = model {
                return Some(model.path.clone());
            }
            return None;
        }
        return None;
    }

    pub(crate) fn get_transform(&self, reflect: &Box<dyn Reflect>) -> Option<Transform> {
        if reflect.represents::<Transform>() {
            let transform = Transform::from_reflect(reflect.as_ref());
            if let Some(transform) = transform {
                return Some(transform.clone());
            }
            return None;
        }
        return None;
    }
}

pub struct ExportSceneMapSerializer<'a> {
    pub entries: &'a [Box<dyn Reflect>],
    pub registry: &'a TypeRegistry,
}

impl<'a> Serialize for ExportSceneMapSerializer<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state = serializer.serialize_map(Some(self.entries.len()))?;
        for reflect in self.entries {
            state.serialize_entry(
                reflect.get_represented_type_info().unwrap().type_path(),
                &TypedReflectSerializer::new(&**reflect, self.registry),
            )?;
        }
        state.end()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::prelude::*;

    #[test]
    fn flaky_save_to_file() {
        let file = "test.ron";
        let save_config = SaveConfig {
            path: Some(EditorPrefabPath::File(String::from(file))),
        };
        let mut app = App::new();
        app.add_plugins((
            MinimalPlugins,
            AssetPlugin::default(),
            ImagePlugin::default(),
            bevy::scene::ScenePlugin,
            EditorRegistryPlugin {},
            SaveResourcesPrefabPlugin {},
        ))
        .insert_resource(save_config)
        .init_resource::<PrefabMemoryCache>()
        .editor_registry::<Name>()
        .editor_registry::<PrefabMarker>()
        .add_systems(Startup, |mut commands: Commands| {
            let child_id = commands.spawn_empty().id();
            commands.spawn(PrefabMarker).add_child(child_id);

            commands.spawn(PrefabMarker).insert(Name::new("my_name"));
        });

        app.update();

        serialize_scene(&mut app.world_mut());

        // Delay for 0.2 second for IOTaskPool to finish
        std::thread::sleep(std::time::Duration::from_secs_f32(0.2));

        assert!(
            std::fs::metadata(format!("./{}", file)).is_ok(),
            "Flaky Test: File not found"
        );

        let contents = std::fs::read_to_string(file).unwrap();

        assert!(contents.contains("my_name"));
        assert!(contents.contains("space_shared::PrefabMarker"));
    }

    #[test]
    fn save_to_memory() {
        let save_config = SaveConfig {
            path: Some(EditorPrefabPath::MemoryCache),
        };
        let mut app = App::new();
        app.add_plugins(MinimalPlugins);
        app.init_state::<SaveState>();
        app.add_plugins((
            AssetPlugin::default(),
            ImagePlugin::default(),
            bevy::scene::ScenePlugin,
            EditorRegistryPlugin {},
            SaveResourcesPrefabPlugin {},
        ))
        .insert_resource(save_config)
        .init_resource::<PrefabMemoryCache>()
        .editor_registry::<Name>()
        .editor_registry::<PrefabMarker>()
        .add_systems(Startup, |mut commands: Commands| {
            let child_id = commands.spawn_empty().id();
            commands.spawn(PrefabMarker).add_child(child_id);

            commands.spawn(PrefabMarker).insert(Name::new("name"));
        });

        app.update();

        serialize_scene(&mut app.world_mut());
        assert!(app
            .world_mut()
            .resource_mut::<PrefabMemoryCache>()
            .scene
            .is_some());
    }

    #[test]
    fn inserts_prepared_children_component() {
        let mut app = App::new();
        app.add_systems(Startup, |mut commands: Commands| {
            let child_id = commands.spawn_empty().id();
            commands.spawn(PrefabMarker).add_child(child_id);

            commands.spawn(PrefabMarker);
        })
        .add_systems(Update, prepare_children);
        app.update();

        let mut query = app
            .world_mut()
            .query_filtered::<Entity, With<ChildrenPrefab>>();
        assert_eq!(query.iter(&app.world_mut()).count(), 1);
    }

    #[test]
    fn deletes_prepared_children_component() {
        let mut app = App::new();
        app.add_systems(Startup, |mut commands: Commands| {
            let child_id = commands.spawn_empty().id();
            commands
                .spawn(PrefabMarker)
                .insert(ChildrenPrefab(vec![child_id]));
            let child_id = commands.spawn_empty().id();
            commands
                .spawn(PrefabMarker)
                .insert(ChildrenPrefab(vec![child_id]));
            commands.spawn(PrefabMarker);
        })
        .add_systems(Update, delete_prepared_children);
        app.update();

        let mut query = app
            .world_mut()
            .query_filtered::<Entity, With<ChildrenPrefab>>();
        assert_eq!(query.iter(&app.world_mut()).count(), 0);
    }

    #[test]
    fn child_prefab_from_children() {
        let mut world = World::new();
        let child = world.spawn_empty().id();
        world.spawn(PrefabMarker).add_child(child);

        let mut query = world.query::<&Children>();
        let children = query.single(&world);
        let prefab = ChildrenPrefab::from_children(children);

        assert_eq!(prefab.0.len(), 1);
    }

    #[test]
    fn attempts_to_serialize_empty_scene() {
        let save_config = SaveConfig {
            path: Some(EditorPrefabPath::MemoryCache),
        };
        let mut app = App::new();
        app.add_plugins((
            MinimalPlugins,
            AssetPlugin::default(),
            ImagePlugin::default(),
            bevy::scene::ScenePlugin,
            EditorRegistryPlugin {},
            SaveResourcesPrefabPlugin {},
        ))
        .add_event::<space_shared::toast::ToastMessage>()
        .insert_resource(save_config)
        .init_resource::<PrefabMemoryCache>();

        app.update();

        serialize_scene(&mut app.world_mut());
        let events = app
            .world_mut()
            .resource::<Events<space_shared::toast::ToastMessage>>();

        let mut iter = events.get_reader();
        let iter = iter.read(events);
        iter.for_each(|e| assert_eq!(e.text, "Saving empty scene"));
    }

    #[test]
    fn prepared_children_ignores_scene_auto_child_component() {
        let mut app = App::new();
        app.add_systems(Startup, |mut commands: Commands| {
            let child_id = commands.spawn_empty().id();
            commands
                .spawn((PrefabMarker, SceneAutoChild))
                .add_child(child_id);

            let child_id = commands.spawn_empty().id();
            commands.spawn(PrefabMarker).add_child(child_id);

            commands.spawn(PrefabMarker);
        })
        .add_systems(Update, prepare_children);
        app.update();

        let mut query = app
            .world_mut()
            .query_filtered::<Entity, With<ChildrenPrefab>>();
        assert_eq!(query.iter(&app.world_mut()).count(), 1);
    }
}
