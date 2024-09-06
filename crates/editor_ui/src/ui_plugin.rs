use crate::*;
use bevy::prelude::*;
use bevy_egui::egui::{
    FontFamily::{Monospace, Proportional},
    FontId, Margin, Rounding, TextStyle as ETextStyle, Vec2,
};
use camera_plugin::draw_camera_gizmo;
use meshless_visualizer::draw_light_gizmo;
use egui::emath::Rect;
use crate::file_manager::FileManagerPlugin;
use self::{
    colors::*,
    sizing::{to_label, Sizing},
};

/// All systems for editor ui will be placed in UiSystemSet
#[derive(SystemSet, Hash, PartialEq, Eq, Debug, Clone, Copy)]
pub struct UiSystemSet;

/// Plugin for editor ui
pub struct EditorUiPlugin {
    pub use_standard_layout: bool,
}

impl Default for EditorUiPlugin {
    fn default() -> Self {
        Self {
            use_standard_layout: true,
        }
    }
}

/// State to determine if editor ui should be shown (or hidden for any reason)
#[derive(Hash, PartialEq, Eq, Debug, Clone, States, Default)]
pub enum ShowEditorUi {
    #[default]
    Show,
    Hide,
}

impl FlatPluginList for EditorUiPlugin {
    fn add_plugins_to_group(&self, group: PluginGroupBuilder) -> PluginGroupBuilder {
        let mut res = group
            .add(SelectedPlugin)
            .add(MeshlessVisualizerPlugin)
            .add(EditorUiCore::default())
            .add(GameViewPlugin)
            .add(menu_toolbars::BottomMenuPlugin)
            .add(MouseCheck)
            .add(CameraViewTabPlugin)
            .add(SpaceHierarchyPlugin::default())
            .add(SpaceInspectorPlugin)
            .add(FileManagerPlugin)
            .add(GizmoToolPlugin)
            .add(ChangeChainViewPlugin)
            .add(settings::SettingsWindowPlugin);

        if self.use_standard_layout {
            res = res.add(DefaultEditorLayoutPlugin);
        }

        res
    }
}

impl PluginGroup for EditorUiPlugin {
    fn build(self) -> PluginGroupBuilder {
        let mut group = PluginGroupBuilder::start::<Self>();
        group = self.add_plugins_to_group(group);
        group
    }
}

pub struct DefaultEditorLayoutPlugin;

impl Plugin for DefaultEditorLayoutPlugin {
    fn build(&self, app: &mut App) {
        let mut editor = app.world.resource_mut::<EditorUi>();
        editor.tree = egui_dock::DockState::new(vec![EditorTabName::GameView]);

        let [game, hierarchy] = editor.tree.main_surface_mut().split_left(
            egui_dock::NodeIndex::root(),
            0.2,
            vec![EditorTabName::Hierarchy],
        );
        let [_hierarchy, _inspector] = editor.tree.main_surface_mut().split_below(
            hierarchy,
            0.3,
            vec![EditorTabName::Inspector],
        );
        let [_, _file_manager] = editor.tree.main_surface_mut().split_below(
            game,
            0.7,
            vec![EditorTabName::FileManager],
        );
    }
}

pub struct EditorUiCore {
    pub disable_no_editor_cams: bool,
}

impl Default for EditorUiCore {
    fn default() -> Self {
        Self {
            disable_no_editor_cams: true,
        }
    }
}

impl Plugin for EditorUiCore {
    fn build(&self, app: &mut App) {
        app.init_state::<ShowEditorUi>();

        app.add_event::<FocusTabEvent>();
        app.add_event::<OpenTabEvent>();
        app.add_event::<ResizeTabEvent>();
        app.add_event::<CloseTabEvent>();

        app.configure_sets(
            Update,
            UiSystemSet
                .in_set(EditorSet::Editor)
                .run_if(in_state(EditorState::Editor).and_then(in_state(ShowEditorUi::Show))),
        );
        app.init_resource::<EditorUi>();
        app.init_resource::<ScheduleEditorTabStorage>();
        app.add_systems(
            Update,
            (
                show_editor_ui
                    .before(update_pan_orbit)
                    .before(ui_camera_block)
                    .after(menu_toolbars::top_menu)
                    .after(menu_toolbars::bottom_menu),
                set_camera_viewport,
            )
                .in_set(UiSystemSet)
                .before(PanOrbitCameraSystemSet),
        );

        app.add_systems(
            PostUpdate,
            set_camera_viewport
                .run_if(has_window_changed)
                .in_set(UiSystemSet),
        );
        app.add_systems(
            Update,
            reset_camera_viewport.run_if(in_state(EditorState::Game)),
        );
        app.add_systems(OnEnter(ShowEditorUi::Hide), reset_camera_viewport);
        app.editor_tab_by_trait(EditorTabName::GameView, GameViewTab::default());

        app.editor_tab_by_trait(
            EditorTabName::Other("Debug World Inspector".to_string()),
            self::debug_panels::DebugWorldInspector {},
        );

        app.init_resource::<EditorLoader>();

        app.insert_resource(EditorCameraEnabled(true));

        app.add_systems(
            Startup,
            (set_start_state, apply_state_transition::<EditorState>).chain(),
        );

        //play systems
        app.add_systems(OnEnter(EditorState::GamePrepare), save_prefab_before_play);
        // clean up meshless children on entering the game state
        app.add_systems(OnEnter(EditorState::GamePrepare), clean_meshless);
        app.add_systems(
            OnEnter(SaveState::Idle),
            to_game_after_save.run_if(in_state(EditorState::GamePrepare)),
        );

        app.add_systems(OnEnter(EditorState::Game), change_camera_in_play);

        app.add_systems(
            OnEnter(EditorState::Editor),
            (clear_and_load_on_start, set_camera_viewport),
        );

        app.add_systems(
            Update,
            (
                draw_camera_gizmo,
                draw_light_gizmo,
                selection::delete_selected,
            )
                .run_if(in_state(EditorState::Editor).and_then(in_state(ShowEditorUi::Show))),
        );

        if self.disable_no_editor_cams {
            app.add_systems(
                Update,
                disable_no_editor_cams.run_if(in_state(EditorState::Editor)),
            );

            app.add_systems(OnEnter(EditorState::Editor), change_camera_in_editor);
        }

        app.add_event::<selection::SelectEvent>();

        app.init_resource::<BundleReg>();

        app.add_systems(Update, test_change_tab);
    }
}

fn test_change_tab(
    mut focus_tab: EventReader<FocusTabEvent>,
    mut open: EventReader<OpenTabEvent>,
    mut close: EventReader<CloseTabEvent>,
    mut resize: EventReader<ResizeTabEvent>,
) {
    for event in focus_tab.read() {
        println!("Focus tab: {:?} with rect {:?}", event.tab, event.rect);
    }
    for event in open.read() {
        match &event.open_type {
            OpenTabType::SplitNode(other_node) => {
                println!("Open tab: {:?} in split node {:?} with rect {:?}", event.tab, other_node, event.rect);
            }
            OpenTabType::SameNode(source_node) => {
                println!("Open tab: {:?} in same node {:?} with rect {:?}", event.tab, source_node, event.rect);
            }
            OpenTabType::AddNode => {
                println!("Open tab: {:?} in new node with rect {:?}", event.tab, event.rect);
            }
        }
    }
    for event in close.read() {
        println!("Close tab: {:?}", event.tab);
    }
    for event in resize.read() {
        println!("Resize tab: {:?} to {:?}", event.tab, event.rect);
    }
    focus_tab.clear();
    open.clear();
    close.clear();
    resize.clear();
}

/// This system use to show all egui editor ui on primary window
/// Will be useful in some specific cases to ad new system before/after this system
pub fn show_editor_ui(world: &mut World) {
    let Ok(egui_context) = world
        .query_filtered::<&mut EguiContext, With<PrimaryWindow>>()
        .get_single(world)
    else {
        return;
    };
    let mut egui_context = egui_context.clone();
    let ctx = egui_context.get_mut();
    egui_extras::install_image_loaders(ctx);
    ctx.style_mut(|stl| {
        stl.spacing.button_padding = Vec2::new(8., 2.);
        stl.spacing.icon_spacing = 4.;
        stl.spacing.icon_width = 16.;
        stl.spacing.menu_margin = Margin {
            left: 8.,
            right: 8.,
            top: 4.,
            bottom: 8.,
        };
        stl.visuals.error_fg_color = ERROR_COLOR;
        stl.visuals.hyperlink_color = HYPERLINK_COLOR;
        stl.visuals.warn_fg_color = WARM_COLOR;
        stl.visuals.menu_rounding = Rounding::same(0.5);
        stl.text_styles = [
            (ETextStyle::Small, FontId::new(10.0, Proportional)),
            (ETextStyle::Body, FontId::new(12., Proportional)),
            (ETextStyle::Button, FontId::new(14., Proportional)),
            (ETextStyle::Heading, FontId::new(20.0, Proportional)),
            (ETextStyle::Monospace, FontId::new(12.0, Monospace)),
        ]
        .into()
    });

    world.resource_scope::<EditorUi, _>(|world, mut editor_ui| {
        editor_ui.ui(world, ctx);
    });
}

/// This resource contains registered editor tabs and current dock tree state
#[derive(Resource)]
pub struct EditorUi {
    pub registry: HashMap<EditorTabName, EditorUiReg>,
    pub tree: egui_dock::DockState<EditorTabName>,
    pub focused_tab: Option<EditorTabName>,
    pub open_tabs: Vec<EditorTabName>,
    pub tabs_size: Vec<(EditorTabName, Rect)>
}

#[derive(Event)]
pub struct FocusTabEvent {
    pub tab: EditorTabName,
    pub rect: Rect,
}

pub enum OpenTabType {
    SplitNode(EditorTabName),
    SameNode(EditorTabName),
    AddNode
}

#[derive(Event)]
pub struct OpenTabEvent {
    pub tab: EditorTabName,
    pub rect: Rect,
    pub open_type: OpenTabType,
}

#[derive(Event)]
pub struct CloseTabEvent {
    pub tab: EditorTabName,
}

#[derive(Event)]
pub struct ResizeTabEvent {
    pub tab: EditorTabName,
    pub rect: Rect,
}

impl Default for EditorUi {
    fn default() -> Self {
        Self {
            registry: HashMap::default(),
            tree: egui_dock::DockState::new(vec![]),
            focused_tab: None,
            open_tabs: vec![],
            tabs_size: vec![],
        }
    }
}

/// This enum determine how tab was registered.
/// ResourceBased - tab will be registered as resource
/// Schedule - tab will be registered as system
pub enum EditorUiReg {
    ResourceBased {
        show_command: EditorTabShowFn,
        title_command: EditorTabGetTitleFn,
    },
    Schedule,
}

impl EditorUi {
    pub fn ui(&mut self, world: &mut World, ctx: &mut egui::Context) {
        if let Some(focused) = self.tree.find_active_focused() {
            if self.focused_tab != Some(focused.1.clone()) {
                self.focused_tab = Some(focused.1.clone());
                world.send_event(FocusTabEvent {
                    tab: focused.1.clone(),
                    rect: focused.0,
                });
            }
        }

        let tree_tabs_name = self.tree.iter_all_tabs().map(|(_, tab)| tab.clone()).collect::<Vec<_>>();
        let mut closed_tabs : Vec<EditorTabName> = vec![];
        if self.open_tabs.len() != tree_tabs_name.len(){
            for tab_name in self.open_tabs.iter(){
                if !tree_tabs_name.contains(tab_name){
                    closed_tabs.push(tab_name.clone());
                }
            }
           self.open_tabs = tree_tabs_name;
        }
        for tab in closed_tabs{
            world.send_event(CloseTabEvent {
                tab: tab.clone(),
            });
        }

        let mut resized_tabs : Vec<(EditorTabName, Rect)> = vec![];
        let open_tabs_rect = {
            let mut open_tabs_rect = vec![];
            let tree_tabs = self.tree.iter_all_nodes().map(|(_, node)| node.clone()).collect::<Vec<_>>();
            for tab in tree_tabs {
                if let Some(rect) = tab.rect() {
                    let test = tab.tabs().and_then(|tabs| Some(tabs));
                    if let Some(tabs) = test {
                        for tab in tabs {
                            open_tabs_rect.push((tab.clone(), rect));
                        }
                    }
                }
            }
            open_tabs_rect
        };
        if self.tabs_size.len() != open_tabs_rect.len() {
            self.tabs_size = open_tabs_rect.clone();
        }
        for tab in open_tabs_rect.clone() {
            if !self.tabs_size.contains(&tab) {
                resized_tabs.push(tab);
            }
        }
        if resized_tabs.len() != 0 {
            self.tabs_size = open_tabs_rect.clone();
        }
        for (tab, rect) in resized_tabs{
            world.send_event(ResizeTabEvent {
                tab: tab.clone(),
                rect,
            });
        }

        //collect tab names to vec to detect visible
        let mut visible = vec![];
        for (_surface_index, tab) in self.tree.iter_all_nodes() {
            match tab {
                egui_dock::Node::Empty => {}
                egui_dock::Node::Leaf {
                    rect: _,
                    viewport: _,
                    tabs,
                    active: _,
                    scroll: _,
                } => visible.extend(tabs.clone()),
                egui_dock::Node::Vertical {
                    rect: _,
                    fraction: _,
                } => {}
                egui_dock::Node::Horizontal {
                    rect: _,
                    fraction: _,
                } => {}
            }
        }

        let cell = world.as_unsafe_world_cell();

        let mut command_queue = CommandQueue::default();
        let mut commands = Commands::new(&mut command_queue, unsafe { cell.world() });

        let mut tab_viewer = unsafe {
            EditorTabViewer {
                commands: &mut commands,
                world: cell.world_mut(),
                registry: &mut self.registry,
                visible,
                tab_commands: vec![],
            }
        };

        DockArea::new(&mut self.tree)
            .show_add_buttons(true)
            .show_add_popup(true)
            .show(ctx, &mut tab_viewer);

        let windows_setting = unsafe { cell.world_mut().resource_mut::<NewWindowSettings>() };
        let mut open_events : Vec<OpenTabEvent> = vec![];
        for command in tab_viewer.tab_commands {
            match command {
                EditorTabCommand::Add {
                    name,
                    surface,
                    node,
                } => {
                    match windows_setting.new_tab {
                        NewTabBehaviour::Pop => {
                            let new_surface_index = self.tree.add_window(vec![name.clone()]);
                            let mut new_node: Option<(Rect, &mut EditorTabName)> = None;
                            if let Some(new_surface) = self.tree.get_surface_mut(new_surface_index) {
                                if let Some(tree) = new_surface.node_tree_mut() {
                                    tree.set_focused_node(node);
                                    new_node = tree.find_active_focused();
                                }
                            }
                            if let Some((rect, _)) = new_node {
                                open_events.push(OpenTabEvent {
                                    tab: name,
                                    rect,
                                    open_type: OpenTabType::AddNode,
                                });
                            }
                        }
                        NewTabBehaviour::SameNode => {
                            let tab_name = name.clone();
                            let mut source_node: Option<(Rect, EditorTabName)> = None;
                            if let Some(tree) = self
                                .tree
                                .get_surface_mut(surface)
                                .and_then(|surface| surface.node_tree_mut())
                            {
                                tree.set_focused_node(node);
                                let mut tree_clone = tree.clone();
                                source_node = tree_clone.find_active_focused().map(|(rect, name)| {
                                    (rect, name.clone())
                                });
                                tree.push_to_focused_leaf(name);
                            }
                            if let Some((rect, source_node_name)) = source_node {
                                open_events.push(OpenTabEvent {
                                    tab: tab_name,
                                    rect,
                                    open_type: OpenTabType::SameNode(source_node_name),
                                });
                            }
                        }
                        NewTabBehaviour::SplitNode => {
                            let tab_name = name.clone();
                            let mut infos: Option<(Rect, EditorTabName)> = None;
                            if let Some(surface) = self.tree.get_surface_mut(surface) {
                                let indexes = surface
                                    .node_tree_mut()
                                    .unwrap()
                                    .split_right(node, 0.5, vec![name]);
                                surface.node_tree_mut().unwrap().set_focused_node(indexes[1]);
                                let rect = surface.node_tree_mut().unwrap().find_active_focused().unwrap().0;
                                let other_node = surface.iter_all_tabs().find(|(node_index, _)| {
                                    node.0 == node_index.0
                                }).unwrap();
                                let other_node_name = other_node.1.clone();
                                infos = Some((rect, other_node_name));
                            }
                            if let Some((rect, other_node_name)) = infos {
                                open_events.push(OpenTabEvent {
                                    tab: tab_name,
                                    rect,
                                    open_type: OpenTabType::SplitNode(other_node_name),
                                });
                            }
                        }
                    }
                },
            }
        }

        unsafe {
            command_queue.apply(cell.world_mut());
        }

        for event in open_events {
            world.send_event(event);
        }
    }
}

/// Trait for registering editor tabs via app.**
pub trait EditorUiAppExt {
    fn editor_tab_by_trait<T>(&mut self, tab_id: EditorTabName, tab: T) -> &mut Self
    where
        T: EditorTab + Resource + Send + Sync + 'static;
    fn editor_tab<T>(
        &mut self,
        tab_id: EditorTabName,
        title: egui::WidgetText,
        tab_systems: impl IntoSystemConfigs<T>,
    ) -> &mut Self;
}

impl EditorUiAppExt for App {
    fn editor_tab_by_trait<T>(&mut self, tab_id: EditorTabName, tab: T) -> &mut Self
    where
        T: EditorTab + Resource + Send + Sync + 'static,
    {
        self.insert_resource(tab);
        let show_fn = Box::new(
            |ui: &mut egui::Ui, commands: &mut Commands, world: &mut World| {
                world.resource_scope(|scoped_world, mut data: Mut<T>| {
                    data.ui(ui, commands, scoped_world)
                });
            },
        );
        let reg = EditorUiReg::ResourceBased {
            show_command: show_fn,
            title_command: Box::new(|world| {
                let sizing = world.resource::<Sizing>().clone();
                to_label(world.resource_mut::<T>().title().text(), sizing.text).into()
            }),
        };

        self.world
            .resource_mut::<EditorUi>()
            .registry
            .insert(tab_id, reg);
        self
    }

    fn editor_tab<T>(
        &mut self,
        tab_id: EditorTabName,
        title: egui::WidgetText,
        tab_systems: impl IntoSystemConfigs<T>,
    ) -> &mut Self {
        let mut tab = ScheduleEditorTab {
            schedule: Schedule::default(),
            title,
        };

        tab.schedule.add_systems(tab_systems);

        self.world
            .resource_mut::<ScheduleEditorTabStorage>()
            .0
            .insert(tab_id.clone(), tab);
        self.world
            .resource_mut::<EditorUi>()
            .registry
            .insert(tab_id, EditorUiReg::Schedule);
        self
    }
}

/// Temporary resource for pretty system, based tab registration
pub struct EditorUiRef(pub egui::Ui);

/// System to block camera control if egui is using mouse
pub fn ui_camera_block(
    mut ctxs: Query<&mut EguiContext, With<PrimaryWindow>>,
    mut state: ResMut<EditorCameraEnabled>,
    game_view: Res<GameViewTab>,
) {
    let Ok(mut ctx_ref) = ctxs.get_single_mut() else {
        return;
    };
    let ctx = ctx_ref.get_mut();
    if ctx.is_using_pointer() || ctx.is_pointer_over_area() {
        let Some(pos) = ctx.pointer_latest_pos() else {
            return;
        };
        if let Some(area) = game_view.viewport_rect {
            if area.contains(pos) {
            } else {
                *state = EditorCameraEnabled(false);
            }
        } else {
            *state = EditorCameraEnabled(false);
        }
    }
}
