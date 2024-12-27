use bevy::{prelude::*, utils::HashMap};
use bevy_egui::egui;
use bevy_egui::egui::UiBuilder;
use crate::{tab_name::TabNameHolder, EditorTab};

/// Temporary resource for pretty system, based tab registration
pub struct EditorUiRef(pub egui::Ui);

pub struct ScheduleEditorTab {
    pub schedule: Schedule,
    pub tab_name: TabNameHolder,
}

impl EditorTab for ScheduleEditorTab {
    fn ui(&mut self, ui: &mut egui::Ui, _: &mut Commands, world: &mut World) {
        let inner_ui = ui.new_child(UiBuilder::new().max_rect(ui.max_rect()).layout(*ui.layout()));
        world.insert_non_send_resource(EditorUiRef(inner_ui));

        self.schedule.run(world);
        world.remove_non_send_resource::<EditorUiRef>();
    }

    fn tab_name(&self) -> TabNameHolder {
        self.tab_name.clone()
    }
}

#[derive(Resource, Default)]
pub struct ScheduleEditorTabStorage(pub HashMap<TabNameHolder, ScheduleEditorTab>);
