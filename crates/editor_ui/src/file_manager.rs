use std::cmp::PartialEq;
use bevy::app::{App, Plugin};
use bevy::prelude::{Commands, World};
use space_prefab::ext::{Resource};
use crate::prelude::{EditorTab};
use super::{editor_tab::EditorTabName, EditorUiAppExt};
use std::fs;
use std::path::{Path, PathBuf};
use bevy_egui::egui::{epaint, Align, CursorIcon, FontSelection, Id, InnerResponse, LayerId, Order, Rect, RichText, Sense, Shape, Style, Ui, Vec2};
use bevy_egui::egui::text::LayoutJob;
use crate::sizing::IconSize;

#[derive(Default)]
pub struct FileManagerPlugin;

impl Plugin for FileManagerPlugin {
    fn build(&self, app: &mut App) {
        app.editor_tab_by_trait(EditorTabName::FileManager, FileManagerTab::default());
    }
}

#[derive(Resource)]
struct FileManagerTab {
    path: PathBuf,
    icon_size: f32,
}

struct File {
    name: String,
    file_type: FileType,
}

#[derive(PartialEq)]
enum FileType {
    File,
    Directory,
}

impl Default for FileManagerTab {
    fn default() -> Self {
        Self {
            path: PathBuf::from("assets/"),
            icon_size: IconSize::XLarge.to_size(),
        }
    }
}

impl EditorTab for FileManagerTab {
    fn ui(&mut self, ui: &mut bevy_egui::egui::Ui, commands: &mut Commands, world: &mut World) {
        let files = FileManagerTab::get_files_in_directory(&self.path);
        let open = false;

        ui.set_max_width(ui.clip_rect().width());
        ui.horizontal(|ui| {
            ui.label(RichText::new(self.path.to_str().unwrap()).size(16.0).strong());
        });
        ui.horizontal_wrapped(|ui| {
            {
                let style = Style::default();
                let mut layout = LayoutJob::default();
                RichText::new("📁").size(self.icon_size).append_to(
                    &mut layout,
                    &style,
                    FontSelection::Default,
                    Align::Center);
                RichText::new(" ..").append_to(
                    &mut layout,
                    &style,
                    FontSelection::Default,
                    Align::Center);
                let response = ui.selectable_label(open, layout);
                if response.clicked() {
                    self.path.pop();
                }
                for file in files {
                    // Self::drop_target(ui, true, |ui| {
                    //     Self::drag_source(ui, Id::from(file.name.clone()), |ui| {
                            self.content(ui, &file);
                    //     });
                    // });
                }
            }
        });
    }

    fn title(&self) -> bevy_egui::egui::WidgetText {
        "File Manager".into()
    }
}

impl FileManagerTab {
    fn get_files_in_directory(directory: &Path) -> Vec<File> {
        let mut paths  = Vec::new();
        if let Ok(entries) = fs::read_dir(directory) {
            for entry in entries {
                if let Ok(entry) = entry {
                    if let Some(path_str) = entry.file_name().to_str() {
                        let file_type = if entry.file_type().unwrap().is_dir() {
                            FileType::Directory
                        } else {
                            FileType::File
                        };
                        paths.push(File {
                            name: path_str.to_string(),
                            file_type,
                        });
                    }
                }
            }
        }
        paths
    }

    pub fn drag_source(ui: &mut Ui, id: Id, body: impl FnOnce(&mut Ui)) {
        let is_being_dragged = ui.memory(|mem| mem.is_being_dragged(id));

        if !is_being_dragged {
            let response = ui.scope(body).response;

            // Check for drags:
            let response = ui.interact(response.rect, id, Sense::drag());
        } else {
            ui.ctx().set_cursor_icon(CursorIcon::Grabbing);

            // Paint the body to a new layer:
            let layer_id = LayerId::new(Order::Tooltip, id);
            let response = ui.with_layer_id(layer_id, body).response;

            // Now we move the visuals of the body to where the mouse is.
            // Normally you need to decide a location for a widget first,
            // because otherwise that widget cannot interact with the mouse.
            // However, a dragged component cannot be interacted with anyway
            // (anything with `Order::Tooltip` always gets an empty [`Response`])
            // So this is fine!

            if let Some(pointer_pos) = ui.ctx().pointer_interact_pos() {
                let delta = pointer_pos - response.rect.center();
                ui.ctx().translate_layer(layer_id, delta);
            }
        }
    }

    pub fn drop_target<R>(
        ui: &mut Ui,
        can_accept_what_is_being_dragged: bool,
        body: impl FnOnce(&mut Ui) -> R,
    ) -> InnerResponse<R> {
        let is_being_dragged = ui.memory(|mem| mem.is_anything_being_dragged());

        let margin = Vec2::splat(4.0);

        let outer_rect_bounds = ui.available_rect_before_wrap();
        let inner_rect = outer_rect_bounds.shrink2(margin);
        let where_to_put_background = ui.painter().add(Shape::Noop);
        let mut content_ui = ui.child_ui(inner_rect, *ui.layout());
        let ret = body(&mut content_ui);
        let outer_rect = Rect::from_min_max(outer_rect_bounds.min, content_ui.min_rect().max + margin);
        let (rect, response) = ui.allocate_at_least(outer_rect.size(), Sense::hover());

        let style = if is_being_dragged && can_accept_what_is_being_dragged && response.hovered() {
            ui.visuals().widgets.active
        } else {
            ui.visuals().widgets.inactive
        };

        let mut fill = style.bg_fill;
        let mut stroke = style.bg_stroke;
        if is_being_dragged && !can_accept_what_is_being_dragged {
            fill = ui.visuals().gray_out(fill);
            stroke.color = ui.visuals().gray_out(stroke.color);
        }

        ui.painter().set(
            where_to_put_background,
            epaint::RectShape::new(rect, style.rounding, fill, stroke),
        );

        InnerResponse::new(ret, response)
    }

    pub fn content(&mut self, mut ui: &mut Ui, file: &File) {
        let style = Style::default();
        let mut layout = LayoutJob::default();
        if file.file_type == FileType::Directory {
            RichText::new("📁").size(self.icon_size).append_to(
                &mut layout,
                &style,
                FontSelection::Default,
                Align::Center);
        } else {
            RichText::new("📄").size(self.icon_size).append_to(
                &mut layout,
                &style,
                FontSelection::Default,
                Align::Center);
        };
        RichText::new(format!(" {}", file.name.clone())).append_to(
            &mut layout,
            &style,
            FontSelection::Default,
            Align::Center);
        let response = ui.selectable_label(false, layout);
        if response.clicked() {
            if file.file_type == FileType::Directory {
                self.path.push(&file.name.clone());
            }
        }
        if response.secondary_clicked() {
            println!("Secondary click");
            //rename file as "credit.txt"

            // fs::rename(format!("{}{}", self.path.to_str().unwrap(), file.name.clone()), format!("{}{}", self.path.to_str().unwrap(), "test.txt")).expect("Unable to rename file");
        }
    }
}