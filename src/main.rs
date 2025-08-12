//! Hierarchal Notes
#![warn(missing_docs, clippy::missing_docs_in_private_items)]

use std::{cell::RefCell, fmt::Display, path::Path, rc::Rc};

use color_eyre::{eyre::eyre, Result};
use crossterm::event::{self, Event};
use ratatui::{
    DefaultTerminal, Frame,
    layout::{Constraint, Layout},
    style::Stylize,
    text::Text,
    widgets::{Block, Borders, List, Paragraph},
};
use time::OffsetDateTime;

/// The version for storing data.
const DATA_VERSION: u16 = 0;

fn main() -> Result<()> {
    color_eyre::install()?;
    let terminal = ratatui::init();
    let result = run(terminal);
    ratatui::restore();
    result
}

/// The current status of a note/task.
#[derive(Clone, Debug, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
enum NoteStatus {
    /// Incomplete.
    Incomplete,
    /// The task has been started.
    Begun,
    /// Between 0 and 1000.
    Fractional(u16),
    /// The task is done.
    Complete,
    /// Custom status.
    Custom(String),
}

impl NoteStatus {
    /// Get the next one of this
    fn next(&self) -> Self {
        match self {
            Self::Incomplete => Self::Begun,
            Self::Begun => Self::Fractional(0),
            Self::Fractional(_) => Self::Complete,
            Self::Complete => Self::Incomplete,
            Self::Custom(_) => Self::Incomplete,
        }
    }
    /// Get the previous one of this
    fn prev(&self) -> Self {
        match self {
            Self::Incomplete => Self::Complete,
            Self::Begun => Self::Incomplete,
            Self::Fractional(_) => Self::Begun,
            Self::Complete => Self::Fractional(0),
            Self::Custom(_) => Self::Complete,
        }
    }
}

impl Display for NoteStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NoteStatus::Incomplete => f.write_str("Incomplete"),
            NoteStatus::Begun => f.write_str("Begun"),
            NoteStatus::Fractional(v) => f.write_fmt(format_args!("{}% done", *v as f64 / 10.0)),
            NoteStatus::Complete => f.write_str("Complete 🤩"),
            NoteStatus::Custom(v) => f.write_str(v),
        }
    }
}

/// A serializable note structure.
#[derive(Clone, Debug, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
struct SerializableNote {
    /// When this note was created.
    created_at: time::OffsetDateTime,
    /// When this note was last edited.
    edited_at: time::OffsetDateTime,
    /// The note's contents.
    data: String,
    /// The current status of the note.
    status: NoteStatus,
    /// The note's children.
    children: Vec<SerializableNote>,
}

/// A container for serializable notes.
#[derive(Clone, Debug, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
struct SerializableNotesContainer {
    /// The version of the container.
    version: u16,
    /// The root-level notes.
    root_notes: Vec<SerializableNote>,
}

/// A single note.
#[derive(Clone, Debug, PartialEq, Eq)]
struct Note {
    /// When this note was created.
    created_at: time::OffsetDateTime,
    /// When this note was last edited.
    edited_at: time::OffsetDateTime,
    /// The note's contents.
    data: String,
    /// The current status of the note.
    status: NoteStatus,
    /// The note's children.
    children: Vec<Rc<RefCell<Note>>>,
    /// The parent of the note.
    parent: Option<Rc<RefCell<Note>>>,
}

impl SerializableNote {
    /// Special method because parent can't be set
    fn part_into(self) -> Rc<RefCell<Note>> {
        let note = Rc::new(RefCell::new(Note { created_at: self.created_at, edited_at: self.edited_at, data: self.data, status: self.status, children: vec![], parent: None}));
        note.borrow_mut().children = self.children.iter().map(|v| {
            let v = v.clone().part_into();
            v.borrow_mut().parent = Some(note.clone());
            v
        })
            .collect();
        note
    }
}

impl From<Note> for SerializableNote {
    fn from(value: Note) -> Self {
        SerializableNote {
            created_at: value.created_at,
            edited_at: value.edited_at,
            data: value.data,
            status: value.status,
            children: value
                .children
                .iter()
                .map(|v| v.borrow().clone().into())
                .collect(),
        }
    }
}

/// The currently selected input.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum AddingInput {
    /// The payload/data.
    Data,
    /// The status.
    Status,
    /// The submit button.
    Submit,
}

/// The currently selected input.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum DeletingInput {
    /// Cancel the deletion.
    Cancel,
    /// Confirm the deletion.
    Confirm,
}

/// The possible dialogs.
#[derive(Clone, Debug, PartialEq, Eq, Default, Hash)]
enum Dialog {
    /// No dialog.
    #[default]
    None,
    /// The adding dialog.
    Adding(AddingInput, (String, NoteStatus, usize)),
    /// The editing dialog.
    Editing(AddingInput, (String, NoteStatus, usize)),
    /// The deleting confirmation dialog.
    Deleting(DeletingInput),
}

impl Dialog {
    /// Matches Dialog::Adding.
    fn is_adding(&self) -> bool {
        matches!(self, Self::Adding(_, _))
    }
}

/// The current status of the app.
#[derive(Clone, Debug, PartialEq, Eq)]
struct Status {
    /// The root-level notes.
    root_notes: Vec<Rc<RefCell<Note>>>,
    /// The currently selected note.
    selected: Option<Rc<RefCell<Note>>>,
    /// the parent and index of the selected note
    selected_index: (Option<Rc<RefCell<Note>>>, usize),
    /// The current dialog.
    current_dialog: Dialog,
    /// Data to display in the status bar. Removed (set to None) after the user switches to something
    /// that displays a different status.
    status_bar: Option<String>,
    /// The time of the last save.
    last_save: std::time::Instant,
}

impl From<&Status> for SerializableNotesContainer {
    fn from(value: &Status) -> Self {
        SerializableNotesContainer {
            version: DATA_VERSION,
            root_notes: value
                .root_notes
                .iter()
                .map(|v| v.borrow().clone().into())
                .collect(),
        }
    }
}

impl TryFrom<SerializableNotesContainer> for Vec<Rc<RefCell<Note>>> {
    type Error = color_eyre::Report;
    fn try_from(value: SerializableNotesContainer) -> std::result::Result<Self, Self::Error> {
        if value.version != 0 {
            return Err(eyre!("invalid version"));
        }
        Ok(value.root_notes.iter().map(|v| v.clone().part_into()).collect())
    }
}

/// Run the stuff
fn run(mut terminal: DefaultTerminal) -> Result<()> {
    let mut status = Status {
        root_notes: vec![],
        selected: None,
        selected_index: (None, 0),
        current_dialog: Dialog::None,
        status_bar: None,
        last_save: std::time::Instant::now(),
    };
    let data_dir = dirs::data_dir().unwrap().join("notation");
    if let Some(e) = std::fs::create_dir_all(&data_dir).err() {
        status.status_bar = Some(format!("Error creating data directory: {}", e))
    }

    if std::fs::exists(data_dir.join("notes.ron"))? {
        match std::fs::read_to_string(data_dir.join("notes.ron")) {
            Ok(data) => {
                match ron::from_str::<SerializableNotesContainer>(&data) {
                    Ok(out) => {
                        status.root_notes = match out.try_into() {
                            Ok(data) => data,
                            Err(e) => {
                                status.status_bar = Some(format!("Error with contents of notes: {}", e));
                                vec![]
                            },
                        };
                    },
                    Err(e) => {
                        status.status_bar = Some(format!("Error parsing notes: {}", e))
                    },
                }
            }
            Err(e) => {
                status.status_bar = Some(format!("Error reading notes: {}", e))
            }
        }
    }
    
    fn scroll_up(status: &mut Status) {
        let (ref parent, ref mut index) = status.selected_index;
        let siblings = parent
            .clone()
            .map(|v| v.borrow().children.clone())
            .unwrap_or(status.root_notes.clone());

        status.status_bar = None;

        if !siblings.is_empty() {
            if index.checked_sub(1).is_none() {
                if status.selected.is_none() {
                    *index = siblings.len() - 1;

                    status.selected = Some(siblings[*index].clone());
                } else {
                    *index = 0;
                    status.selected = None;
                }
            } else {
                *index = index.saturating_sub(1);

                status.selected = Some(siblings[*index].clone());
            }
        }
    }
    fn scroll_down(status: &mut Status) {
        let (ref parent, ref mut index) = status.selected_index;
        let siblings = parent
            .clone()
            .map(|v| v.borrow().children.clone())
            .unwrap_or(status.root_notes.clone());

        status.status_bar = None;

        if *index < siblings.len().saturating_sub(1)
            || (siblings.len() == 1 && *index == 0 && status.selected.is_none())
        {
            if status.selected.is_some() {
                *index += 1;
            }
            status.selected = Some(siblings[*index].clone());
        } else {
            *index = 0;
            status.selected = None;
        }
    }
    fn save(data_dir: &Path, status: &mut Status) -> Result<()> {
        let serializable: SerializableNotesContainer = (&*status).into();

        let data = ron::to_string(&serializable)?;

        std::fs::write(data_dir.join("notes.ron"), data)?;
        status.status_bar = Some("Saved.".to_string());
        status.last_save = std::time::Instant::now();
        
        Ok(())
    }
    loop {
        if status.last_save.elapsed() >= std::time::Duration::from_secs(60) {
            save(&data_dir, &mut status)?;
        }
        terminal.draw(render(&mut status))?;
        if event::poll(std::time::Duration::from_millis(100))? {
            if status.current_dialog == Dialog::None {
                match event::read()? {
                    Event::Key(ev) => {
                        if ev.is_press() || ev.is_repeat() {
                            match ev.code {
                                event::KeyCode::Char('d') if ev.modifiers.contains(event::KeyModifiers::CONTROL) => {
                                    status.status_bar = Some(format!("{:#?}", status.last_save.elapsed()))
                                }
                                event::KeyCode::Char('s')
                                    if ev.modifiers.contains(event::KeyModifiers::CONTROL) => {
                                        save(&data_dir, &mut status)?;
                                    }
                                event::KeyCode::Char('q') => break Ok(()),
                                event::KeyCode::Up => scroll_up(&mut status),
                                event::KeyCode::Down => scroll_down(&mut status),
                                event::KeyCode::Enter | event::KeyCode::Right => {
                                    status.status_bar = None;
                                    if let Some(selected) = status.selected.clone() {
                                        status.selected_index = (Some(selected.clone()), 0);
                                        status.selected = selected.borrow().children.first().cloned();
                                    }
                                }
                                event::KeyCode::Backspace | event::KeyCode::Left => {
                                    status.status_bar = None;
                                    if let Some(parent) = status.selected_index.0 {
                                        let parentparent = parent.borrow().parent.clone();
                                        status.selected_index = (
                                            parentparent.clone(),
                                            parentparent
                                                .map(|v| v.borrow().children.clone())
                                                .unwrap_or(status.root_notes.clone())
                                                .iter()
                                                .position(|v| *v == parent)
                                                .unwrap(),
                                        );
                                        status.selected = Some(parent.clone());
                                    }
                                }
                                event::KeyCode::Char('a') => {
                                    status.status_bar = None;
                                    status.current_dialog = Dialog::Adding(
                                        AddingInput::Data,
                                        (String::new(), NoteStatus::Incomplete, 0),
                                    );
                                }
                                event::KeyCode::Char('e') => {
                                    status.status_bar = None;
                                    if let Some(ref selected) = status.selected {
                                        let selected = selected.borrow();
                                        status.current_dialog = Dialog::Editing(
                                            AddingInput::Data,
                                            (selected.data.clone(), selected.status.clone(), 0),
                                        );
                                    }
                                }
                                event::KeyCode::Char('d') | event::KeyCode::Delete => {
                                    status.status_bar = None;
                                    if status.selected.is_some() {
                                        status.current_dialog = Dialog::Deleting(DeletingInput::Cancel);
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                    Event::Mouse(ev) => {
                        if ev.kind.is_scroll_down() {
                            scroll_down(&mut status)
                        } else if ev.kind.is_scroll_up() {
                            scroll_up(&mut status)
                        }
                    }
                    _ => {}
                }
            } else if let Event::Key(ev) = event::read()?
                && (ev.is_press() || ev.is_repeat())
            {
                match status.current_dialog {
                    Dialog::Deleting(ref mut inp) => match ev.code {
                        event::KeyCode::Enter | event::KeyCode::Char(' ') => {
                            if *inp == DeletingInput::Confirm {
                                let selection = status.selected.take().unwrap();
                                if let Some(parent) = &selection.borrow().parent {
                                    let pos = parent
                                        .borrow()
                                        .children
                                        .iter()
                                        .position(|v| *v == selection)
                                        .unwrap();
                                    parent.borrow_mut().children.remove(pos);
                                } else {
                                    let pos = status
                                        .root_notes
                                        .iter()
                                        .position(|v| *v == selection)
                                        .unwrap();
                                    status.root_notes.remove(pos);
                                }
                                scroll_up(&mut status);
                            }
                            status.current_dialog = Dialog::None;
                        }
                        event::KeyCode::Left => {
                            if *inp == DeletingInput::Confirm {
                                *inp = DeletingInput::Cancel
                            }
                        }
                        event::KeyCode::Right => {
                            if *inp == DeletingInput::Cancel {
                                *inp = DeletingInput::Confirm
                            }
                        }
                        _ => {}
                    },
                    Dialog::Adding(ref mut inp, (ref mut data, ref mut note_status, ref mut loc))
                    | Dialog::Editing(ref mut inp, (ref mut data, ref mut note_status, ref mut loc)) => {
                        match ev.code {
                            event::KeyCode::Char('q') if *inp != AddingInput::Data => {
                                status.current_dialog = Dialog::None;
                            }
                            event::KeyCode::Char(c) => {
                                if *inp == AddingInput::Data {
                                    data.insert(*loc, c);
                                    *loc += 1
                                }
                            }
                            event::KeyCode::Up => {
                                if *inp == AddingInput::Status {
                                    *inp = AddingInput::Data
                                } else if *inp == AddingInput::Submit {
                                    *inp = AddingInput::Status
                                }
                            }
                            event::KeyCode::Down => {
                                if *inp == AddingInput::Data {
                                    *inp = AddingInput::Status;
                                    *loc = data.len();
                                } else if *inp == AddingInput::Status {
                                    *inp = AddingInput::Submit
                                }
                            }
                            event::KeyCode::Left => {
                                if *inp == AddingInput::Status {
                                    *note_status = note_status.prev()
                                } else if *inp == AddingInput::Data {
                                    *loc = loc.saturating_sub(1)
                                }
                            }
                            event::KeyCode::Right => {
                                if *inp == AddingInput::Status {
                                    *note_status = note_status.next()
                                } else if *inp == AddingInput::Data && *loc < data.len() {
                                    *loc += 1
                                }
                            }
                            event::KeyCode::Backspace => {
                                if *inp == AddingInput::Data {
                                    if *loc >= data.len() {
                                        data.pop();
                                    } else if !data.is_empty() {
                                        data.remove(*loc - 1);
                                    }
                                    *loc = loc.saturating_sub(1);
                                }
                            }
                            event::KeyCode::Enter => {
                                if *inp == AddingInput::Submit {
                                    if !data.is_empty() {
                                        let data = data.clone();
                                        let note_status = note_status.clone();
                                        let t = OffsetDateTime::now_local()?;
                                        if status.current_dialog.is_adding() {
                                            let mut note_to_add = Note {
                                                data,
                                                created_at: t,
                                                edited_at: t,
                                                status: note_status,
                                                children: vec![],
                                                parent: None,
                                            };
                                            if let Some(add_to) = status.selected_index.0.as_ref() {
                                                note_to_add.parent = Some(add_to.clone());
                                                add_to
                                                    .borrow_mut()
                                                    .children
                                                    .push(Rc::new(RefCell::new(note_to_add)));
                                            } else {
                                                status
                                                    .root_notes
                                                    .push(Rc::new(RefCell::new(note_to_add)));
                                            }
                                        } else {
                                            let selected = status.selected.as_ref().unwrap();
                                            let mut edit = selected.borrow_mut();
                                            edit.data = data;
                                            edit.status = note_status;
                                            edit.edited_at = t;
                                        }
                                    }
                                    status.current_dialog = Dialog::None;
                                }
                            }
                            _ => {}
                        }
                    }
                    _ => {}
                }
            }
        }
    }
}

/// Widgets!
fn render(status: &mut Status) -> impl FnOnce(&mut ratatui::Frame<'_>) {
    |frame: &mut Frame| {
        use Constraint::{Fill, Length, Min, Ratio};

        match status.current_dialog {
            Dialog::Deleting(inp) => {
                let vertical = Layout::vertical([Fill(1), Min(12), Fill(1)]);
                let [_, dialog_area, _] = vertical.areas(frame.area());

                let input = Layout::vertical([Length(1), Length(1), Length(3)]);
                let [text_area, _, button_area] = input.areas(dialog_area);

                let horiz =
                    Layout::horizontal([Fill(1), Length(8), Ratio(1, 4), Length(9), Fill(1)]);
                let [_, cancel_area, _, confirm_area, _] = horiz.areas(button_area);

                frame.render_widget(Block::bordered(), dialog_area);

                frame.render_widget(
                    Paragraph::new(format!(
                        "Really delete \"{}\"?",
                        status.selected.as_ref().unwrap().borrow().data
                    ))
                    .bold()
                    .centered(),
                    text_area,
                );

                let para = Paragraph::new("Cancel").centered();
                frame.render_widget(
                    if inp == DeletingInput::Cancel {
                        para.bold()
                    } else {
                        para
                    }
                    .block(Block::bordered()),
                    cancel_area,
                );

                let para = Paragraph::new("Confirm").centered();
                frame.render_widget(
                    if inp == DeletingInput::Confirm {
                        para.bold()
                    } else {
                        para
                    }
                    .block(Block::bordered()),
                    confirm_area,
                );
            }
            Dialog::Adding(ref inp, (ref data, ref note_status, loc))
            | Dialog::Editing(ref inp, (ref data, ref note_status, loc)) => {
                let vertical = Layout::vertical([Fill(1), Min(12), Fill(1)]);
                let [_, dialog_area, _] = vertical.areas(frame.area());

                let input =
                    Layout::vertical([Length(3), Length(1), Length(3), Length(1), Length(3)]);
                let [data_area, _, status_area, _, submit_area] = input.areas(dialog_area);

                let horiz = Layout::horizontal([Ratio(3, 8), Ratio(1, 4), Ratio(3, 8)]);
                let [_, data_area, _] = horiz.areas(data_area);
                let [_, status_area, _] = horiz.areas(status_area);

                frame.render_widget(Block::bordered(), dialog_area);

                let para = Paragraph::new(data.clone()).left_aligned();
                frame.render_widget(
                    if *inp == AddingInput::Data {
                        para.bold()
                    } else {
                        para
                    }
                    .block(Block::bordered().title("Note")),
                    data_area,
                );

                if *inp == AddingInput::Data {
                    frame.set_cursor_position((data_area.x + 1 + loc as u16, data_area.y + 1));
                }

                let para = Paragraph::new(format!("{}", note_status)).centered();
                frame.render_widget(
                    if *inp == AddingInput::Status {
                        para.bold()
                    } else {
                        para
                    }
                    .block(Block::bordered().title("Status")),
                    status_area,
                );

                let para = Paragraph::new("Submit").centered();
                frame.render_widget(
                    if *inp == AddingInput::Submit {
                        para.bold()
                    } else {
                        para
                    }
                    .block(Block::bordered()),
                    submit_area,
                );
            }
            Dialog::None => {
                let vertical = Layout::vertical([Length(1), Min(0), Length(1)]);
                let [title_area, main_area, status_area] = vertical.areas(frame.area());
                let horizontal = Layout::horizontal([Fill(2), Fill(1)]);
                let [left_area, right_area] = horizontal.areas(main_area);

                frame.render_widget(
                    Block::new().borders(Borders::TOP).title("Notation").red(),
                    title_area,
                );
                frame.render_widget(
                    Block::new().borders(Borders::BOTTOM).title(
                        if let Some(ref note) = status.selected {
                            status.status_bar = None;
                            let note = note.borrow();
                            format!(
                                "{} - created at {}{}",
                                color_eyre::owo_colors::OwoColorize::green(&note.status.to_string()),
                                color_eyre::owo_colors::OwoColorize::blue(&note.created_at
                                    .format(&time::format_description::well_known::Rfc2822)
                                    .unwrap()),
                                if note
                                    .created_at
                                    .format(&time::format_description::well_known::Rfc2822)
                                    .unwrap()
                                    != note
                                        .edited_at
                                        .format(&time::format_description::well_known::Rfc2822)
                                        .unwrap()
                                {
                                    format!(
                                        "; last edited at {}",
                                        color_eyre::owo_colors::OwoColorize::blue(&note.edited_at
                                            .format(&time::format_description::well_known::Rfc2822)
                                            .unwrap())
                                    )
                                } else {
                                    String::new()
                                }
                            )
                        } else if let Some(status_bar) = status.status_bar.clone() {
                            status_bar
                        } else {
                            status.status_bar = None;
                            String::new()
                        },
                    ).red(),
                    status_area,
                );
                let selected_fn = |v: &Rc<RefCell<Note>>| {
                    let mut data = Text::raw(v.borrow().data.clone());
                    if status.selected.as_ref().is_some_and(|v2| *v2 == *v) {
                        data = data.reversed()
                    }
                    data
                };
                let note_list = List::new(
                    if status.selected_index.0.is_some() {
                        status
                            .selected_index
                            .0
                            .as_ref()
                            .unwrap()
                            .borrow()
                            .children
                            .clone()
                    } else {
                        status.root_notes.clone()
                    }
                    .iter()
                    .map(selected_fn),
                );
                frame.render_widget(note_list.block(Block::bordered().title("Notes")).green(), left_area);

                if status.selected.is_some()
                    && !status
                        .selected
                        .as_ref()
                        .unwrap()
                        .borrow()
                        .children
                        .is_empty()
                {
                    let children_list = List::new(
                        status
                            .selected
                            .as_ref()
                            .unwrap()
                            .clone()
                            .borrow()
                            .children
                            .iter()
                            .map(selected_fn),
                    );
                    frame.render_widget(
                        children_list.block(Block::bordered().title("Children")).cyan(),
                        right_area,
                    );
                } else if let Some(note) = status
                    .selected
                    .as_ref()
                    .or(status.selected_index.0.as_ref())
                {
                    let mut current_note = note.clone();

                    let mut tree = Vec::new();
                    let mut lines = 0usize;

                    loop {
                        if lines >= frame.area().height as usize - 1 {
                            tree.push("...".to_string());
                            break;
                        }

                        let data = current_note.borrow().data.clone();
                        let mut data = data.split_at(data.len().min(29)).0.to_string();
                        if data.len() == 29 {
                            data.push_str("...");
                        }
                        tree.push(data);

                        if current_note.borrow().parent.is_none() {
                            break;
                        }

                        current_note = current_note.clone().borrow().parent.clone().unwrap();
                        lines += 1;
                    }
                    let tree = List::new(tree);
                    frame.render_widget(tree.block(Block::bordered().title("Tree")).magenta(), right_area);
                }
            }
        }
    }
}
