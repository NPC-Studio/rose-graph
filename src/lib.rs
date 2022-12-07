#![allow(clippy::bool_comparison)]

use std::{
    cmp::Ordering,
    collections::BTreeMap,
    fmt, iter,
    ops::{Deref, DerefMut},
};

// use camino::Utf8Path;
pub const ROOT: &str = "";
pub const ROOT_OWNED: String = String::new();

pub mod tree_utils {
    pub const ROOT: &str = "";
    pub const ROOT_OWNED: String = String::new();
    pub const DELIMITER: char = '/';

    /// Returns the parent of a given path
    pub fn parent_path(input: &(impl AsRef<str> + ?Sized)) -> &str {
        input
            .as_ref()
            .rsplit_once(DELIMITER)
            .map(|(lhs, _rhs)| lhs)
            .unwrap_or(ROOT)
    }

    pub fn path_components(input: &(impl AsRef<str> + ?Sized)) -> impl Iterator<Item = &str> {
        input
            .as_ref()
            .split(DELIMITER)
            .filter(|v| v.is_empty() == false)
    }

    /// Returns the bare file name on a given path
    pub fn file_name(input: &(impl AsRef<str> + ?Sized)) -> &str {
        input
            .as_ref()
            .rsplit_once(DELIMITER)
            .map(|(_lhs, rhs)| rhs)
            .unwrap_or_else(|| input.as_ref())
    }

    pub fn join(input: &(impl AsRef<str> + ?Sized), second: &(impl AsRef<str> + ?Sized)) -> String {
        let input = input.as_ref();
        let second = second.as_ref();

        if input == ROOT {
            second.to_string()
        } else {
            format!("{}{}{}", input, DELIMITER, second)
        }
    }

    /// A history of paths that should be open within a tree.
    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize, Default)]
    pub struct TreeHistory(super::UiTree<(), ()>);

    impl TreeHistory {
        pub fn open_path(
            &mut self,
            path: &(impl AsRef<str> + ?Sized),
        ) -> Result<(), crate::CosmicPathErr> {
            self.0.add_folder_all(path)
        }

        pub fn close_path(
            &mut self,
            path: &(impl AsRef<str> + ?Sized),
        ) -> Result<(), crate::CosmicPathErr> {
            self.0.remove_item(path).map(|_| ())
        }

        pub fn is_open(&self, path: &(impl AsRef<str> + ?Sized)) -> bool {
            self.0.item_exists(path)
        }
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, Default)]
pub struct UiTree<T, F>(UiTreeFolder<T, F>);

impl<T, F> UiTree<T, F> {
    pub fn new(root_value: F) -> Self {
        Self(UiTreeFolder {
            value: root_value,
            children: Default::default(),
        })
    }

    pub fn iter_paths(&self) -> impl Iterator<Item = (String, UiTreeItemRef<'_, T, F>)> {
        self.0.iter_paths(ROOT_OWNED).skip(1)
    }

    pub fn iter(&self) -> impl Iterator<Item = UiTreeItemRef<'_, T, F>> {
        self.0.iter().skip(1)
    }
}

impl<T, F> Deref for UiTree<T, F> {
    type Target = UiTreeFolder<T, F>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T, F> DerefMut for UiTree<T, F> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize, Default)]
pub struct UiTreeFolder<T, F = ()> {
    pub value: F,
    pub children: BTreeMap<String, Box<UiTreeItem<T, F>>>,
}

impl<T, F> UiTreeFolder<T, F> {
    pub fn new(value: F) -> Self {
        Self {
            value,
            children: Default::default(),
        }
    }

    /// Clears this folder, and sets a new value for it.
    pub fn clear(&mut self, value: F) {
        self.value = value;
        self.children.clear();
    }

    pub fn unique_path(
        &self,
        folder: &(impl AsRef<str> + ?Sized),
        base_file_name: &(impl AsRef<str> + ?Sized),
    ) -> String {
        let base_file_name = base_file_name.as_ref();
        let mut path = tree_utils::join(folder, base_file_name);
        let mut add = 0;
        while self.item_exists(&path) {
            add += 1;

            path = tree_utils::join(folder, &format!("{} {}", base_file_name, add));
        }

        path
    }

    pub fn file(&self, path: &(impl AsRef<str> + ?Sized)) -> Option<&T> {
        match self.item(path)? {
            UiTreeItemRef::Folder(_) => None,
            UiTreeItemRef::File(file) => Some(file),
        }
    }

    pub fn folder(&self, path: &(impl AsRef<str> + ?Sized)) -> Option<&UiTreeFolder<T, F>> {
        match self.item(path)? {
            UiTreeItemRef::Folder(folder) => Some(folder),
            UiTreeItemRef::File(_) => None,
        }
    }

    /// Returns the root item. This is *always* a Folder.
    pub fn root(&self) -> UiTreeItemRef<'_, T, F> {
        UiTreeItemRef::Folder(self)
    }

    pub fn item(&self, path: &(impl AsRef<str> + ?Sized)) -> Option<UiTreeItemRef<'_, T, F>> {
        let mut folder = self;
        let mut iter = tree_utils::path_components(path);

        while let Some(cmp) = iter.next() {
            match folder.children.get(cmp)?.as_ref() {
                UiTreeItem::Folder(new_folder) => {
                    folder = new_folder;
                }
                UiTreeItem::File(file) => {
                    if iter.next().is_some() {
                        // oopsie!
                        return None;
                    } else {
                        return Some(UiTreeItemRef::File(file));
                    }
                }
            };
        }

        Some(UiTreeItemRef::Folder(folder))
    }

    pub fn item_mut(
        &mut self,
        path: &(impl AsRef<str> + ?Sized),
    ) -> Option<UiTreeItemRefMut<'_, T, F>> {
        let mut folder = self;
        let mut iter = tree_utils::path_components(path);

        while let Some(cmp) = iter.next() {
            match folder.children.get_mut(cmp)?.as_mut() {
                UiTreeItem::Folder(new_folder) => {
                    folder = new_folder;
                }
                UiTreeItem::File(file) => {
                    if iter.next().is_some() {
                        // oopsie!
                        return None;
                    } else {
                        return Some(UiTreeItemRefMut::File(file));
                    }
                }
            };
        }

        Some(UiTreeItemRefMut::Folder(folder))
    }

    pub fn file_mut(&mut self, path: &(impl AsRef<str> + ?Sized)) -> Option<&mut T> {
        match self.item_mut(path)? {
            UiTreeItemRefMut::Folder(_) => None,
            UiTreeItemRefMut::File(file) => Some(file),
        }
    }

    pub fn folder_mut(
        &mut self,
        path: &(impl AsRef<str> + ?Sized),
    ) -> Option<&mut UiTreeFolder<T, F>> {
        match self.item_mut(path)? {
            UiTreeItemRefMut::Folder(folder) => Some(folder),
            UiTreeItemRefMut::File(_) => None,
        }
    }

    pub fn item_exists(&self, path: &(impl AsRef<str> + ?Sized)) -> bool {
        self.item(path).is_some()
    }

    pub fn folder_exists(&self, path: &(impl AsRef<str> + ?Sized)) -> bool {
        self.item(path).map(|v| v.is_folder()).unwrap_or_default()
    }

    pub fn file_exists(&self, path: &(impl AsRef<str> + ?Sized)) -> bool {
        self.item(path).map(|v| v.is_file()).unwrap_or_default()
    }

    pub fn add_item(
        &mut self,
        path: &(impl AsRef<str> + ?Sized),
        new_item: UiTreeItem<T, F>,
    ) -> Result<(), CosmicPathErr> {
        let parent_path = tree_utils::parent_path(path);
        let child_name = tree_utils::file_name(path);
        if child_name.is_empty() {
            return Err(CosmicPathErr::EmptyName);
        }

        let folder = self.item_mut(parent_path).ok_or(CosmicPathErr::BadPath)?;
        let folder = match folder {
            UiTreeItemRefMut::Folder(f) => f,
            UiTreeItemRefMut::File(_) => return Err(CosmicPathErr::BadPath),
        };

        if folder.children.get(child_name).is_some() {
            return Err(CosmicPathErr::DuplicateName);
        }

        folder
            .children
            .insert(child_name.to_owned(), Box::new(new_item));

        Ok(())
    }

    pub fn add_file(
        &mut self,
        path: &(impl AsRef<str> + ?Sized),
        value: T,
    ) -> Result<(), CosmicPathErr> {
        self.add_item(path, UiTreeItem::File(value))
    }

    pub fn add_folder(
        &mut self,
        path: &(impl AsRef<str> + ?Sized),
        folder_value: F,
    ) -> Result<(), CosmicPathErr> {
        self.add_item(
            path,
            UiTreeItem::Folder(UiTreeFolder {
                value: folder_value,
                children: Default::default(),
            }),
        )
    }

    pub fn remove_item(
        &mut self,
        path: &(impl AsRef<str> + ?Sized),
    ) -> Result<UiTreeItem<T, F>, CosmicPathErr> {
        let parent_path = tree_utils::parent_path(path);
        let child_name = tree_utils::file_name(path);

        let item = self.item_mut(parent_path).ok_or(CosmicPathErr::BadPath)?;
        let folder = match item {
            UiTreeItemRefMut::Folder(f) => f,
            UiTreeItemRefMut::File(_) => return Err(CosmicPathErr::BadPath),
        };

        let output = folder
            .children
            .remove(child_name)
            .ok_or(CosmicPathErr::BadPath)?;

        Ok(*output)
    }

    pub fn remove_file(&mut self, path: &(impl AsRef<str> + ?Sized)) -> Result<T, CosmicPathErr> {
        // ie, it must be a file, not a folder.
        if self.file(path).is_none() {
            return Err(CosmicPathErr::BadPath);
        }

        match self.remove_item(path)? {
            UiTreeItem::Folder(_) => unimplemented!(),
            UiTreeItem::File(f) => Ok(f),
        }
    }

    pub fn remove_folder(
        &mut self,
        path: &(impl AsRef<str> + ?Sized),
    ) -> Result<UiTreeFolder<T, F>, CosmicPathErr> {
        if self.folder(path).is_none() {
            return Err(CosmicPathErr::BadPath);
        }

        match self.remove_item(path)? {
            UiTreeItem::Folder(f) => Ok(f),
            UiTreeItem::File(_) => unimplemented!(),
        }
    }

    pub fn move_item(
        &mut self,
        old_path: &(impl AsRef<str> + ?Sized),
        new_path: &(impl AsRef<str> + ?Sized),
    ) -> Result<(), CosmicPathErr> {
        let old_path = old_path.as_ref();
        let new_path = new_path.as_ref();

        if old_path == new_path {
            return Ok(());
        }

        // confirm the new path will work before we make the cut...
        {
            let parent_path = tree_utils::parent_path(new_path);
            let child_name = tree_utils::file_name(new_path);

            let new_parent = self.folder(parent_path).ok_or(CosmicPathErr::BadPath)?;
            if new_parent.children.contains_key(child_name) {
                return Err(CosmicPathErr::DuplicateName);
            }

            // check if new_path fully contains the old path -- if that's the case,
            // then we're moving a folder into its own subfolders, which is not allowed.
            let mut new_iter = tree_utils::path_components(new_path);

            let mut success = false;

            for old_path in tree_utils::path_components(old_path) {
                if Some(old_path) != new_iter.next() {
                    success = true;
                    break;
                }
            }

            if success == false {
                return Err(CosmicPathErr::BadPath);
            }

            let old_path_with_delim = format!("{}{}", old_path, tree_utils::DELIMITER);
            if new_path.starts_with(&old_path_with_delim) {
                return Err(CosmicPathErr::BadPath);
            }
        }

        // cut
        let node = self.remove_item(old_path)?;

        // re-add and unwrap (this should never fail...)
        self.add_item(new_path, node).unwrap();

        Ok(())
    }

    pub fn files(&self) -> impl Iterator<Item = &'_ T> {
        self.children.values().flat_map(|n| {
            let x: Box<dyn Iterator<Item = &T>> = match n.as_ref() {
                UiTreeItem::Folder(f) => Box::new(f.files()),
                UiTreeItem::File(f) => Box::new(iter::once(f)),
            };

            x
        })
    }

    pub fn files_mut(&mut self) -> impl Iterator<Item = &'_ mut T> {
        self.children.values_mut().flat_map(|n| {
            let x: Box<dyn Iterator<Item = &mut T>> = match n.as_mut() {
                UiTreeItem::Folder(f) => Box::new(f.files_mut()),
                UiTreeItem::File(f) => Box::new(iter::once(f)),
            };

            x
        })
    }

    pub fn files_paths(&self, base_path: String) -> impl Iterator<Item = (String, &'_ T)> {
        self.children.iter().flat_map(move |(subpath, n)| {
            let path = tree_utils::join(&base_path, subpath);
            let x: Box<dyn Iterator<Item = (String, &T)>> = match n.as_ref() {
                UiTreeItem::Folder(f) => Box::new(f.files_paths(path)),
                UiTreeItem::File(f) => Box::new(iter::once((path, f))),
            };

            x
        })
    }

    pub fn files_paths_mut(
        &mut self,
        base_path: String,
    ) -> impl Iterator<Item = (String, &'_ mut T)> {
        self.children.iter_mut().flat_map(move |(subpath, n)| {
            let path = tree_utils::join(&base_path, subpath);
            let x: Box<dyn Iterator<Item = (String, &mut T)>> = match n.as_mut() {
                UiTreeItem::Folder(f) => Box::new(f.files_paths_mut(path)),
                UiTreeItem::File(f) => Box::new(iter::once((path, f))),
            };

            x
        })
    }

    /// Directly sets the children
    pub fn set_children(&mut self, children: BTreeMap<String, Box<UiTreeItem<T, F>>>) {
        self.children = children;
    }

    pub fn iter(&self) -> impl Iterator<Item = UiTreeItemRef<'_, T, F>> {
        iter::once(UiTreeItemRef::Folder(self)).chain(self.children.iter().flat_map(
            move |(_subpath, item)| {
                let x: Box<dyn Iterator<Item = UiTreeItemRef<'_, T, F>>> = match item.as_ref() {
                    UiTreeItem::Folder(f) => Box::new(f.iter()),
                    UiTreeItem::File(f) => Box::new(iter::once(UiTreeItemRef::File(f))),
                };

                x
            },
        ))
    }

    pub fn iter_paths(
        &self,
        base: String,
    ) -> impl Iterator<Item = (String, UiTreeItemRef<'_, T, F>)> {
        iter::once((base.clone(), UiTreeItemRef::Folder(self))).chain(
            self.children.iter().flat_map(move |(subpath, item)| {
                let path = tree_utils::join(&base, subpath);

                let x: Box<dyn Iterator<Item = (String, UiTreeItemRef<'_, T, F>)>> =
                    match item.as_ref() {
                        UiTreeItem::Folder(f) => Box::new(f.iter_paths(path)),
                        UiTreeItem::File(f) => Box::new(iter::once((path, UiTreeItemRef::File(f)))),
                    };

                x
            }),
        )
    }

    /// Performs an action
    pub fn for_each_mut<R>(
        &mut self,
        empty_folders_pass: bool,
        filter: impl Fn(&str, &T) -> bool + Copy,
        mut on_item: impl FnMut(&str, &mut UiTreeItemRefMut<'_, T, F>) -> Option<R>,
    ) {
        fn observer<T, F, R>(
            full_path: &str,
            input: &mut BTreeMap<String, Box<UiTreeItem<T, F>>>,
            filter: impl Fn(&str, &T) -> bool + Copy,
            empty_folders_pass: bool,
            on_item: &mut impl FnMut(&str, &mut UiTreeItemRefMut<'_, T, F>) -> Option<R>,
        ) {
            let intermediary_sort = {
                let mut intermediary_sort: Vec<_> = input.iter_mut().collect();
                intermediary_sort.sort_unstable_by(|lhs, rhs| {
                    if lhs.1.is_folder() && rhs.1.is_file() {
                        Ordering::Less
                    } else if rhs.1.is_folder() {
                        Ordering::Greater
                    } else {
                        let lhs = ListSorter::new(lhs.0);
                        let rhs = ListSorter::new(rhs.0);

                        lhs.cmp(&rhs)
                    }
                });
                intermediary_sort
            };

            for (new_name, child) in intermediary_sort {
                let new_child_full_path = tree_utils::join(full_path, new_name);

                let passed = match child.to_reference() {
                    UiTreeItemRef::Folder(folder) => {
                        let mut found_file = false;
                        let mut output = folder
                            .iter_paths(new_child_full_path.to_string())
                            .filter_map(|(path, v)| match v {
                                UiTreeItemRef::Folder(_) => None,
                                UiTreeItemRef::File(file_data) => {
                                    found_file = true;
                                    Some((path, file_data))
                                }
                            })
                            .any(|(path, v)| filter(&path, v));

                        if output == false && found_file == false && empty_folders_pass {
                            output = true;
                        }

                        output
                    }
                    UiTreeItemRef::File(file) => filter(&new_child_full_path, file),
                };
                if passed {
                    let mut child = child.to_reference_mut();
                    if let Some(_inner) = on_item(&new_child_full_path, &mut child) {
                        if let UiTreeItemRefMut::Folder(folder) = child {
                            observer(
                                &new_child_full_path,
                                &mut folder.children,
                                filter,
                                empty_folders_pass,
                                on_item,
                            );
                        }
                    }
                }
            }
        }

        observer(
            tree_utils::ROOT,
            &mut self.children,
            filter,
            empty_folders_pass,
            &mut on_item,
        );
    }

    /// Performs an action
    pub fn for_each<R>(
        &self,
        empty_folders_pass: bool,
        filter: impl Fn(&str, &T) -> bool + Copy,
        mut on_item: impl FnMut(&str, &UiTreeItemRef<'_, T, F>) -> Option<R>,
    ) {
        fn observer<T, F, R>(
            full_path: &str,
            input: &BTreeMap<String, Box<UiTreeItem<T, F>>>,
            filter: impl Fn(&str, &T) -> bool + Copy,
            empty_folders_pass: bool,
            on_item: &mut impl FnMut(&str, &UiTreeItemRef<'_, T, F>) -> Option<R>,
        ) {
            let intermediary_sort = {
                let mut intermediary_sort: Vec<_> = input.iter().collect();
                intermediary_sort.sort_unstable_by(|lhs, rhs| {
                    if lhs.1.is_folder() && rhs.1.is_file() {
                        Ordering::Less
                    } else if rhs.1.is_folder() {
                        Ordering::Greater
                    } else {
                        let lhs = ListSorter::new(lhs.0);
                        let rhs = ListSorter::new(rhs.0);

                        lhs.cmp(&rhs)
                    }
                });
                intermediary_sort
            };

            for (new_name, child) in intermediary_sort {
                let new_child_full_path = tree_utils::join(full_path, new_name);

                let passed = match child.to_reference() {
                    UiTreeItemRef::Folder(folder) => {
                        let mut found_file = false;
                        let mut output = folder
                            .iter_paths(new_child_full_path.to_string())
                            .filter_map(|(path, v)| match v {
                                UiTreeItemRef::Folder(_) => None,
                                UiTreeItemRef::File(file_data) => {
                                    found_file = true;
                                    Some((path, file_data))
                                }
                            })
                            .any(|(path, v)| filter(&path, v));

                        if output == false && found_file == false && empty_folders_pass {
                            output = true;
                        }

                        output
                    }
                    UiTreeItemRef::File(file) => filter(&new_child_full_path, file),
                };
                if passed {
                    let child = child.to_reference();
                    if let Some(_inner) = on_item(&new_child_full_path, &child) {
                        if let UiTreeItemRef::Folder(folder) = child {
                            observer(
                                &new_child_full_path,
                                &folder.children,
                                filter,
                                empty_folders_pass,
                                on_item,
                            );
                        }
                    }
                }
            }
        }

        observer(
            tree_utils::ROOT,
            &self.children,
            filter,
            empty_folders_pass,
            &mut on_item,
        );
    }
}

impl<T, F> UiTreeFolder<T, F>
where
    F: Default,
{
    pub fn add_folder_all(
        &mut self,
        path: &(impl AsRef<str> + ?Sized),
    ) -> Result<(), CosmicPathErr> {
        let mut folder = self;

        for component in tree_utils::path_components(path) {
            if let Err(e) = folder.add_folder(component, F::default()) {
                match e {
                    CosmicPathErr::BadPath
                    | CosmicPathErr::EmptyName
                    | CosmicPathErr::EmptySubpath => {
                        return Err(e);
                    }
                    // dupe is fine...
                    CosmicPathErr::DuplicateName => {}
                }
            }

            folder = folder.folder_mut(component).unwrap();
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum UiTreeItem<T, F> {
    Folder(UiTreeFolder<T, F>),
    File(T),
}

impl<T, F> UiTreeItem<T, F> {
    pub fn to_reference(&self) -> UiTreeItemRef<'_, T, F> {
        match self {
            UiTreeItem::Folder(folder) => UiTreeItemRef::Folder(folder),
            UiTreeItem::File(file) => UiTreeItemRef::File(file),
        }
    }

    pub fn to_reference_mut(&mut self) -> UiTreeItemRefMut<'_, T, F> {
        match self {
            UiTreeItem::Folder(folder) => UiTreeItemRefMut::Folder(folder),
            UiTreeItem::File(file) => UiTreeItemRefMut::File(file),
        }
    }

    /// Returns `true` if the ui_tree_item_ref is [`Folder`].
    pub fn is_folder(&self) -> bool {
        matches!(self, Self::Folder(..))
    }

    /// Returns `true` if the ui_tree_item_ref is [`Folder`].
    pub fn as_folder(&self) -> Option<&UiTreeFolder<T, F>> {
        if let UiTreeItem::Folder(v) = self {
            Some(v)
        } else {
            None
        }
    }

    /// Returns `Some` if the ui_tree_item_ref is [`Folder`].
    pub fn as_folder_mut(&mut self) -> Option<&mut UiTreeFolder<T, F>> {
        if let UiTreeItem::Folder(v) = self {
            Some(v)
        } else {
            None
        }
    }

    /// Returns `true` if the ui_tree_item_ref is [`Folder`].
    pub fn as_file(&self) -> Option<&T> {
        if let UiTreeItem::File(v) = self {
            Some(v)
        } else {
            None
        }
    }

    /// Returns `true` if the ui_tree_item_ref is [`File`].
    pub fn is_file(&self) -> bool {
        matches!(self, Self::File(..))
    }
}

impl<T> UiTreeItem<T, T> {
    pub fn data(&self) -> &T {
        self.to_reference().data()
    }

    pub fn data_mut(&mut self) -> &mut T {
        self.to_reference_mut().data_mut()
    }

    pub fn data_owned(self) -> T {
        match self {
            UiTreeItem::Folder(t) => t.value,
            UiTreeItem::File(t) => t,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum UiTreeItemRef<'a, T, F> {
    Folder(&'a UiTreeFolder<T, F>),
    File(&'a T),
}

impl<'a, T, F> Clone for UiTreeItemRef<'a, T, F> {
    fn clone(&self) -> Self {
        match self {
            Self::Folder(arg0) => Self::Folder(arg0),
            Self::File(arg0) => Self::File(arg0),
        }
    }
}
impl<'a, T, F> Copy for UiTreeItemRef<'a, T, F> {}

impl<'a, T, F> UiTreeItemRef<'a, T, F> {
    /// Returns `true` if the ui_tree_item_ref is [`Folder`].
    pub fn is_folder(&self) -> bool {
        matches!(self, Self::Folder(..))
    }

    /// Returns `true` if the ui_tree_item_ref is [`File`].
    pub fn is_file(&self) -> bool {
        matches!(self, Self::File(..))
    }

    /// Returns `true` if the ui_tree_item_ref is [`File`].
    pub fn as_file(self) -> Option<&'a T> {
        if let Self::File(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

// for the case where they're the same thing...
impl<'a, T> UiTreeItemRef<'a, T, T> {
    pub fn data(self) -> &'a T {
        match self {
            UiTreeItemRef::Folder(t) => &t.value,
            UiTreeItemRef::File(t) => t,
        }
    }
}

impl<'a, T, F> UiTreeItemRef<'a, T, F>
where
    T: Clone,
    F: Clone,
{
    pub fn to_owned(&self) -> UiTreeItem<T, F> {
        match *self {
            UiTreeItemRef::Folder(f) => UiTreeItem::Folder(f.clone()),
            UiTreeItemRef::File(f) => UiTreeItem::File(f.clone()),
        }
    }
}

#[derive(Debug)]
pub enum UiTreeItemRefMut<'a, T, F> {
    Folder(&'a mut UiTreeFolder<T, F>),
    File(&'a mut T),
}

impl<'a, T, F> UiTreeItemRefMut<'a, T, F> {
    /// Returns `true` if the ui_tree_item_ref is [`Folder`].
    pub fn is_folder(&self) -> bool {
        matches!(self, Self::Folder(..))
    }

    /// Returns `true` if the ui_tree_item_ref is [`File`].
    pub fn is_file(&self) -> bool {
        matches!(self, Self::File(..))
    }
}

// for the case where they're the same thing...
impl<'a, T> UiTreeItemRefMut<'a, T, T> {
    pub fn data_mut(self) -> &'a mut T {
        match self {
            UiTreeItemRefMut::Folder(t) => &mut t.value,
            UiTreeItemRefMut::File(t) => t,
        }
    }
}

/// Just basically naming that the Error is "the path didn't exist"
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum CosmicPathErr {
    BadPath,
    EmptyName,
    EmptySubpath,
    DuplicateName,
}

impl fmt::Display for CosmicPathErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let st = match self {
            CosmicPathErr::BadPath => "Path was not valid.",
            CosmicPathErr::DuplicateName => {
                "Attempted to add a new node with the same name as an existing node"
            }
            CosmicPathErr::EmptySubpath => {
                "Attempted to add a chid with the empty subpath \"\" as part of its path."
            }
            CosmicPathErr::EmptyName => {
                "An empty name is never a valid name for a file or a folder, except the ROOT."
            }
        };

        f.pad(st)
    }
}
impl std::error::Error for CosmicPathErr {}

/// This is a ZST for LeafTrees.
pub struct LeafTreeFoldersHaveNoData;

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(transparent)]
#[repr(transparent)]
pub struct LeafTree<T>(UiTree<T, ()>);

impl<T> Deref for LeafTree<T> {
    type Target = UiTree<T, ()>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for LeafTree<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T> LeafTree<T> {
    pub fn new() -> Self {
        Self(UiTree::new(()))
    }

    /// Adds a folder into the LeafTree.
    /// We removed the value argument in this wrapper, since all values for Folder data
    /// is `()`.
    pub fn add_folder(&mut self, path: &(impl AsRef<str> + ?Sized)) -> Result<(), CosmicPathErr> {
        self.0.add_folder(path, ())
    }

    /// This is a wrapper around a standard iterator. Note that it skips the root value, which
    /// is always `()`.
    pub fn iter(&self) -> impl Iterator<Item = (String, UiTreeItemRef<'_, T, ()>)> {
        self.0.iter_paths()
    }

    /// This is a wrapper around a standard iterator. Note that it skips the root value, which
    /// is always `()`.
    pub fn files_paths(&self) -> impl Iterator<Item = (String, &T)> {
        self.0.files_paths(tree_utils::ROOT_OWNED)
    }

    /// Awaken afresh, as if it were all just a bad dream
    pub fn clear(&mut self) {
        self.0.clear(());
    }
}

impl<T> Default for LeafTree<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<Tree> FromIterator<(String, Tree)> for LeafTree<Tree> {
    fn from_iter<T: IntoIterator<Item = (String, Tree)>>(iter: T) -> Self {
        let mut me = Self::new();

        for (path, v) in iter {
            let parent_path = tree_utils::parent_path(&path);

            me.add_folder_all(parent_path).unwrap();
            me.add_file(&path, v).unwrap();
        }

        me
    }
}

#[derive(Debug, PartialEq, Eq)]
struct ListSorter<'a>(Option<i32>, &'a str);

impl<'a> ListSorter<'a> {
    fn new(input: &'a str) -> Self {
        match input.split_once('.') {
            Some((lhs, rhs)) => match lhs.parse::<i32>().ok() {
                Some(v) => Self(Some(v), rhs),
                None => Self(None, input),
            },
            None => Self(None, input),
        }
    }
}

impl<'a> PartialOrd for ListSorter<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<'a> Ord for ListSorter<'a> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (&self.0, &other.0) {
            (Some(my_num), Some(their_num)) => {
                let ord = my_num.cmp(their_num);
                if ord != Ordering::Equal {
                    return ord;
                } else {
                    // continue
                }
            }
            (Some(_), None) => {
                return Ordering::Less;
            }
            (None, Some(_)) => {
                return Ordering::Greater;
            }
            (None, None) => {
                // continue
            }
        }

        self.1.cmp(self.1)
    }
}

#[cfg(test)]
mod cosmic_tree_tests {
    use super::*;

    #[test]
    fn add_children() {
        let mut tree = LeafTree::new();

        tree.add_folder("bean_zero").unwrap();
        tree.add_file("bean_one", 1).unwrap();

        assert!(tree.folder("bean_zero").is_some());
        assert_eq!(*tree.file("bean_one").unwrap(), 1);

        tree.add_file("bean_zero/bean_three", 3).unwrap();
        assert!(tree.item_exists("bean_zero/bean_three"));
        assert_eq!(*tree.file("bean_zero/bean_three").unwrap(), 3);

        // we expect that an empty path works fine
        assert!(tree.item_exists(ROOT));
        assert!(tree.item_exists(""));

        // make sure we can't double add...
        assert_eq!(
            tree.add_file("bean_zero", 0).unwrap_err(),
            CosmicPathErr::DuplicateName
        );

        // make sure we can't double add...
        assert_eq!(
            tree.add_file("bean_zero/bean_three", 3).unwrap_err(),
            CosmicPathErr::DuplicateName
        );

        // and check for bad paths...
        assert_eq!(
            tree.add_file("bean_zero/bean_four/bean_three", 5)
                .unwrap_err(),
            CosmicPathErr::BadPath
        );
        // and check for bad paths...
        assert_eq!(tree.add_file("", 5).unwrap_err(), CosmicPathErr::EmptyName);

        // and check for empty paths...
        assert_eq!(tree.add_file("", 10).unwrap_err(), CosmicPathErr::EmptyName);
        assert_eq!(
            tree.add_file("bean_zero", 10).unwrap_err(),
            CosmicPathErr::DuplicateName
        );
        assert_eq!(
            tree.add_file("bean_zero/", 10).unwrap_err(),
            CosmicPathErr::EmptyName
        );
    }

    #[test]
    fn remove_children() {
        let mut tree = LeafTree::new();

        // testing whether we can remove from root
        tree.add_file("doomed_bean", 0).unwrap();
        tree.remove_file("doomed_bean").unwrap();
        assert!(!tree.item_exists("doomed_bean"));

        // a child deletion
        tree.add_folder("bean_zero").unwrap();
        tree.add_file("bean_zero/bean_child", 1).unwrap();

        assert_eq!(*tree.file("bean_zero/bean_child").unwrap(), 1);

        assert!(tree.item_exists("bean_zero/bean_child"));
        assert!(!tree.item_exists("bean_zero/fake_bongo_child"));

        assert_eq!(tree.remove_file("bean_zero/bean_child").unwrap(), 1);

        // should be false as it has now been deleted
        assert!(!tree.item_exists("bean_zero/bean_child"));
        // normal remove_child does not move to root
        assert!(!tree.item_exists("bean_child"));

        // alrighty now we defeat grandchildren
        tree.add_folder("grandma").unwrap();
        tree.add_folder("grandma/mum").unwrap();
        tree.add_file("grandma/mum/kid", 2).unwrap();

        // sorry grandma
        let egg = tree.remove_folder("grandma").unwrap();

        // paths should all be invalid now
        assert!(!tree.item_exists("grandma/mum/kid"));
        assert!(!tree.item_exists("grandma/mum"));
        assert!(!tree.item_exists("grandma"));

        // the egg is valid!
        assert!(egg.item_exists(""));
        assert!(egg.item_exists("mum"));
        assert!(egg.item_exists("mum/kid"));
    }

    #[test]
    fn move_children() {
        let mut tree = LeafTree::new();

        // let's test moving a child to the root
        tree.add_folder("a").unwrap();
        tree.add_file("a/b", 1).unwrap();
        // and another one, we'll check it this one is still there after
        tree.add_file("a/duck", 2).unwrap();
        // alright move it to root
        tree.move_item("a/b", "b").unwrap();
        // hokay bean zero child shouldn't be with bean zero anymore
        assert!(!tree.item_exists("a/b"));
        // should be on the root
        assert!(tree.item_exists("b"));
        // check our other child is still there
        assert!(tree.item_exists("a/duck"));

        // just double check our length is correct (a, b, duck)
        // assert_eq!(tree.children.len(), 3);
        // should have bean_zero and bean_zero_child
        assert_eq!(tree.children.len(), 2);

        // alright let's try rehoming our child to ducky dad
        tree.add_folder("ducky_dad").unwrap();
        tree.move_item("a/duck", "ducky_dad/duck").unwrap();

        // are we waddling along happily with ducky dad?
        assert!(tree.item_exists("ducky_dad/duck"));
        assert_eq!(*tree.file("ducky_dad/duck").unwrap(), 2);

        // let's rename duck
        tree.move_item("ducky_dad/duck", "ducky_dad/ducky_child")
            .unwrap();
        assert!(tree.item_exists("ducky_dad/ducky_child"));
        assert!(!tree.item_exists("ducky_dad/duck"));
        assert_eq!(*tree.file("ducky_dad/ducky_child").unwrap(), 2);

        // alrighty now mum and kid move away
        tree.add_folder("grandma").unwrap();
        tree.add_folder("grandma/mum").unwrap();
        tree.add_file("grandma/mum/kid", 2).unwrap();

        tree.add_folder("cool_place").unwrap();
        tree.move_item("grandma/mum", "cool_place/mum").unwrap();

        assert!(tree.item_exists("cool_place/mum"));
        assert!(tree.item_exists("cool_place/mum/kid"));
        assert!(!tree.item_exists("grandma/mum/kid"));

        // should have a, b, duck, ducky_dad, grandma, mum, kid, cool_place
        // assert_eq!(tree.leaves.len(), 8);
        // should have a, b, ducky_dad, grandma, cool_place
        assert_eq!(tree.children.len(), 5);

        tree.clear();

        tree.add_folder("a").unwrap();
        tree.add_folder("a/b").unwrap();
        tree.add_folder("a/b/c").unwrap();

        assert_eq!(
            tree.move_item("a/b", "a/b/c/bad").unwrap_err(),
            CosmicPathErr::BadPath
        );
    }

    #[test]
    fn iter() {
        let mut tree = LeafTree::new();
        tree.add_folder("a").unwrap();
        tree.add_folder("a/b").unwrap();
        tree.add_file("a/b/c", 0).unwrap();
        tree.add_folder("a/b/d").unwrap();
        tree.add_file("a/b/d/e", 0).unwrap();
        tree.add_file("a/b/f", 1).unwrap();
        tree.add_file("a/g", 2).unwrap();
        tree.add_file("a/h", 3).unwrap();
        tree.add_file("i", 4).unwrap();

        let values: Vec<_> = tree.files().copied().collect();
        assert_eq!(values, [0, 0, 1, 2, 3, 4]);
    }

    #[test]
    fn iter_mut() {
        let mut tree = LeafTree::new();
        tree.add_folder("a").unwrap();
        tree.add_folder("a/b").unwrap();
        tree.add_file("a/b/c", 0).unwrap();
        tree.add_folder("a/b/d").unwrap();
        tree.add_file("a/b/d/e", 0).unwrap();
        tree.add_file("a/b/f", 1).unwrap();
        tree.add_file("a/g", 2).unwrap();
        tree.add_file("a/h", 3).unwrap();
        tree.add_file("i", 4).unwrap();

        {
            let mut iterator = tree.files_mut();

            assert_eq!(*iterator.next().unwrap(), 0);
            assert_eq!(*iterator.next().unwrap(), 0);
            assert_eq!(*iterator.next().unwrap(), 1);
            assert_eq!(*iterator.next().unwrap(), 2);
            assert_eq!(*iterator.next().unwrap(), 3);
            assert_eq!(*iterator.next().unwrap(), 4);
            assert_eq!(iterator.next(), None);
        }

        {
            let mut iterator = tree.iter();

            // assert_eq!(iterator.next().unwrap().0, "");
            assert_eq!(iterator.next().unwrap().0, "a");
            assert_eq!(iterator.next().unwrap().0, "a/b");
            assert_eq!(iterator.next().unwrap().0, "a/b/c");
            assert_eq!(iterator.next().unwrap().0, "a/b/d");
            assert_eq!(iterator.next().unwrap().0, "a/b/d/e");
            assert_eq!(iterator.next().unwrap().0, "a/b/f");
            assert_eq!(iterator.next().unwrap().0, "a/g");
            assert_eq!(iterator.next().unwrap().0, "a/h");
            assert_eq!(iterator.next().unwrap().0, "i");
            assert_eq!(iterator.next(), None);
        }
    }
}
