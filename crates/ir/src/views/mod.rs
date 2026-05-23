mod function;
use crate::{Component, Function, IRPointer, Label, SlynxIR};

pub trait IRStorage<T> {
    fn get_storage(&self) -> &[T];
    fn get_storage_mut(&mut self) -> &mut [T];
    fn get(&self, ptr: IRPointer<T, 1>) -> &T {
        &self.get_storage()[ptr.ptr()]
    }
    fn get_mut(&mut self, ptr: IRPointer<T, 1>) -> &mut T {
        &mut self.get_storage_mut()[ptr.ptr()]
    }
    fn get_ranged(&self, ptr: IRPointer<T, 0>) -> &[T] {
        &self.get_storage()[ptr.range()]
    }
    fn get_ranged_mut(&mut self, ptr: IRPointer<T, 0>) -> &mut [T] {
        &mut self.get_storage_mut()[ptr.range()]
    }
}

pub struct IRViewer<'a, T> {
    ptr: IRPointer<T, 1>,
    ir: &'a SlynxIR,
}

macro_rules! impl_storage {
    ($storage_type: ty, $storage_name: ident) => {
        paste::paste! {
            impl IRStorage<$storage_type> for SlynxIR {
                fn get_storage(&self) -> &[$storage_type] {
                    &self.$storage_name
                }
                fn get_storage_mut(&mut self) -> &mut [$storage_type] {
                    &mut self.$storage_name
                }
            }
        }
    };
}

impl_storage!(Function, functions);
impl_storage!(Label, labels);
impl_storage!(Component, components);
