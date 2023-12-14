
use std::alloc::Layout;
use std::fmt::Debug;

use crate::QuMsg;
use crate::Register;
use crate::QuStackId;
use crate::QuVm;
use crate::Uuid;
use crate::compiler::CommonItem;
use crate::compiler::ConstantId;
use crate::compiler::Definitions;
use crate::compiler::FunctionGroupId;
use crate::compiler::FunctionIdentity;
use crate::compiler::FunctionMetadata;
use crate::compiler::FunctionReference;
use crate::compiler::ItemId;
use crate::compiler::ModuleId;
use crate::compiler::ModuleMetadata;


pub struct ArgsAPI<'a> {
	pub(crate) vm: &'a mut QuVm,
	pub(crate) arg_ids: &'a [QuStackId],
	pub(crate) out_id: QuStackId,
} impl<'a> ArgsAPI<'a> {
	/// Gets a reference to the value of the function argument at `index`.
	pub fn get<T: Register + 'static>(
		&self,
		index:usize,
	) -> Result<&T, QuMsg> {
		self.vm.read::<T>(self.arg_ids[index])
	}

	/// Sets the return value of the function to `value`.
	pub fn set<T: Register + 'static>(
		&mut self,
		value:T,
	) {
		self.vm.write::<T>(self.out_id, value);
	}

	pub fn set_hold(&mut self, value: bool) {
		self.vm.hold_is_true = value;
	}
}

pub type ExternalFunctionPointer = dyn Fn(&mut ArgsAPI) -> Result<(), QuMsg>;

pub struct ModuleBuilder<'a> {
	pub(crate) definitions: &'a mut Definitions,
	pub(crate) module_id: ModuleId,
} impl<'a> RegistererLayer for ModuleBuilder<'a> {
    fn get_layer_item_id(&self) -> ItemId {
        ItemId::Module(self.module_id)
    }

    fn get_definitions(&self) -> &Definitions {
        &self.definitions
    }

    fn get_definitions_mut(&mut self) -> &mut Definitions {
        &mut self.definitions
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
/// An ID for an external function.
pub struct QuExtFnId(pub usize);
impl QuExtFnId {

	/// Constructs a new [`QuExtFnId`].
	pub fn new(index:usize) -> Self {
		Self(index)
	}

} impl From<usize> for QuExtFnId {

	fn from(v:usize) -> Self {
		QuExtFnId(v)
	}

}


impl From<QuExtFnId> for u8 {
	fn from(v:QuExtFnId) -> Self {
		v.0 as u8
	}
}


pub type ModuleBody = dyn Fn(&mut ModuleBuilder) -> Result<(), QuMsg>;
pub struct Registerer<'a> {
	pub(crate) definitions: &'a mut Definitions,
} impl<'a> RegistererLayer for Registerer<'a> {
    fn get_layer_item_id(&self) -> ItemId {
        ItemId::None
    }

    fn get_definitions(&self) -> &Definitions {
        &self.definitions
    }

    fn get_definitions_mut(&mut self) -> &mut Definitions {
        &mut self.definitions
    }

	fn add_class<T:Register+'static>(
		&mut self
	) -> Result<ClassId, QuMsg> {
		panic!("Classes can't be added at this level. Add a module first.")
	}

	fn add_constant<T: Register + 'static>(
		&mut self,
		_name: impl Into<String>,
		_value: T,
	) -> Result<ConstantId, QuMsg> {
		panic!("Constants can't be added at this level. Add a module first.")
	}

	fn add_function(
		&mut self,
		_name: impl Into<String>,
		_args: &[ClassId],
		_out: ClassId,
		_body: &'static ExternalFunctionPointer,
	) -> Result<(), QuMsg> {
		panic!("Functions can't be added at this level. Add a module first.")
	}

	fn add_class_static_function(
		&mut self,
		_for_class: ClassId,
		_name: impl Into<String>,
		_args: &[ClassId],
		_out: ClassId,
		_body: &'static ExternalFunctionPointer,
	) -> Result<(), QuMsg> {
		panic!("Static functions can't be added at this level. Add a module first.")
	}
}

#[derive(Clone, Copy, Debug, Default, Hash, Eq, Ord, PartialEq, PartialOrd)]
pub struct ClassId(pub usize);
impl ClassId {
	pub fn new(index:usize) -> Self {
		Self(index)
	}
} impl From<usize> for ClassId {
	fn from(index: usize) -> Self {
		Self(index)
	}
}
impl From<ClassId> for u8 {
	fn from(v:ClassId) -> Self {
		v.0 as u8
	}
} impl From<ClassId> for usize {
	fn from(v:ClassId) -> Self {
		v.0 as usize
	}
}


#[derive(Debug, Default, Clone)]
pub struct QuStruct {
	/// The size of the struct in bytes.
	pub size: u8,
	pub(crate) is_trait: bool,
	pub common: CommonItem,

} impl QuStruct {
	pub fn new(
		name:impl Into<String>,
		size:usize,
	) -> Self {
		let name = name.into();
		assert!(size < u8::MAX as usize);
		let aligned_size = if size != 0 {
			Layout::from_size_align(size, 4)
				.unwrap()
				.pad_to_align()
				.size()
		} else { 0 };

		let mut common = CommonItem::default();
		common.name = name;

		Self {
			common,
			size: aligned_size as u8,
			..Default::default()
		}
	}

	pub fn has_item(&self, identity: &str) -> bool {
		self.common.has_item(identity)
	}


	pub fn get_item_id(&self, identity: &str) -> Result<ItemId, QuMsg> {
		self.common.get_item_id(identity)
	}


	pub fn get_function_group_id(
		&self,
		function_name: &str,
	) -> Result<FunctionGroupId, QuMsg> {
		self.common.get_function_group_id(function_name)
	}
}


/// A builder layer for adding external items into Qu.
/// 
/// A layer could be something like a module class.
pub trait RegistererLayer {
	/// Returns the [`ItemId`] of the current layer.
	fn get_layer_item_id(&self) -> ItemId;
	/// Returns a reference to all defined items.
	fn get_definitions(&self) -> &Definitions;
	/// Returns a mutable reference to all defined items.
	fn get_definitions_mut(&mut self) -> &mut Definitions;
	
	/// Returns the [`Uuid`] of the Qu instance.
	fn get_uuid(&self) -> &Uuid {
		&self.get_definitions().uuid
	}

	/// Adds a class to the current layer.
	fn add_class<T:Register+'static>(
		&mut self
	) -> Result<ClassId, QuMsg> {
		let module_id = match self.get_layer_item_id() {
			ItemId::Module(id) => {id},
			_ => todo!("Support adding classes to more types items"),
		};

		self.get_definitions_mut().register_module_struct::<T>(module_id)
	}

	/// Adds a constant to the current layer.
	fn add_constant<T: Register + 'static>(
		&mut self,
		name: impl Into<String>,
		value: T,
	) -> Result<ConstantId, QuMsg> {
		let layer_id = self.get_layer_item_id();
		self.get_definitions_mut().define_constant_in_item(
			name.into(),
			value,
			layer_id,
		)
	}

	/// Adds a function in the layer.
	fn add_function(
		&mut self,
		name: impl Into<String>,
		args: &[ClassId],
		out: ClassId,
		body: &'static ExternalFunctionPointer,
	) -> Result<(), QuMsg> {
		let module_id = match self.get_layer_item_id() {
			ItemId::Module(id) => {id},
			_ => todo!("Support adding functions to more types items"),
		};

		self.get_definitions_mut().register_function_in_module(
			module_id,
			FunctionMetadata {
				identity: FunctionIdentity {
					name: name.into(),
					parameters: Box::from(args),
					return_type: out,
				},
				code_block: FunctionReference::External(body),
				
			}
		)?;
		Ok(())
	}

	/// Adds a static function to a class.
	fn add_class_static_function(
		&mut self,
		for_class: ClassId,
		name: impl Into<String>,
		args: &[ClassId],
		out: ClassId,
		body: &'static ExternalFunctionPointer,
	) -> Result<(), QuMsg> {
		self.get_definitions_mut().register_static_function_in_class(
			for_class,
			FunctionMetadata {
				identity: FunctionIdentity {
					name: name.into(),
					parameters: Box::from(args),
					return_type: out,
				},
				code_block: FunctionReference::External(body),
				
			},
		)?;
		Ok(())
	}

	/// Adds a module in the layer.
	fn add_module(
		&mut self, name:impl Into<String>,
		body:&ModuleBody
	) -> Result<ModuleId, QuMsg> {
		match self.get_layer_item_id() {
			ItemId::None => {/* Ok */},
			_ => todo!("Support adding modules to more types items"),
		}

		self.get_definitions_mut().define_module(name.into(), body)
	}


	/// Adds a class to the current layer.
	fn add_trait<T: Register + 'static>(
		&mut self
	) -> Result<ClassId, QuMsg> {
		let module_id = match self.get_layer_item_id() {
			ItemId::Module(id) => {id},
			_ => todo!("Support adding traits to more types items"),
		};

		let id = self.get_definitions_mut()
			.register_module_struct::<T>(module_id)?;
		self.get_definitions_mut().get_class_mut(id)?.is_trait = true;
		Ok(id)
	}


	/// Returns the [`ClassId`] of the given struct.
	fn get_class_id_of<T: Register + 'static>(
		&self
	) -> Option<ClassId> {
		T::get_id(&self.get_definitions().uuid)
	}

	/// Gets a module by name.
	fn get_module(&self, name:&str) -> Result<&ModuleMetadata, QuMsg> {
		self.get_definitions().get_module_by_name(name)
	}


	/// Implements a trait in a class.
	fn implement(&mut self, trait_id:ClassId, in_class_id:ClassId) -> Result<(), QuMsg> {
		self.get_definitions_mut().implement(trait_id, in_class_id)?;
		Ok(())
	}

}