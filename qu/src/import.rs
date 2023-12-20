
use std::alloc::Layout;
use std::fmt::Debug;

use crate::QuMsg;
use crate::Register;
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
use crate::vm::RegId;


pub struct ArgsAPI<'a> {
	pub(crate) vm: &'a mut QuVm,
	pub(crate) fn_id: FunctionId,
	pub(crate) arg_ids: &'a [RegId],
	pub(crate) out_id: RegId,
} impl<'a> ArgsAPI<'a> {
	/// Gets a reference to the value of the function argument at `index`.
	pub fn get<T: Register + 'static>(
		&self,
		index:usize,
	) -> Result<&T, QuMsg> {
		let fn_data = self.vm.definitions.get_function(self.fn_id)?;
		let value_type_id = self.vm.definitions.class_id::<T>().unwrap();
		let fn_param_id = fn_data.identity.parameters[index];
		if fn_param_id != value_type_id {
			panic!(
				"Return value of type {} does not match function's parameter[{}] type {}",
				self.vm.definitions.get_class(value_type_id).unwrap().common.name,
				index,
				self.vm.definitions.get_class(fn_param_id).unwrap().common.name,
			)
		}
		self.vm.read::<T>(self.arg_ids[index])
	}

	/// Sets the return value of the function to `value`.
	pub fn set<T: Register + 'static>(
		&mut self,
		value:T,
	) {
		let fn_data = self.vm.definitions.get_function(self.fn_id).unwrap();
		let value_type_id = self.vm.definitions.class_id::<T>().unwrap();
		let fn_return_id = fn_data.identity.return_type;
		if fn_return_id != value_type_id {
			panic!(
				"Return value of type {} does not match function's return type {}",
				self.vm.definitions.get_class(value_type_id).unwrap().common.name,
				self.vm.definitions.get_class(fn_return_id).unwrap().common.name,
			)
		}
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
		_args: impl Into<Box<[ClassId]>>,
		_out: ClassId,
		_body: &'static ExternalFunctionPointer,
	) -> Result<(), QuMsg> {
		panic!("Functions can't be added at this level. Add a module first.")
	}

	fn add_class_static_function(
		&mut self,
		_for_class: ClassId,
		_name: impl Into<String>,
		_args: impl Into<Box<[ClassId]>>,
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

	pub(crate) fn is(self, other:ClassId, d:&Definitions) -> bool{
		if self == other {
			return true;
		}
		d
			.get_class(self)
			.unwrap()
			.common.implementations
			.contains_key(&other)
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

#[derive(Clone, Copy, Debug, Default, Hash, Eq, Ord, PartialEq, PartialOrd)]
pub struct FunctionId(pub usize);
impl FunctionId {
	pub fn new(index:usize) -> Self {
		Self(index)
	}
} impl From<usize> for FunctionId {
	fn from(index: usize) -> Self {
		Self(index)
	}
} impl From<FunctionId> for usize {
	fn from(v:FunctionId) -> Self {
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
		args: impl Into<Box<[ClassId]>>,
		out: ClassId,
		body: &'static ExternalFunctionPointer,
	) -> Result<(), QuMsg> {
		let module_id = match self.get_layer_item_id() {
			ItemId::Module(id) => {id},
			_ => todo!("Support adding functions to more types items"),
		};

		let args = args.into();

		self.get_definitions_mut().define_function_in_item(
			ItemId::Module(module_id),
			FunctionMetadata {
				identity: FunctionIdentity {
					name: name.into(),
					parameters: args,
					return_type: out,
				},
				code_block: FunctionReference::External(body),
				..Default::default()
				
			},
			None,
			true,
		)?;
		Ok(())
	}

	/// Adds a function in the layer.
	fn add_function_to_class(
		&mut self,
		class: ClassId,
		name: impl Into<String>,
		args: impl Into<Box<[ClassId]>>,
		out: ClassId,
		body: &'static ExternalFunctionPointer,
	) -> Result<(), QuMsg> {
		let module_id = match self.get_layer_item_id() {
			ItemId::Module(id) => {id},
			_ => todo!("Support adding functions to more types items"),
		};

		let args = args.into();

		if args.len() == 0 {
			panic!("Can't add function to class without binding a 'self' argument. TODO: better msg")
		}
		if !args[0].is(class, self.get_definitions()) {
			panic!(
				"First argument's type, {}, does not match class's type, {}.",
				self.get_definitions().get_class(args[0])?.common.name,
				self.get_definitions().get_class(class)?.common.name,
			)
		}

		self.get_definitions_mut().define_function_in_item(
			ItemId::Module(module_id),
			FunctionMetadata {
				identity: FunctionIdentity {
					name: name.into(),
					parameters: args,
					return_type: out,
				},
				code_block: FunctionReference::External(body),
				..Default::default()
				
			},
			Some(class),
			true,
		)?;
		Ok(())
	}

	/// Adds a static function to a class.
	fn add_class_static_function(
		&mut self,
		for_class: ClassId,
		name: impl Into<String>,
		args: impl Into<Box<[ClassId]>>,
		out: ClassId,
		body: &'static ExternalFunctionPointer,
	) -> Result<(), QuMsg> {
		let args = args.into();
		self.get_definitions_mut().define_function_in_item(
			ItemId::Class(for_class),
			FunctionMetadata {
				identity: FunctionIdentity {
					name: name.into(),
					parameters: args,
					return_type: out,
				},
				code_block: FunctionReference::External(body),
				..Default::default()
			},
			None,
			false,
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
		let id = T::id();
		if !self.get_definitions().classes.contains_key(&id) {return None}
		Some(id)
	}

	/// Gets a module by name.
	fn get_module(&self, name:&str) -> Result<&ModuleMetadata, QuMsg> {
		self.get_definitions().get_module_by_name(name)
	}


	/// Implements a trait in a class.
	fn implement(&mut self, trait_id:ClassId, in_class_id:ClassId) -> Result<(), QuMsg> {
		self.get_definitions_mut().impl_trait_in_item(trait_id, in_class_id)?;
		Ok(())
	}


	/// Implements a trait's function in the given class.
	fn implement_function(
		&mut self,
		trait_id:ClassId,
		in_class_id:ClassId,
		name: impl Into<String>,
		args: impl Into<Box<[ClassId]>>,
		out: ClassId,
		body: &'static ExternalFunctionPointer,
	) -> Result<(), QuMsg> {
		let args = args.into();
		let layer_id = self.get_layer_item_id();
		self.get_definitions_mut().define_function_implementation(
			in_class_id,
			trait_id,
			layer_id,
			FunctionMetadata {
				identity: FunctionIdentity {
					name: name.into(),
					parameters: args,
					return_type: out,
				},
				code_block: FunctionReference::External(body),
				..Default::default()
			},
		)?;
		Ok(())
	}

}