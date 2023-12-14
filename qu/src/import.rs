
use std::alloc::Layout;
use std::collections::HashMap;
use std::fmt::Debug;
use std::mem::size_of;

use crate::ExternalFunction;
use crate::QuMsg;
use crate::Register;
use crate::QuStackId;
use crate::QuVm;
use crate::Uuid;
use crate::compiler::ConstantId;
use crate::compiler::Definitions;
use crate::compiler::ExternalFunctionId;
use crate::compiler::FunctionGroupId;
use crate::compiler::FunctionId;
use crate::compiler::FunctionIdentity;
use crate::compiler::ItemId;
use crate::compiler::ModuleId;
use crate::compiler::ModuleMetadata;
use crate::compiler::SomeFunctionId;


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


#[derive(Clone, Debug, Default)]
pub struct QuRegistered {
	pub fns:Vec<ExternalFunction>,
	pub structs:Vec<QuStruct>,
	pub structs_map:HashMap<String, ClassId>,
} impl QuRegistered {
	/// Constructs a new [`QuImports`].
	pub fn new() -> Self {
		Self {
			fns: Vec::default(),
			structs: Vec::default(),
			structs_map: HashMap::default(),
		}
	}
	

	pub fn get_fn_data_by_id(
		&self,
		fn_id:FunctionId
	) -> Result<&ExternalFunction, QuMsg>{
		match self.fns.get(fn_id) {
			Some(v) => Ok(v),
			None => Err(format!(
				"No external function by id `{:?}` is registered.",
				fn_id
			).into()),
		}
	}


	pub fn get_struct<T:Register>(
		&self,
	) -> Result<&QuStruct, QuMsg>{
		self.get_struct_by_str(<T as Register>::name())
	}


	pub fn get_struct_by_str(&self, name:&str
	) -> Result<&QuStruct, QuMsg>{
		Ok(self.get_struct_by_id(self.get_struct_id_by_str(name)?)?)
	}


	pub fn get_struct_by_id(&self, id:ClassId
	) -> Result<&QuStruct, QuMsg>{
		match self.structs.get(id.0) {
			Some(v) => Ok(v),
			None => Err(format!(
				"No external struct by id `{:?}` is registered.",
				id
			).into()),
		}
	}


	pub fn get_struct_by_id_mut(&mut self, id:ClassId
	) -> Result<&mut QuStruct, QuMsg>{
		match self.structs.get_mut(id.0) {
			Some(v) => Ok(v),
			None => Err(format!(
				"No external struct by id `{:?}` is registered.",
				id
			).into()),
		}
	}


	pub fn get_struct_id<T:Register>(
		&self
	) -> Result<ClassId, QuMsg> {
		self.get_struct_id_by_str(<T as Register>::name())
	}


	pub fn get_struct_id_by_str(&self, name:&str
	) -> Result<ClassId, QuMsg>{
		let Some(d) = self.structs_map.get(name)
			else {
				return Err(format!(
					"No external struct by name `{name}` is registered."
				).into())
			};
		Ok(*d)
	}


	/// Registers an external struct to be used within the Qu langauge.
	pub fn register_struct<S:Register+'static>(&mut self) {
		let r_struct = QuStruct::new(
			<S as Register>::name(),
			size_of::<S>(),
		);
		
		self.structs_map.insert(
			<S as Register>::name().to_owned(),
			ClassId::new(self.structs.len()),
		);
		self.structs.push(r_struct);

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


#[derive(Clone)]
pub struct QuStruct {
	pub constants_map: HashMap<String, ConstantId>,
	pub external_functions_map: HashMap<FunctionIdentity, ExternalFunctionId>,
	pub functions_map: HashMap<FunctionIdentity, FunctionId>,
	pub function_groups_map: HashMap<String, FunctionGroupId>,
	pub static_functions_map: HashMap<FunctionIdentity, SomeFunctionId>,
	
	pub name: String,
	/// The size of the struct in bytes.
	pub size: u8,

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
		Self {
			name,
			size: aligned_size as u8,
			constants_map: Default::default(),
			external_functions_map: Default::default(),
			functions_map: Default::default(),
			function_groups_map: Default::default(),
			static_functions_map: Default::default(),
		}
	}

	pub fn has_item(&self, identity: &str) -> bool {
		self.constants_map.contains_key(identity)
			|| self.function_groups_map.contains_key(identity)
	}


	pub fn get_item_id(&self, identity: &str) -> Result<ItemId, QuMsg> {
		if let Some(id) = self.constants_map.get(identity) {
			return Ok(ItemId::Constant(*id));
		}
		if let Some(id) = self.function_groups_map.get(identity) {
			return Ok(ItemId::FunctionGroup(*id));
		}

		Err(format!(
			"Class '{}' has no item by name '{}'.", self.name, identity,
		).into())
	}


	pub fn get_function_group_id(
		&self,
		function_name: &str,
	) -> Result<FunctionGroupId, QuMsg> {
		self.function_groups_map
			.get(function_name)
			.ok_or_else(||->QuMsg {
				format!(
					"Class '{}' has no function named '{}'",
					self.name,
					function_name
				).into()
			})
			.map(|id| {*id})
	}

	pub fn get_static_function_id(
		&self,
		identity: &FunctionIdentity,
	) -> Result<SomeFunctionId, QuMsg> {
		let id = self.static_functions_map
			.get(identity)
			.ok_or_else(||->QuMsg {
				format!(
					"Class has no static function with identity '{identity}'.",
				).into()
			})?;
		Ok(*id)
	}
} impl Debug for QuStruct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("QuStruct")
			.field("name", &self.name)
			.finish()
    }
} impl Default for QuStruct {
    fn default() -> Self {
        Self {
			constants_map: Default::default(),
			external_functions_map: Default::default(),
			functions_map: Default::default(),
			name: Default::default(),
			size: Default::default(),
			function_groups_map: Default::default(),
			static_functions_map: Default::default(),
		}
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
			ExternalFunction {
				name: name.into(),
				pointer: body,
				parameters: Vec::from(args),
				return_type: out,
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
			ExternalFunction {
				name: name.into(),
				pointer: body,
				parameters: Vec::from(args),
				return_type: out,
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

}