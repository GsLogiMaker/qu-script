
use std::collections::HashMap;
use std::fmt::Debug;
use std::fmt::format;
use std::mem::size_of;
use std::slice::SliceIndex;

use crate::ExternalFunction;
use crate::ExternalFunctionDefinition;
use crate::Module;
use crate::QuMsg;
use crate::QuRegisterStruct;
use crate::QuStackId;
use crate::QuVm;
use crate::QuVoid;
use crate::Vector2;
use crate::compiler::ConstantId;
use crate::compiler::Definitions;
use crate::compiler::ExternalFunctionId;
use crate::compiler::FunctionGroupId;
use crate::compiler::FunctionId;
use crate::compiler::FunctionIdentity;
use crate::compiler::ItemId;
use crate::compiler::FunctionGroup;
use crate::compiler::ModuleId;
use crate::compiler::ModuleMetadata;
use crate::compiler::SomeFunctionId;


pub struct ArgsAPI<'a> {
	pub(crate) vm: &'a mut QuVm,
	pub(crate) arg_ids: &'a [QuStackId],
	pub(crate) out_id: QuStackId,
} impl<'a> ArgsAPI<'a> {
	/// Gets a reference to the value of the function argument at `index`.
	pub fn get<T:QuRegisterStruct>(&self, index:usize) -> Result<&T, QuMsg> {
		self.vm.read::<T>(self.arg_ids[index])
	}

	/// Sets the return value of the function to `value`.
	pub fn set<T:QuRegisterStruct>(&mut self, value:T) {
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
} impl<'a> ModuleBuilder<'a> {
	/// Define a class in this module.
	pub fn add_class<T:QuRegisterStruct+'static>(
		&mut self
	) -> Result<ClassId, QuMsg> {
		self.definitions.register_module_struct::<T>(self.module_id)
	}

	/// Define a function in this module.
	pub fn add_function(
		&mut self,
		name: String,
		args: &[ClassId],
		out: ClassId,
		body: &'static ExternalFunctionPointer,
	) -> Result<(), QuMsg> {
		self.definitions.register_module_function(self.module_id, ExternalFunctionDefinition {
			name: name,
			pointer: body,
			parameters: Vec::from(args),
			return_type: out,
		})?;
		self.definitions.register_functions()
	}

	pub fn get_module(&self, name:&str) -> Result<&ModuleMetadata, QuMsg> {
		self.definitions.get_module_by_name(name)
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
	

	pub fn get_fn_data_by_id(&self, fn_id:FunctionId
	) -> Result<&ExternalFunction, QuMsg>{
		match self.fns.get(fn_id) {
			Some(v) => Ok(v),
			None => Err(format!(
				"No external function by id `{:?}` is registered.",
				fn_id
			).into()),
		}
	}


	pub fn get_struct<T:QuRegisterStruct>(
		&self,
	) -> Result<&QuStruct, QuMsg>{
		self.get_struct_by_str(<T as QuRegisterStruct>::name())
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


	pub fn get_struct_id<T:QuRegisterStruct>(
		&self
	) -> Result<ClassId, QuMsg> {
		self.get_struct_id_by_str(<T as QuRegisterStruct>::name())
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
	pub fn register_struct<S:QuRegisterStruct+'static>(&mut self) {
		let r_struct = QuStruct::new(
			<S as QuRegisterStruct>::name(),
			&<S as QuRegisterStruct>::register_fns,
			size_of::<S>(),
		);
		
		self.structs_map.insert(
			<S as QuRegisterStruct>::name().to_owned(),
			ClassId::new(self.structs.len()),
		);
		self.structs.push(r_struct);

	}
}


pub type ModuleBody = dyn Fn(&mut ModuleBuilder) -> Result<(), QuMsg>;
pub struct Registerer<'a> {
	pub(crate) definitions: &'a mut Definitions,
} impl<'a> Registerer<'a> {
	pub fn add_module(
		&mut self, name:String,
		body:&ModuleBody
	) -> Result<ModuleId, QuMsg> {
		self.definitions.define_module(name, body)
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
	
	pub name: String,
	pub register_fn: &'static dyn Fn(&mut Definitions) -> Vec<ExternalFunctionDefinition>,
	/// The size of the struct in bytes.
	pub size: u8,

} impl QuStruct {
	pub fn new(
		name:impl Into<String>,
		fn_registerer:&'static dyn Fn(&mut Definitions) -> Vec<ExternalFunctionDefinition>,
		size:usize,
	) -> Self {
		let name = name.into();
		assert!(size < u8::MAX as usize);
		Self {
			name,
			register_fn: fn_registerer,
			size: size as u8,
			constants_map: Default::default(),
			external_functions_map: Default::default(),
			functions_map: Default::default(),
			function_groups_map: Default::default(),
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
			register_fn: &|definitions:&mut Definitions| {vec![]},
			size: Default::default(),
			function_groups_map: Default::default(),
		}
    }
}